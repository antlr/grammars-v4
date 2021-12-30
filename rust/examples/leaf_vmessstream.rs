use std::{cmp::min, io, pin::Pin};

use aes::Aes128;
use bytes::{BufMut, BytesMut};
use cfb_mode::stream_cipher::{NewStreamCipher, StreamCipher};
use cfb_mode::Cfb;
use futures::{
    ready,
    task::{Context, Poll},
};
use rand::{rngs::StdRng, Rng, SeedableRng};
use tokio::io::{AsyncRead, AsyncWrite};

use crate::common::crypto::{
    aead::{AeadDecryptor, AeadEncryptor},
    Decryptor, Encryptor,
};

use super::crypto::{PaddingLengthGenerator, ShakeSizeParser, VMessAEADSequence};
use super::protocol::ClientSession;

enum ReadState {
    WaitingResponseHeader,
    WaitingLength,
    WaitingData(usize, usize),
    PendingData(usize),
}

enum WriteState {
    WaitingChunk,
    PendingChunk(usize, (usize, usize)),
}

pub struct VMessAuthStream<T> {
    inner: T,
    sess: ClientSession,
    enc: AeadEncryptor<VMessAEADSequence>,
    enc_size_parser: ShakeSizeParser,
    dec: AeadDecryptor<VMessAEADSequence>,
    dec_size_parser: ShakeSizeParser,
    tag_len: usize,
    read_buf: BytesMut,
    write_buf: BytesMut,
    read_state: ReadState,
    write_state: WriteState,
    read_pos: usize,
}

impl<T> VMessAuthStream<T> {
    pub fn new(
        s: T,
        sess: ClientSession,
        enc: AeadEncryptor<VMessAEADSequence>,
        enc_size_parser: ShakeSizeParser,
        dec: AeadDecryptor<VMessAEADSequence>,
        dec_size_parser: ShakeSizeParser,
        tag_len: usize,
    ) -> Self {
        VMessAuthStream {
            inner: s,
            sess,
            enc,
            enc_size_parser,
            dec,
            dec_size_parser,
            tag_len,

            // never depend on these sizes, reserve when need
            read_buf: BytesMut::with_capacity(0x2 + 0x4000),
            write_buf: BytesMut::with_capacity(0x2 + 0x4000),

            read_state: ReadState::WaitingResponseHeader,
            write_state: WriteState::WaitingChunk,
            read_pos: 0,
        }
    }
}

trait ReadExt {
    fn poll_read_exact(&mut self, cx: &mut Context, size: usize) -> Poll<io::Result<()>>;
}

impl<T: AsyncRead + Unpin> ReadExt for VMessAuthStream<T> {
    fn poll_read_exact(&mut self, cx: &mut Context, size: usize) -> Poll<io::Result<()>> {
        self.read_buf.reserve(size);
        unsafe { self.read_buf.set_len(size) };
        loop {
            if self.read_pos < size {
                let n =
                    ready!(Pin::new(&mut self.inner)
                        .poll_read(cx, &mut self.read_buf[self.read_pos..]))?;
                self.read_pos += n;
                if n == 0 {
                    return Err(eof()).into();
                }
            }
            if self.read_pos >= size {
                self.read_pos = 0;
                return Poll::Ready(Ok(()));
            }
        }
    }
}

fn eof() -> io::Error {
    io::Error::new(io::ErrorKind::UnexpectedEof, "early eof")
}

fn crypto_err() -> io::Error {
    io::Error::new(io::ErrorKind::Other, "crypto error")
}

impl<T: AsyncRead + Unpin> AsyncRead for VMessAuthStream<T> {
    fn poll_read(
        mut self: Pin<&mut Self>,
        cx: &mut Context,
        buf: &mut [u8],
    ) -> Poll<io::Result<usize>> {
        loop {
            match self.read_state {
                ReadState::WaitingResponseHeader => {
                    let me = &mut *self;
                    ready!(me.poll_read_exact(cx, 4))?;
                    let mut enc = Cfb::<Aes128>::new_var(
                        &me.sess.response_body_key,
                        &me.sess.response_body_iv,
                    )
                    .map_err(|_| io::Error::new(io::ErrorKind::Other, "crypto error"))?;
                    enc.decrypt(&mut me.read_buf[..4]);

                    if me.read_buf[0] != me.sess.response_header {
                        return Poll::Ready(Err(crypto_err()));
                    }

                    // ready to read data chunks
                    me.read_state = ReadState::WaitingLength;
                }
                ReadState::WaitingLength => {
                    // read and decode payload length
                    let me = &mut *self;
                    let size_bytes = me.dec_size_parser.size_bytes();
                    ready!(me.poll_read_exact(cx, size_bytes))?;
                    let padding_size = me.dec_size_parser.next_padding_len() as usize;
                    let size = me.dec_size_parser.decode(&me.read_buf[..size_bytes]) as usize;

                    // ready to read payload
                    me.read_state = ReadState::WaitingData(size, padding_size);
                }
                ReadState::WaitingData(size, padding_size) => {
                    // read and decipher payload
                    let me = &mut *self;
                    ready!(me.poll_read_exact(cx, size))?;
                    let encrypted_size = size - padding_size;
                    let _ = me.read_buf.split_off(encrypted_size); // trim padding
                    me.dec.decrypt(&mut me.read_buf).map_err(|_| crypto_err())?;

                    // ready to read plaintext payload into buf
                    me.read_state = ReadState::PendingData(encrypted_size - me.tag_len);
                }
                ReadState::PendingData(n) => {
                    let to_read = min(buf.len(), n);
                    let payload = self.read_buf.split_to(to_read);
                    (&mut buf[..to_read]).copy_from_slice(&payload);
                    if to_read < n {
                        // there're unread data, continues in next poll
                        self.read_state = ReadState::PendingData(n - to_read);
                    } else {
                        // all data consumed, ready to read next chunk
                        self.read_state = ReadState::WaitingLength;
                    }

                    return Poll::Ready(Ok(to_read));
                }
            }
        }
    }
}

impl<T: AsyncWrite + Unpin> AsyncWrite for VMessAuthStream<T> {
    fn poll_write(
        mut self: Pin<&mut Self>,
        cx: &mut Context,
        buf: &[u8],
    ) -> Poll<io::Result<usize>> {
        loop {
            match self.write_state {
                WriteState::WaitingChunk => {
                    let me = &mut *self;
                    let padding_size = me.enc_size_parser.next_padding_len() as usize;
                    let max_payload_size = 0x4000 - me.tag_len - padding_size;
                    let consume_len = min(buf.len(), max_payload_size);
                    let payload_len = consume_len + me.tag_len + padding_size;

                    // encode size
                    let size_bytes = me.enc_size_parser.size_bytes();
                    me.write_buf.resize(size_bytes, 0);
                    me.enc_size_parser
                        .encode(payload_len as u16, &mut me.write_buf);

                    let mut piece2 = me.write_buf.split_off(size_bytes);

                    // seal payload
                    piece2.reserve(consume_len + me.tag_len);
                    piece2.put_slice(&buf[..consume_len]);
                    me.enc.encrypt(&mut piece2).map_err(|_| crypto_err())?;

                    let mut piece3 = piece2.split_off(consume_len + me.tag_len);

                    // add random paddings
                    if padding_size > 0 {
                        piece3.resize(padding_size, 0);
                        let mut rng = StdRng::from_entropy();
                        for i in 0..piece3.len() {
                            piece3[i] = rng.gen();
                        }
                    }

                    piece2.unsplit(piece3);
                    me.write_buf.unsplit(piece2);

                    // ready to write data
                    self.write_state =
                        WriteState::PendingChunk(consume_len, (me.write_buf.len(), 0));
                }

                // consumed is the consumed plaintext length we're going to return to caller.
                // total is total length of the ciphertext data chunk we're going to write to remote.
                // written is the number of ciphertext bytes were written.
                WriteState::PendingChunk(consumed, (total, written)) => {
                    let me = &mut *self;

                    // There would be trouble if the caller change the buf upon pending, but I
                    // believe that's not a usual use case.
                    let nw = ready!(Pin::new(&mut me.inner).poll_write_buf(cx, &mut me.write_buf))?;
                    if nw == 0 {
                        return Err(eof()).into();
                    }

                    if written + nw >= total {
                        // data chunk written, go to next chunk
                        me.write_state = WriteState::WaitingChunk;
                        return Poll::Ready(Ok(consumed));
                    }

                    me.write_state = WriteState::PendingChunk(consumed, (total, written + nw));
                }
            }
        }
    }

    fn poll_flush(mut self: Pin<&mut Self>, cx: &mut Context) -> Poll<io::Result<()>> {
        Pin::new(&mut self.inner).poll_flush(cx)
    }

    fn poll_shutdown(mut self: Pin<&mut Self>, cx: &mut Context) -> Poll<io::Result<()>> {
        Pin::new(&mut self.inner).poll_shutdown(cx)
    }
}