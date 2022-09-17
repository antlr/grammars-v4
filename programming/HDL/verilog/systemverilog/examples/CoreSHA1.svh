/*
MIT License

Copyright (c) 2021 layup

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/
class CoreSHA1 extends BaseHash #(512, 160, 160);
  protected tBlock block_reg;
  typedef struct packed {tWord h0, h1, h2, h3, h4;} sState;
  protected sState state;
  protected struct packed {tWord mw, lw;} bit_cnt;
  protected byte msg_reg[$];
  function new();
    super.new();
    this_type = HASH_SHA1;
    initState();
  endfunction
  virtual function void initState();
    this.bit_cnt   = 0;
    this.block_reg = 0;
    this.msg_reg.delete();
    state.h0 = 32'h67452301;
    state.h1 = 32'hefcdab89;
    state.h2 = 32'h98badcfe;
    state.h3 = 32'h10325476;
    state.h4 = 32'hc3d2e1f0;
  endfunction
  const static protected tWord Kt_0 = 32'h5a827999;
  const static protected tWord Kt_1 = 32'h6ed9eba1;
  const static protected tWord Kt_2 = 32'h8f1bbcdc;
  const static protected tWord Kt_3 = 32'hca62c1d6;
  virtual function sState trans(sState stin, tBlock blkin);
    tWord a, b, c, d, e, T, W[0:79];
    {a, b, c, d, e} = stin;
    for (byte i = 0; i < 16; i++) begin
      W[15-i] = blkin[31:0];
      blkin >>= 32;
      `_LOG($sformatf("W[%02d] = %08h\n", 15 - i, W[15-i]))
    end
    for (byte i = 16; i < 80; i++) begin
      W[i] = ROTL((W[i-3] ^ W[i-8] ^ W[i-14] ^ W[i-16]), 1);
    end
    for (byte t = 0; t < 80; t++) begin
      if (t < 20) begin
        T = ROTL(a, 5) + fCh(b, c, d) + e + Kt_0 + W[t];
      end else if (t < 40) begin
        T = ROTL(a, 5) + fParity(b, c, d) + e + Kt_1 + W[t];
      end else if (t < 60) begin
        T = ROTL(a, 5) + fMaj(b, c, d) + e + Kt_2 + W[t];
      end else begin
        T = ROTL(a, 5) + fParity(b, c, d) + e + Kt_3 + W[t];
      end
      {e, d, c, b, a} = {d, c, ROTL(b, 30), a, T};
      `_LOG($sformatf("[t=%02d]abcde: %08h_%08h_%08h_%08h_%08h\n", t, a, b, c, d, e))
    end
    stin.h0 += a;
    stin.h1 += b;
    stin.h2 += c;
    stin.h3 += d;
    stin.h4 += e;
    `_LOG($sformatf(
          "[%s]out: %08h_%08h_%08h_%08h_%08h\n",
          this_type.name(),
          stin.h0,
          stin.h1,
          stin.h2,
          stin.h3,
          stin.h4
          ))
    return stin;
  endfunction
  virtual function void update(byte msg[$]);
    bit_cnt += msg.size() * 8;
    msg_reg = {msg_reg, msg};
    while (msg_reg.size() >= BLOCK_SISE / 8) begin
      repeat (BLOCK_SISE / 8 - 1) begin
        block_reg[7:0] = msg_reg.pop_front();
        block_reg <<= 8;
      end
      block_reg[7:0] = msg_reg.pop_front();
      state = trans(state, block_reg);
      block_reg = 0;
    end
  endfunction
  virtual function tDigestTr getDigest();
    int pad_len;
    tWord pad_word;
    sState st_tmp;
    byte pad_msg[$];
    `_LOG($sformatf("Total Message size: %0d bit \n", bit_cnt))
    msg_reg.push_back(8'h80);
    pad_len = (msg_reg.size()< BLOCK_SISE/8 - BLOCK_SISE/64)?
(BLOCK_SISE/8 - msg_reg.size() - BLOCK_SISE/64):
(BLOCK_SISE/8 - msg_reg.size() - BLOCK_SISE/64 + BLOCK_SISE/8);
    repeat (pad_len) pad_msg.push_back(0);
    pad_word = bit_cnt.mw;
    repeat (BLOCK_SISE / 128) begin
      pad_msg.push_back(pad_word[BLOCK_SISE/16-1:BLOCK_SISE/16-8]);
      pad_word <<= 8;
    end
    pad_word = bit_cnt.lw;
    repeat (BLOCK_SISE / 128) begin
      pad_msg.push_back(pad_word[BLOCK_SISE/16-1:BLOCK_SISE/16-8]);
      pad_word <<= 8;
    end
    update(pad_msg);
    st_tmp = state;
    initState();
    `_LOG($sformatf("[%s]Message digest: %0h\n", this_type.name(), st_tmp))
    return st_tmp;
  endfunction
endclass : CoreSHA1
