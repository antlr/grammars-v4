<<>>

<<"a">>

<<0xcafe>>

<<1, 2, <<3,33,333>>, "four", ?A, 0x78FF>>

<<42::8>>

<<42 :: 16>>

<<999999999999999999999 :: 1_000_000>>

<<0::1, 0::1, 1::1, 1::1>>

<<0, 1, x::binary>>

<<head::binary-size(2), rest::binary>>

<<1::size(1)>>

<<102::integer-native, rest::binary>>

<<102::native-integer, rest::binary>>

<<102::unsigned-big-integer, rest::binary>>

<<102::unsigned-big-integer-size(8), rest::binary>>

<<102::unsigned-big-integer-8, rest::binary>>

<<102::8-integer-big-unsigned, rest::binary>>

<<102, rest::binary>>

<<1024::8*4>>

<<int::integer-signed>>

<<-100>>

<<-var>>

<<-?A>>