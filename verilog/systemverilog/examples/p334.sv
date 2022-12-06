virtual class C #(
    parameter DECODE_W,
    parameter ENCODE_W = $clog2(DECODE_W)
);
  static function logic [ENCODE_W-1:0] ENCODER_f(input logic [DECODE_W-1:0] DecodeIn);
    ENCODER_f = '0;
    for (int i = 0; i < DECODE_W; i++) begin
      if (DecodeIn[i]) begin
        ENCODER_f = i[ENCODE_W-1:0];
        break;
      end
    end
  endfunction
  static function logic [DECODE_W-1:0] DECODER_f(input logic [ENCODE_W-1:0] EncodeIn);
    DECODER_f = '0;
    DECODER_f[EncodeIn] = 1'b1;
  endfunction
endclass
