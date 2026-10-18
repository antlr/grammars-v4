module byte_swap (
    inout wire [31:0] A,
    inout wire [31:0] B
);
  alias {A[7:0], A[15:8], A[23:16], A[31:24]} = B;
endmodule
module byte_rip (
    inout wire [31:0] W,
    inout wire [ 7:0] LSB,
    MSB
);
  alias W[7:0] = LSB; alias W[31:24] = MSB;
endmodule
