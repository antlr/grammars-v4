module test #(
    N = 1
) (
    input  [N-1:0] in,
    output [N-1:0] out
);
  if ((N < 1) || (N > 8)) $error("Parameter N has an invalid value of %0d", N);
  assign out = in;
endmodule
