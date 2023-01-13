module DFF (
    Q,
    CLK,
    DAT
);
  input CLK;
  input [7:0] DAT;
  output [7:0] Q;
  always @(posedge clk) Q = DAT;
  specify
    $setup(DAT, posedge CLK, 10);
  endspecify
endmodule
