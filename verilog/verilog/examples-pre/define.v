// Examples of `define text macros
`define MY_NUMBER 5
`define MY_STRING "Hello world!"
`define ADD2PLUS2 2 + 2
`define ADD5(RESULT, SOURCE) \
  RESULT = SOURCE + 5; \
  $display("Inside ADD5 macro. Scope is %m");
module test;
  reg [7:0] a, b;
  initial begin
    $display(`MY_NUMBER);
    $display(`MY_STRING);
    $display(2 + 2);
    $display(`ADD2PLUS2);
    a = 1;
`ifdef MY_FEATURE
    `ADD5(b, a)
    $display("a:%0d, b:%0d", a, b);
`else
    $display("No feature");
`endif
  end
endmodule
