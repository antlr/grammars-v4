module example1;
  wire a, b;
  reg c, d;
  `ifdef A
    `ifndef B
      `define B(x,y) \
        assign x = y;
    `elsif C
      `C(a,b)
    `else
      `undef A
    `endif
  `endif
  always @* a = c;
  assign b = d;
endmodule
