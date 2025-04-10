module m;
  logic r;
  wire dw1, dw2;
  initial begin
    r = 0;
    #10 r = 1;
  end
  assign dw1 = r;
  p p_i (
      dw2,
      dw1
  );
  always @(dw2) $display("dw2 is %b", dw2);
endmodule
program p (
    output pw2,
    input  pw1
);
  assign pw2 = pw1;
endprogram
