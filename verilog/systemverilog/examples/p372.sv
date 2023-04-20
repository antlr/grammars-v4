module m (
    input a,
    b
);
  a1 :
  assert #0 (a == b);
endmodule
module m (
    input a,
    b
);
  always_comb begin
    a1 : assert #0 (a == b);
  end
endmodule
