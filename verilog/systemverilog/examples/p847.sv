module mux8 (
    in1,
    in2,
    s,
    q
);
  output [7:0] q;
  input [7:0] in1, in2;
  input s;
  specify
    (in1 => q) = (3, 4);
    (in2 => q) = (2, 3);
    (s *> q) = 1;
  endspecify
endmodule
