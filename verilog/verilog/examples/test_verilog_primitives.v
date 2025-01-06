// Test Scenario to validate correct translation
// Refer: https://github.com/Nic30/hdlConvertor/issues/173
module dummy (
    c,
    a,
    b
);
  output c;
  input a, b;
  and a1 (c, a, b);
endmodule
module d2 (
    y,
    a
);
  output y;
  input a;
  not n1 (y, a);
endmodule
module example (
    a,
    b,
    c,
    d
);
  input a, b, c;
  output d;
  wire tmp;
  wire tmp2;
  and a1 (tmp, a, b);
  dummy du (
      tmp2,
      b,
      c
  );
  or o1 (d, tmp, tmp2);
endmodule
