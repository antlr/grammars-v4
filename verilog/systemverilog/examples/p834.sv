primitive srff(q, s, r);
  output q;
  reg q;
  input s, r;
initial q = 1'b1;
  table
    1 0 : ? : 1;
    f 0 : 1 : -;
    0 r : ? : 0;
    0 f : 0 : -;
    1 1 : ? : 0;
  endtable
endprimitive
