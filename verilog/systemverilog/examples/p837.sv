primitive jk_edge_ff(q, clock, j, k, preset, clear);
  output q;
  reg q;
  input clock, j, k, preset, clear;
  table
    ? ? ? 0 1 : ? : 1;
    ? ? ? * 1 : 1 : 1;
    ? ? ? 1 0 : ? : 0;
    ? ? ? 1 * : 0 : 0;
    r 0 0 0 0 : 0 : 1;
    r 0 0 1 1 : ? : -;
    r 0 1 1 1 : ? : 0;
    r 1 0 1 1 : ? : 1;
    r 1 1 1 1 : 0 : 1;
    r 1 1 1 1 : 1 : 0;
    f ? ? ? ? : ? : -;
    b * ? ? ? : ? : -;
    b ? * ? ? : ? : -;
  endtable
endprimitive
