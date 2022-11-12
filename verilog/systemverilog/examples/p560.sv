covergroup cg(ref int x, ref int y, input int c);
  coverpoint x;
  b: coverpoint y;
  cx: coverpoint x;
  option.weight = c;
  cross x, y{option.weight = c;}
endgroup
covergroup g4;
  coverpoint s0 iff (!reset);
endgroup
