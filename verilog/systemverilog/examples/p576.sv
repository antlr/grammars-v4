class cg_cls;
  covergroup cg(ref logic [0:3] x, ref logic [0:7] y, ref logic [0:2] a);
    xy: coverpoint {x, y};
    coverpoint y;
    XYA: cross xy, a{}
  endgroup
  covergroup cg;
    coverpoint a {bins x[] = {[0 : 10]};}
    coverpoint b {bins y[] = {[0 : 20]};}
    aXb : cross a, b{bins one = '{'{1, 2}, '{3, 4}, '{5, 6}};}
  endgroup
endclass
