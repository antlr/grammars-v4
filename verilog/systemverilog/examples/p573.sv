module top;
  bit [7:0] v_a, v_b;
  covergroup cg @(posedge clk);
    a: coverpoint v_a {
      bins a1 = {[0 : 63]};
      bins a2 = {[64 : 127]};
      bins a3 = {[128 : 191]};
      bins a4 = {[192 : 255]};
    }
    b: coverpoint v_b {
      bins b1 = {0}; bins b2 = {[1 : 84]}; bins b3 = {[85 : 169]}; bins b4 = {[170 : 255]};
    }
    c : cross a, b{
      bins c1 = !binsof (a) intersect {[100 : 200]};
      bins c2 = binsof (a.a2) || binsof (b.b2);
      bins c3 = binsof (a.a1) && binsof (b.b4);
    }
  endgroup
endmodule
