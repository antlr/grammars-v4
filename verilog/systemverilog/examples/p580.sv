covergroup g1(int w, string instComment) @(posedge clk);
  option.per_instance = 1;
  option.comment = instComment;
  a: coverpoint a_var {option.auto_bin_max = 128;}
  b: coverpoint b_var {option.weight = w;}
  c1 : cross a_var, b_var;
endgroup
