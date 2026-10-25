covergroup cg @(posedge clk);
  coverpoint v_a {
    bins sa = (4 => 5 => 6), ([7 : 9], 10 => 11, 12);
    bins sb[] = (4 => 5 => 6), ([7 : 9], 10 => 11, 12);
    bins sc = (12 => 3 [-> 1]);
    bins allother = default sequence;
  }
endgroup
