covergroup cg23;
  coverpoint a {ignore_bins ignore_vals = {7, 8}; ignore_bins ignore_trans = (1 => 3 => 5);}
endgroup
covergroup cg3;
  coverpoint b {illegal_bins bad_vals = {1, 2, 3}; illegal_bins bad_trans = (4 => 5 => 6);}
endgroup
