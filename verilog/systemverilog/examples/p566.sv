covergroup sg @(posedge clk);
  coverpoint v {
    bins b2 = (2 [-> 3: 5]);
    bins b3 = (3 [-> 3: 5]);
    bins b5 = (5 [* 3]);
    bins b6 = (1 => 2 [= 3: 6] => 5);
  }
endgroup
