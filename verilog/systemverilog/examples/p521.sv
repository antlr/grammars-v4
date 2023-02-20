function int count_ones(bit [9:0] w);
  for (count_ones = 0; w != 0; w = w >> 1) count_ones += w & 1'b1;
endfunction
