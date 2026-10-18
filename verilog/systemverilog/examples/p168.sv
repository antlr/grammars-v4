module top;
  function void fn0;
    byte b [] = {1, 2, 3, 4};
    int  y;
    y = b.sum;
    y = b.product;
    y = b.xor with (item + 4);
  endfunction
  function void fn1;
    logic [7:0] m[2][2] = '{'{5, 10}, '{15, 20}};
    int y;
    y = m.sum with (item.sum with (item));
  endfunction
  function void fn2;
    logic bit_arr[1024];
    int   y;
    y = bit_arr.sum with (int'(item));
  endfunction
endmodule
