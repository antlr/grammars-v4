typedef union packed {
  s_atmcell acell;
  bit [423:0] bit_slice;
  bit [52:0][7:0] byte_slice;
} u_atmcell;
