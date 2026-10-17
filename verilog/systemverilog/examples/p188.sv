class Base;
  typedef enum {
    bin,
    oct,
    dec,
    hex
  } radix;
  static task print(radix r, integer n);
  endtask
endclass
