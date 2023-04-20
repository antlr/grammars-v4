typedef enum {
  low,
  mid,
  high
} AddrType;
class MyBus extends Bus;
  rand AddrType atype;
  constraint addr_range {
    (atype == low) -> addr inside {[0 : 15]};
    (atype == mid) -> addr inside {[16 : 127]};
    (atype == high) -> addr inside {[128 : 255]};
  }
endclass
