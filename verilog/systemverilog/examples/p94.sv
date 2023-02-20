class Base #(
    parameter p = 1
);
  typedef struct {
    real r;
    bit [p-1:0] data;
  } T;
  static function T Tsum(input T driver[]);
    Tsum.r = 0.0;
    Tsum.data = 0;
    foreach (driver[i]) Tsum.data += driver[i].data;
    Tsum.r = $itor(Tsum.data);
  endfunction
endclass
typedef Base#(32) MyBaseT;
nettype MyBaseT::T narrowTsum with MyBaseT::Tsum;
typedef Base#(64) MyBaseType;
nettype MyBaseType::T wideTsum with MyBaseType::Tsum;
narrowTsum net1;
wideTsum net2;
