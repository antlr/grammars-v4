typedef struct {
  real field1;
  bit  field2;
} T;
function automatic T Tsum(input T driver[]);
  Tsum.field1 = 0.0;
  foreach (driver[i]) Tsum.field1 += driver[i].field1;
endfunction
nettype T wT;
nettype T wTsum with Tsum;
typedef real TR[5];
nettype TR wTR;
nettype wTsum nettypeid2;
