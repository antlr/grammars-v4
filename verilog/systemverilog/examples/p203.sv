typedef class C2;
class C1;
  C2 c;
endclass
class C2;
  C1 c;
endclass
typedef class C;
module top;
  C #(1, real) v2;
  C #(
      .p(2),
      .T(real)
  ) v3;
endmodule
class C #(
    parameter p = 2,
    type T = int
);
endclass
