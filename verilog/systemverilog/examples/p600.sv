typedef bit node;
node [2:0] X;
int signed Y;
package A;
  enum {
    A,
    B,
    C = 99
  } X;
  typedef bit [9:1'b1] word;
endpackage : A
import A::*;
module top;
  typedef struct {node A, B;} AB_t;
  AB_t AB[10];
endmodule
