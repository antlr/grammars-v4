package p;
  function int f();
    return 1;
  endfunction
endpackage
package p2;
  function int f();
    return 1;
  endfunction
endpackage
module top;
  import p::*;
  int x;
  if (1) begin : b
    initial x = f();
  end
  import p2::*;
endmodule
