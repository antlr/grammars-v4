package p;
  struct {int x;} s1;
  struct {int x;} s2;
  function void f();
    int x;
  endfunction
endpackage
module m;
  import p::*;
  if (1) begin : s1
    initial begin
      s1.x = 1;
      s2.x = 1;
      f.x  = 1;
      f2.x = 1;
    end
    int x;
    some_module s2 ();
  end
endmodule
