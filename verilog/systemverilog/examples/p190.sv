typedef real T;
class C;
  typedef int T;
  extern function T f();
  extern function real f2();
endclass
function C::T C::f();
  return 1;
endfunction
function real C::f2();
  return 1.0;
endfunction
