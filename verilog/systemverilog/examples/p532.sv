class C;
  rand integer x;
endclass
function int F(C obj, integer y);
  F = obj.randomize() with (x) {x < y;};
endfunction
class C;
  rand integer x;
endclass
function int F(C obj, integer x);
  F = obj.randomize() with {x < local:: x;};
endfunction
