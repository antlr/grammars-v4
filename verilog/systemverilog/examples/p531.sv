class C1;
  rand integer x;
endclass
class C2;
  integer x;
  integer y;
  task doit(C1 f, integer x, integer z);
    int result;
    result = f.randomize() with {x < y + z;};
  endtask
endclass
