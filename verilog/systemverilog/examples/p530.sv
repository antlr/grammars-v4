class SimpleSum;
  rand bit [7:0] x, y, z;
  constraint c {z == x + y;}
endclass
task InlineConstraintDemo(SimpleSum p);
  int success;
  success = p.randomize() with {x < y;};
endtask
