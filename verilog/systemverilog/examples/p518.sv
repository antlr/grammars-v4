class B;
  rand bit s;
  rand bit [31:0] d;
  constraint c {s -> d == 0;}
endclass
