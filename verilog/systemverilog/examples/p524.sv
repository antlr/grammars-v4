class D;
  int x;
endclass
class C;
  rand int x, y;
  D a, b;
  constraint c1 {(x < y || a.x > b.x || a.x == 5) -> x + y == 10;}
endclass
class D;
  int x;
endclass
class C;
  rand int x, y;
  D a, b;
  constraint c1 {(x < y && a.x > b.x && a.x == 5) -> x + y == 10;}
endclass
