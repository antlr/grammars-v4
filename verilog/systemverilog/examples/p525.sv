class D;
  int x;
endclass
class C;
  rand int x, y;
  D a, b;
  constraint c1 {(x < y && (a.x > b.x || a.x == 5)) -> x + y == 10;}
endclass
class Packet;
  rand bit mode;
  rand int length;
  constraint deflt {
    soft length inside {32, 1024};
    soft mode -> length == 1024;
  }
endclass
