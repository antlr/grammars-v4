class B;
  rand int x, y;
  constraint C {x <= F(y);}
  constraint D {y inside {2, 4, 8};}
endclass
class SList;
  rand int   n;
  rand Slist next;
  constraint sort {n < next.n;}
endclass
