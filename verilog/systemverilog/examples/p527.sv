class A;
  rand int x;
  constraint A1 {soft x == 3;}
  constraint A2 {disable soft x;}
  constraint A3 {soft x inside {1, 2};}
endclass
module top;
  initial begin
    A a = new();
    a.randomize();
  end
endmodule
