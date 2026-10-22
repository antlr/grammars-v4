module mod1;
  typedef struct {
    int x;
    int y;
  } st;
  st  s1;
  int k = 1;
  initial begin
    #1 s1 = '{1, 2 + k};
    #1 $display(s1.x, s1.y);
    #1 s1 = '{x: 2, y: 3 + k};
    #1 $display(s1.x, s1.y);
    #1 $finish;
  end
endmodule
