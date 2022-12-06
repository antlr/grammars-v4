module msl;
  int st0;
  initial begin
    int st1;
    static int st2;
    automatic int auto1;
  end
  task automatic t1();
    int auto2;
    static int st3;
    automatic int auto3;
  endtask
endmodule
