module top;
  clocking dram @(clk);
    input #1ps address;
    input #5 output #6 data;
  endclocking
  clocking cd1 @(posedge phi1);
    input #1step state = top.cpu1.state;
  endclocking
endmodule
