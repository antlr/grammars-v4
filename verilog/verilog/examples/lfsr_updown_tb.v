// http://www.asic-world.com/examples/verilog/lfsr.html#Random_Counter_(LFSR)
`define WIDTH 8
module tb ();
  reg clk;
  reg reset;
  reg enable;
  reg up_down;
  wire [`WIDTH-1 : 0] count;
  wire overflow;
  initial begin
    $monitor("rst %b en %b updown %b cnt %b overflow %b", reset, enable, up_down, count, overflow);
    clk = 0;
    reset = 1;
    enable = 0;
    up_down = 0;
    #10 reset = 0;
    #1 enable = 1;
    #20 up_down = 1;
    #30 $finish;
  end
  always #1 clk = ~clk;
  lfsr_updown U (
      .clk     (clk),
      .reset   (reset),
      .enable  (enable),
      .up_down (up_down),
      .count   (count),
      .overflow(overflow)
  );
endmodule
