module shift;
  logic [3:0] start, result;
  initial begin
    start  = 1;
    result = (start << 2);
  end
endmodule
module ashift;
  logic signed [3:0] start, result;
  initial begin
    start  = 4'b1000;
    result = (start >>> 2);
  end
endmodule
