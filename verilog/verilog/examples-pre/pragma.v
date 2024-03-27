module Example (
    input clk,
    input [3:0] data_in,
    output reg [7:0] data_out
);
  `pragma synthesis_clock("clk", 10, 5, 0, 0, 0, 0)
  always @(posedge clk) begin
    `pragma translate_off
    data_out <= {4'b0, data_in} + 8'hAA;
    `pragma translate_on
  end
endmodule
