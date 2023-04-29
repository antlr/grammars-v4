// comment 0
// comment 1
// comment with some indinet
/**
 * https://timetoexplore.net/blog/block-ram-in-verilog-with-vivado
 * simple example of sram
 */
module sram #(
    // widht of address
    parameter ADDR_WIDTH = 8,
    // widht of memory cell in memory
    DATA_WIDTH = 8,
    // number of cells in memory
    DEPTH = 256
) (
    // main clock signal
    input wire i_clk,
    input wire [ADDR_WIDTH-1:0] i_addr,
    // write enable signal
    input wire i_write,
    // input data
    input wire [DATA_WIDTH-1:0] i_data,
    // output data
    output reg [DATA_WIDTH-1:0] o_data
);
  // main memory
  reg [DATA_WIDTH-1:0] memory_array[0:DEPTH-1];
  // the main write/read process
  always @(posedge i_clk) begin
    // write or read decision
    if (i_write) begin
      // write data
      memory_array[i_addr] <= i_data;
    end else begin
      // read data
      o_data <= memory_array[i_addr];
    end
  end
endmodule
