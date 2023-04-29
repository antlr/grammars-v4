// http://www.asic-world.com/code/hdl_models/rom_using_file.v
//-----------------------------------------------------
// Design Name : rom_using_file
// File Name   : rom_using_file.v
// Function    : ROM using readmemh
// Coder       : Deepak Kumar Tala
//-----------------------------------------------------
module rom_using_file (
    address,  // Address input
    data,  // Data output
    read_en,  // Read Enable
    ce  // Chip Enable
);
  input [7:0] address;
  output [7:0] data;
  input read_en;
  input ce;
  reg [7:0] mem[0:255];
  assign data = (ce && read_en) ? mem[address] : 8'b0;
  initial begin
    $readmemb("memory.list", mem);  // memory_list is memory file
  end
endmodule
