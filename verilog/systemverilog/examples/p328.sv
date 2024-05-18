module ram_model (
    address,
    write,
    chip_select,
    data
);
  parameter data_width = 8;
  parameter ram_depth = 256;
  localparam addr_width = clogb2(ram_depth);
  input [addr_width - 1:0] address;
  input write, chip_select;
  inout [data_width - 1:0] data;
  function integer clogb2(input [31:0] value);
    value = value - 1;
    for (clogb2 = 0; value > 0; clogb2 = clogb2 + 1) value = value >> 1;
  endfunction
  logic [data_width - 1:0] data_store[0:ram_depth - 1];
endmodule : ram_model
