module generic_decoder #(
    num_code_bits = 3,
    localparam num_out_bits = 1 << num_code_bits
) (
    input [num_code_bits-1:0] A,
    output reg [num_out_bits-1:0] Y
);
endmodule
