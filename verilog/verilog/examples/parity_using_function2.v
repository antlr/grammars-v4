// http://www.asic-world.com/code/hdl_models/parity_using_function2.v
//-----------------------------------------------------
// Design Name : parity_using_function2
// File Name   : parity_using_function2.v
// Function    : Parity using function
// Coder       : Deepak Kumar Tala
//-----------------------------------------------------
module parity_using_function2 (
    data_in,  //  8 bit data in
    parity_out  //  1 bit parity out
);
  output parity_out;
  input [7:0] data_in;
  reg parity_out;
  function parity;
    input [31:0] data;
    integer i;
    begin
      parity = 0;
      for (i = 0; i < 32; i = i + 1) begin
        parity = parity ^ data[i];
      end
    end
  endfunction
  always @(data_in) begin
    parity_out = parity(data_in);
  end
endmodule
