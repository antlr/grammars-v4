// http://www.asic-world.com/code/hdl_models/dff_async_reset.v
//-----------------------------------------------------
// Design Name : dff_async_reset
// File Name   : dff_async_reset.v
// Function    : D flip-flop async reset
// Coder       : Deepak Kumar Tala
//-----------------------------------------------------
module dff_async_reset (
    data,  // Data Input
    clk,  // Clock Input
    reset,  // Reset input
    q  // Q output
);
  //-----------Input Ports---------------
  input data, clk, reset;
  //-----------Output Ports---------------
  output q;
  //------------Internal Variables--------
  reg q;
  //-------------Code Starts Here---------
  always @(posedge clk or negedge reset)
    if (~reset) begin
      q <= 1'b0;
    end else begin
      q <= data;
    end
endmodule  //End Of Module dff_async_reset
