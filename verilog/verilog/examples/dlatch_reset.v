// http://www.asic-world.com/code/hdl_models/dlatch_reset.v
//-----------------------------------------------------
// Design Name : dlatch_reset
// File Name   : dlatch_reset.v
// Function    : DLATCH async reset
// Coder       : Deepak Kumar Tala
//-----------------------------------------------------
module dlatch_reset (
    data,  // Data Input
    en,  // LatchInput
    reset,  // Reset input
    q  // Q output
);
  //-----------Input Ports---------------
  input data, en, reset;
  //-----------Output Ports---------------
  output q;
  //------------Internal Variables--------
  reg q;
  //-------------Code Starts Here---------
  always @(en or reset or data)
    if (~reset) begin
      q <= 1'b0;
    end else if (en) begin
      q <= data;
    end
endmodule  //End Of Module dlatch_reset
