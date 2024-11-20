// http://www.asic-world.com/code/hdl_models/aFifo.v
//==========================================
// Function : Asynchronous FIFO (w/ 2 asynchronous clocks).
// Coder    : Alex Claros F.
// Date     : 15/May/2005.
// Notes    : This implementation is based on the article
//            'Asynchronous FIFO in Virtex-II FPGAs'
//            writen by Peter Alfke. This TechXclusive
//            article can be downloaded from the
//            Xilinx website. It has some minor modifications.
//=========================================
module aFifo #(
    parameter    DATA_WIDTH    = 8,
    ADDRESS_WIDTH = 4,
    FIFO_DEPTH    = (1 << ADDRESS_WIDTH)
)
//Reading port
(
    output reg  [DATA_WIDTH-1:0] Data_out,
    output reg                   Empty_out,
    input  wire                  ReadEn_in,
    input  wire                  RClk,
    //Writing port.
    input  wire [DATA_WIDTH-1:0] Data_in,
    output reg                   Full_out,
    input  wire                  WriteEn_in,
    input  wire                  WClk,
    input  wire                  Clear_in
);
  /////Internal connections & variables//////
  reg [DATA_WIDTH-1:0] Mem[FIFO_DEPTH-1:0];
  wire [ADDRESS_WIDTH-1:0] pNextWordToWrite, pNextWordToRead;
  wire EqualAddresses;
  wire NextWriteAddressEn, NextReadAddressEn;
  wire Set_Status, Rst_Status;
  reg Status;
  wire PresetFull, PresetEmpty;
  //////////////Code///////////////
  //Data ports logic:
  //(Uses a dual-port RAM).
  //'Data_out' logic:
  always @(posedge RClk) if (ReadEn_in & !Empty_out) Data_out <= Mem[pNextWordToRead];
  //'Data_in' logic:
  always @(posedge WClk) if (WriteEn_in & !Full_out) Mem[pNextWordToWrite] <= Data_in;
  //Fifo addresses support logic:
  //'Next Addresses' enable logic:
  assign NextWriteAddressEn = WriteEn_in & ~Full_out;
  assign NextReadAddressEn  = ReadEn_in & ~Empty_out;
  //Addreses (Gray counters) logic:
  GrayCounter GrayCounter_pWr (
      .GrayCount_out(pNextWordToWrite),
      .Enable_in(NextWriteAddressEn),
      .Clear_in(Clear_in),
      .Clk(WClk)
  );
  GrayCounter GrayCounter_pRd (
      .GrayCount_out(pNextWordToRead),
      .Enable_in(NextReadAddressEn),
      .Clear_in(Clear_in),
      .Clk(RClk)
  );
  //'EqualAddresses' logic:
  assign EqualAddresses = (pNextWordToWrite == pNextWordToRead);
  //'Quadrant selectors' logic:
  assign Set_Status = (pNextWordToWrite[ADDRESS_WIDTH-2] ~^ pNextWordToRead[ADDRESS_WIDTH-1]) &
                         (pNextWordToWrite[ADDRESS_WIDTH-1] ^  pNextWordToRead[ADDRESS_WIDTH-2]);
  assign Rst_Status = (pNextWordToWrite[ADDRESS_WIDTH-2] ^  pNextWordToRead[ADDRESS_WIDTH-1]) &
                         (pNextWordToWrite[ADDRESS_WIDTH-1] ~^ pNextWordToRead[ADDRESS_WIDTH-2]);
  //'Status' latch logic:
  always @(Set_Status, Rst_Status, Clear_in)  //D Latch w/ Asynchronous Clear & Preset.
    if (Rst_Status | Clear_in) Status = 0;  //Going 'Empty'.
    else if (Set_Status) Status = 1;  //Going 'Full'.
  //'Full_out' logic for the writing port:
  assign PresetFull = Status & EqualAddresses;  //'Full' Fifo.
  always @(posedge WClk, posedge PresetFull)  //D Flip-Flop w/ Asynchronous Preset.
    if (PresetFull) Full_out <= 1;
    else Full_out <= 0;
  //'Empty_out' logic for the reading port:
  assign PresetEmpty = ~Status & EqualAddresses;  //'Empty' Fifo.
  always @(posedge RClk, posedge PresetEmpty)  //D Flip-Flop w/ Asynchronous Preset.
    if (PresetEmpty) Empty_out <= 1;
    else Empty_out <= 0;
endmodule
