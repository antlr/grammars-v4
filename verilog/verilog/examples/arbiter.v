//http://www.asic-world.com/code/hdl_models/arbiter.v
//----------------------------------------------------
// A four level, round-robin arbiter. This was
// orginally coded by WD Peterson in VHDL.
//----------------------------------------------------
module arbiter (
    clk,
    rst,
    req3,
    req2,
    req1,
    req0,
    gnt3,
    gnt2,
    gnt1,
    gnt0
);
  // --------------Port Declaration----------------------- 
  input clk;
  input rst;
  input req3;
  input req2;
  input req1;
  input req0;
  output gnt3;
  output gnt2;
  output gnt1;
  output gnt0;
  //--------------Internal Registers----------------------
  wire [1:0] gnt;
  wire       comreq;
  wire       beg;
  wire [1:0] lgnt;
  wire       lcomreq;
  reg        lgnt0;
  reg        lgnt1;
  reg        lgnt2;
  reg        lgnt3;
  reg        lasmask;
  reg        lmask0;
  reg        lmask1;
  reg        ledge;
  //--------------Code Starts Here----------------------- 
  always @(posedge clk)
    if (rst) begin
      lgnt0 <= 0;
      lgnt1 <= 0;
      lgnt2 <= 0;
      lgnt3 <= 0;
    end else begin
      lgnt0 <=(~lcomreq & ~lmask1 & ~lmask0 & ~req3 & ~req2 & ~req1 & req0)
        | (~lcomreq & ~lmask1 &  lmask0 & ~req3 & ~req2 &  req0)
        | (~lcomreq &  lmask1 & ~lmask0 & ~req3 &  req0)
        | (~lcomreq &  lmask1 &  lmask0 & req0  )
        | ( lcomreq & lgnt0 );
      lgnt1 <=(~lcomreq & ~lmask1 & ~lmask0 &  req1)
        | (~lcomreq & ~lmask1 &  lmask0 & ~req3 & ~req2 &  req1 & ~req0)
        | (~lcomreq &  lmask1 & ~lmask0 & ~req3 &  req1 & ~req0)
        | (~lcomreq &  lmask1 &  lmask0 &  req1 & ~req0)
        | ( lcomreq &  lgnt1);
      lgnt2 <=(~lcomreq & ~lmask1 & ~lmask0 &  req2  & ~req1)
        | (~lcomreq & ~lmask1 &  lmask0 &  req2)
        | (~lcomreq &  lmask1 & ~lmask0 & ~req3 &  req2  & ~req1 & ~req0)
        | (~lcomreq &  lmask1 &  lmask0 &  req2 & ~req1 & ~req0)
        | ( lcomreq &  lgnt2);
      lgnt3 <=(~lcomreq & ~lmask1 & ~lmask0 & req3  & ~req2 & ~req1)
        | (~lcomreq & ~lmask1 &  lmask0 & req3  & ~req2)
        | (~lcomreq &  lmask1 & ~lmask0 & req3)
        | (~lcomreq &  lmask1 &  lmask0 & req3  & ~req2 & ~req1 & ~req0)
        | ( lcomreq & lgnt3);
    end
  //----------------------------------------------------
  // lasmask state machine.
  //----------------------------------------------------
  assign beg = (req3 | req2 | req1 | req0) & ~lcomreq;
  always @(posedge clk) begin
    lasmask <= (beg & ~ledge & ~lasmask);
    ledge   <= (beg & ~ledge & lasmask) | (beg & ledge & ~lasmask);
  end
  //----------------------------------------------------
  // comreq logic.
  //----------------------------------------------------
  assign lcomreq = (req3 & lgnt3) | (req2 & lgnt2) | (req1 & lgnt1) | (req0 & lgnt0);
  //----------------------------------------------------
  // Encoder logic.
  //----------------------------------------------------
  assign lgnt = {(lgnt3 | lgnt2), (lgnt3 | lgnt1)};
  //----------------------------------------------------
  // lmask register.
  //----------------------------------------------------
  always @(posedge clk)
    if (rst) begin
      lmask1 <= 0;
      lmask0 <= 0;
    end else if (lasmask) begin
      lmask1 <= lgnt[1];
      lmask0 <= lgnt[0];
    end else begin
      lmask1 <= lmask1;
      lmask0 <= lmask0;
    end
  assign comreq = lcomreq;
  assign gnt    = lgnt;
  //----------------------------------------------------
  // Drive the outputs
  //----------------------------------------------------
  assign gnt3   = lgnt3;
  assign gnt2   = lgnt2;
  assign gnt1   = lgnt1;
  assign gnt0   = lgnt0;
endmodule
