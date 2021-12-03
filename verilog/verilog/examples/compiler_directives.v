`timescale 1ns/10ps
`line 1 "compiler_directives.v" 1
`resetall `default_nettype none
`unconnected_drive pull1
module top (x, y);`celldefine
  `include "verilog_header.vh"
  input x;
  output y;
  `unconnected_drive pull0
  wire z;
  wire [`SIGNAL_WIDTH_ (z)-1:0] w;
  `begin_keywords "1364-2005" `ifdef SOME_MACRO always @(posedge x) w <= y^z; `else
    always @(*) w = y+z;
    `define __SOMETHING 1
    // comment inside else_group_of_lines
      `ifdef ASD_
        genvar g; generate for(gg = 0; gg < 5; gg=gg+1) assign a[gg] = b[4-gg]; endgenerate
      `else
        assign a = b ;
      `endif
    assign y = x; `endif
  `define _INVERT_(a, b)\
    assign b = ~a;\
    /* comment inside macro_text */\
    `end_keywords // this is also inside macro_text
  `_INVERT_(x, y) // macro_usage
  `undef _INVERT_
  NOT u1 (z, y);
  `pragma _PRAGMA_NAME_ prgm_kywrd0, prgm_kywrd1 = "prgm_val1", prgm_id
  // on the default channel, we expect to see the following tokens
/*moduletop(x,y);inputx;outputy;wirez;wire[-1:0]w;NOTu1(z,y);endmodule<EOF>*/
endmodule `endcelldefine
`default_nettype wire
