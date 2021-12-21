`timescale 1ns / 10ps
`line 1 "compiler_directives.v" 1
`resetall
`default_nettype none
`unconnected_drive pull1
`begin_keywords "1364-2005"
module top (
    x,
    y
);
  `celldefine
  `include "verilog_header.vh"
  `define ASD_
  input x;
  output y;
  `unconnected_drive pull0
  wire z;
  wire [`SIGNAL_WIDTH_ (z)-1:0] w;
`ifdef SOME_MACRO
  always @(posedge x) w <= y ^ z;
`else
  always @(*) w = y + z;
  `define __SOMETHING 1
`ifdef ASD_
  genvar gg;
  generate
    for (gg = 0; gg < 5; gg = gg + 1) assign a[gg] = b[4-gg];
  endgenerate
`elsif QWERTY
  initial begin
    z = $clog2(9);
  end
`else
  assign a = b;
`endif
  assign y = x;
`endif
  `define _INVERT_(a, b)\
assign b = ~a;\
// comment inside macro text
  `_INVERT_(x, y)  // macro usage
  `undef _INVERT_
  NOT u1 (
      z,
      y
  );
endmodule
`end_keywords
`endcelldefine
`default_nettype wire
