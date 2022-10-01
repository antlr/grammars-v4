/*
Copyright (C) 2019-2021, Gisselquist Technology, LLC

This program is free software (firmware): you can redistribute it and/or
modify it under the terms of the GNU General Public License as published
by the Free Software Foundation, either version 3 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTIBILITY or
FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License along
with this program. (It's in the $(ROOT)/doc directory. Run make with no
target there if the PDF file isn't present.) If not, see
<http://www.gnu.org/licenses/> for a copy.

License: GPL, v3, as defined and found on www.gnu.org,
http://www.gnu.org/licenses/gpl.html
*/
`default_nettype none
module skidbuffer #(
    parameter [0:0] OPT_LOWPOWER = 0,
    parameter [0:0] OPT_OUTREG = 1,
    parameter [0:0] OPT_PASSTHROUGH = 0,
    parameter DW = 8
) (
    input wire i_clk,
    i_reset,
    input wire i_valid,
    output reg o_ready,
    input wire [DW-1:0] i_data,
    output reg o_valid,
    input wire i_ready,
    output reg [DW-1:0] o_data
);
  reg [DW-1:0] r_data;
  generate
    if (OPT_PASSTHROUGH) begin : PASSTHROUGH
      always @(*) o_ready = i_ready;
      always @(*) o_valid = i_valid;
      always @(*)
        if (!i_valid && OPT_LOWPOWER) o_data = 0;
        else o_data = i_data;
      always @(*) r_data = 0;
    end else begin : LOGIC
      reg r_valid;
      initial r_valid = 0;
      always @(posedge i_clk)
        if (i_reset) r_valid <= 0;
        else if ((i_valid && o_ready) && (o_valid && !i_ready)) r_valid <= 1;
        else if (i_ready) r_valid <= 0;
      initial r_data = 0;
      always @(posedge i_clk)
        if (OPT_LOWPOWER && i_reset) r_data <= 0;
        else if (OPT_LOWPOWER && (!o_valid || i_ready)) r_data <= 0;
        else if ((!OPT_LOWPOWER || !OPT_OUTREG || i_valid) && o_ready) r_data <= i_data;
      always @(*) o_ready = !r_valid;
      if (!OPT_OUTREG) begin
        always @(*) o_valid = !i_reset && (i_valid || r_valid);
        always @(*)
          if (r_valid) o_data = r_data;
          else if (!OPT_LOWPOWER || i_valid) o_data = i_data;
          else o_data = 0;
      end else begin : REG_OUTPUT
        initial o_valid = 0;
        always @(posedge i_clk)
          if (i_reset) o_valid <= 0;
          else if (!o_valid || i_ready) o_valid <= (i_valid || r_valid);
        initial o_data = 0;
        always @(posedge i_clk)
          if (OPT_LOWPOWER && i_reset) o_data <= 0;
          else if (!o_valid || i_ready) begin
            if (r_valid) o_data <= r_data;
            else if (!OPT_LOWPOWER || i_valid) o_data <= i_data;
            else o_data <= 0;
          end
      end
    end
  endgenerate
`ifdef FORMAL
`ifdef VERIFIC
  `define FORMAL_VERIFIC
`endif
`endif
`ifdef FORMAL_VERIFIC
  property RESET_CLEARS_IVALID;
    @(posedge i_clk) i_reset |=> !i_valid;
  endproperty
  property IDATA_HELD_WHEN_NOT_READY;
    @(posedge i_clk) disable iff (i_reset) i_valid && !o_ready |=> i_valid && $stable(
        i_data
    );
  endproperty
`ifdef SKIDBUFFER
  assume property (IDATA_HELD_WHEN_NOT_READY);
`else
  assert property (IDATA_HELD_WHEN_NOT_READY);
`endif
  generate
    if (!OPT_PASSTHROUGH) begin
      assert property (@(posedge i_clk) OPT_OUTREG && i_reset |=> o_ready && !o_valid);
      assert property (@(posedge i_clk) !OPT_OUTREG && i_reset |-> !o_valid);
      assert property (@(posedge i_clk)
disable iff (i_reset)
o_valid && !i_ready
|=> (o_valid && $stable(
          o_data
      )));
      assert property (@(posedge i_clk)
disable iff (i_reset)
(i_valid && o_ready
&& (!OPT_OUTREG || o_valid) && !i_ready)
|=> (!o_ready && r_data == $past(
          i_data
      )));
      if (!OPT_OUTREG) begin
        assert property (@(posedge i_clk) disable iff (i_reset) i_ready |=> (o_valid == i_valid));
      end else begin
        assert property (@(posedge i_clk) disable iff (i_reset) i_valid && o_ready |=> o_valid);
        assert property (@(posedge i_clk)
disable iff (i_reset)
!i_valid && o_ready && i_ready |=> !o_valid);
      end
      assert property (@(posedge i_clk) !o_ready && i_ready |=> o_ready);
      if (OPT_LOWPOWER) begin
        assert property (@(posedge i_clk) (OPT_OUTREG || !i_reset) && !o_valid |-> o_data == 0);
        assert property (@(posedge i_clk) o_ready |-> r_data == 0);
      end
`ifdef SKIDBUFFER
      reg f_changed_data;
      cover property (@(posedge i_clk)
disable iff (i_reset)
(!o_valid && !i_valid)
##1 i_valid && i_ready [*3]
##1 i_valid && !i_ready
##1 i_valid && i_ready [*2]
##1 i_valid && !i_ready [*2]
##1 i_valid && i_ready [*3]
##1 o_valid && i_ready [*0:5]
##1 (!o_valid && !i_valid && f_changed_data));
      initial f_changed_data = 0;
      always @(posedge i_clk)
        if (i_reset) f_changed_data <= 1;
        else if (i_valid && $past(!i_valid || o_ready)) begin
          if (i_data != $past(i_data + 1)) f_changed_data <= 0;
        end else if (!i_valid && i_data != 0) f_changed_data <= 0;
`endif
    end
  endgenerate
`endif
endmodule
`ifndef YOSYS
`default_nettype wire
`endif
