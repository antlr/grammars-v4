/*
Copyright (C) 2015-2021, Gisselquist Technology, LLC

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
module ufifo #(
    parameter BW = 8,
    parameter [3:0] LGFLEN = 4,
    parameter [0:0] RXFIFO = 1'b1,
    parameter FLEN = (1 << LGFLEN)
) (
    input wire i_clk,
    i_reset,
    input wire i_wr,
    input wire [(BW-1):0] i_data,
    output wire o_empty_n,
    input wire i_rd,
    output wire [(BW-1):0] o_data,
    output wire [15:0] o_status,
    output wire o_err
);
  reg [(BW-1):0] fifo[0:(FLEN-1)];
  reg [(BW-1):0] r_data, last_write;
  reg [(LGFLEN-1):0] wr_addr, rd_addr, r_next;
  reg will_overflow, will_underflow;
  reg osrc;
  wire [(LGFLEN-1):0] w_waddr_plus_one, w_waddr_plus_two;
  wire w_write, w_read;
  reg [(LGFLEN-1):0] r_fill;
  wire [3:0] lglen;
  wire w_half_full;
  reg [9:0] w_fill;
  assign w_write = (i_wr && (!will_overflow || i_rd));
  assign w_read = (i_rd && o_empty_n);
  assign w_waddr_plus_two = wr_addr + 2;
  assign w_waddr_plus_one = wr_addr + 1;
  initial will_overflow = 1'b0;
  always @(posedge i_clk)
    if (i_reset) will_overflow <= 1'b0;
    else if (i_rd) will_overflow <= (will_overflow) && (i_wr);
    else if (w_write) will_overflow <= (will_overflow) || (w_waddr_plus_two == rd_addr);
    else if (w_waddr_plus_one == rd_addr) will_overflow <= 1'b1;
  initial wr_addr = 0;
  always @(posedge i_clk)
    if (i_reset) wr_addr <= {(LGFLEN) {1'b0}};
    else if (w_write) wr_addr <= w_waddr_plus_one;
  always @(posedge i_clk) if (w_write) fifo[wr_addr] <= i_data;
  initial will_underflow = 1'b1;
  always @(posedge i_clk)
    if (i_reset) will_underflow <= 1'b1;
    else if (i_wr) will_underflow <= 1'b0;
    else if (w_read) will_underflow <= (will_underflow) || (r_next == wr_addr);
  initial rd_addr = 0;
  initial r_next = 1;
  always @(posedge i_clk)
    if (i_reset) begin
      rd_addr <= 0;
      r_next  <= 1;
    end else if (w_read) begin
      rd_addr <= rd_addr + 1;
      r_next  <= rd_addr + 2;
    end
  always @(posedge i_clk) if (w_read) r_data <= fifo[r_next[LGFLEN-1:0]];
  always @(posedge i_clk)
    if (i_wr && (!o_empty_n || (w_read && r_next == wr_addr)))
      last_write <= i_data;
  initial osrc = 1'b0;
  always @(posedge i_clk)
    if (i_reset) osrc <= 1'b0;
    else if (i_wr && (!o_empty_n || (w_read && r_next == wr_addr))) osrc <= 1'b1;
    else if (i_rd) osrc <= 1'b0;
  assign o_data = (osrc) ? last_write : r_data;
  generate
    if (RXFIFO) begin : RXFIFO_FILL
      initial r_fill = 0;
      always @(posedge i_clk)
        if (i_reset) r_fill <= 0;
        else
          case ({
            w_write, w_read
          })
            2'b01: r_fill <= r_fill - 1'b1;
            2'b10: r_fill <= r_fill + 1'b1;
            default: begin
            end
          endcase
    end else begin : TXFIFO_FILL
      initial r_fill = -1;
      always @(posedge i_clk)
        if (i_reset) r_fill <= -1;
        else
          case ({
            w_write, w_read
          })
            2'b01: r_fill <= r_fill + 1'b1;
            2'b10: r_fill <= r_fill - 1'b1;
            default: begin
            end
          endcase
    end
  endgenerate
  assign o_err = (i_wr && !w_write);
  assign lglen = LGFLEN;
  always @(*) begin
    w_fill = 0;
    w_fill[(LGFLEN-1):0] = r_fill;
  end
  assign w_half_full = r_fill[(LGFLEN-1)];
  assign o_status = {lglen, w_fill, w_half_full, (RXFIFO != 0) ? !will_underflow : !will_overflow};
  assign o_empty_n = !will_underflow;
`ifdef FORMAL
  reg f_past_valid;
  initial f_past_valid = 0;
  always @(posedge i_clk) f_past_valid <= 1;
  reg  [LGFLEN-1:0] f_fill;
  wire [LGFLEN-1:0] f_raddr_plus_one;
  always @(*) f_fill = wr_addr - rd_addr;
  always @(*) assert (will_underflow == (f_fill == 0));
  always @(*) assert (will_overflow == (&f_fill));
  assign f_raddr_plus_one = rd_addr + 1;
  always @(*) assert (f_raddr_plus_one == r_next);
  always @(*)
    if (will_underflow) begin
      assert (!w_read);
      assert (!osrc);
    end
  always @(posedge i_clk)
    if (RXFIFO)
      assert (r_fill == f_fill);
      else assert (r_fill == (~f_fill));
`ifdef UFIFO
  (* anyconst *) reg [LGFLEN-1:0] f_const_addr;
  (* anyconst *) reg [BW-1:0] f_const_data, f_const_second;
  reg [LGFLEN-1:0] f_next_addr;
  reg [1:0] f_state;
  reg f_first_in_fifo, f_second_in_fifo;
  reg [LGFLEN-1:0] f_distance_to_first, f_distance_to_second;
  always @(*) begin
    f_next_addr = f_const_addr + 1;
    f_distance_to_first = f_const_addr - rd_addr;
    f_distance_to_second = f_next_addr - rd_addr;
    f_first_in_fifo = (f_distance_to_first < f_fill)
&& !will_underflow
&& (fifo[f_const_addr] == f_const_data);
    f_second_in_fifo = (f_distance_to_second < f_fill)
&& !will_underflow
&& (fifo[f_next_addr] == f_const_second);
  end
  initial f_state = 2'b00;
  always @(posedge i_clk)
    if (i_reset) f_state <= 2'b00;
    else
      case (f_state)
        2'b00:
        if (w_write && (wr_addr == f_const_addr) && (i_data == f_const_data)) f_state <= 2'b01;
        2'b01:
        if (w_read && (rd_addr == f_const_addr)) f_state <= 2'b00;
        else if (w_write && (wr_addr == f_next_addr))
          f_state <= (i_data == f_const_second) ? 2'b10 : 2'b00;
        2'b10: if (w_read && (rd_addr == f_const_addr)) f_state <= 2'b11;
        2'b11: if (w_read) f_state <= 2'b00;
      endcase
  always @(*)
    case (f_state)
      2'b00: begin
      end
      2'b01: begin
        assert (!will_underflow);
        assert (f_first_in_fifo);
        assert (!f_second_in_fifo);
        assert (wr_addr == f_next_addr);
        assert (fifo[f_const_addr] == f_const_data);
        if (rd_addr == f_const_addr) assert (o_data == f_const_data);
      end
      2'b10: begin
        assert (f_first_in_fifo);
        assert (f_second_in_fifo);
      end
      2'b11: begin
        assert (f_second_in_fifo);
        assert (rd_addr == f_next_addr);
        assert (o_data == f_const_second);
      end
    endcase
`endif
  reg cvr_filled;
  always @(*) cover (o_empty_n);
`ifdef UFIFO
  always @(*) cover (o_err);
  initial cvr_filled = 0;
  always @(posedge i_clk)
    if (i_reset) cvr_filled <= 0;
    else if (&f_fill[LGFLEN-1:0]) cvr_filled <= 1;
  always @(*) cover (cvr_filled && !o_empty_n);
`endif
`endif
endmodule
