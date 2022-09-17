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
module txuartlite #(
    parameter [4:0] TIMING_BITS = 5'd24,
    parameter TB = TIMING_BITS,
    parameter [(TB-1):0] CLOCKS_PER_BAUD = 8
) (
    input wire i_clk,
    input wire i_wr,
    input wire [7:0] i_data,
    output reg o_uart_tx,
    output wire o_busy
);
  localparam [3:0] TXUL_BIT_ZERO = 4'h0, TXUL_STOP = 4'h8, TXUL_IDLE = 4'hf;
  reg [(TB-1):0] baud_counter;
  reg [3:0] state;
  reg [7:0] lcl_data;
  reg r_busy, zero_baud_counter;
  initial r_busy = 1'b1;
  initial state = TXUL_IDLE;
  always @(posedge i_clk) begin
    if (!zero_baud_counter) r_busy <= 1'b1;
    else if (state > TXUL_STOP) begin
      state  <= TXUL_IDLE;
      r_busy <= 1'b0;
      if ((i_wr) && (!r_busy)) begin
        r_busy <= 1'b1;
        state  <= TXUL_BIT_ZERO;
      end
    end else begin
      r_busy <= 1'b1;
      if (state <= TXUL_STOP) state <= state + 1'b1;
      else state <= TXUL_IDLE;
    end
  end
  assign o_busy = (r_busy);
  initial lcl_data = 8'hff;
  always @(posedge i_clk)
    if ((i_wr) && (!r_busy)) lcl_data <= i_data;
    else if (zero_baud_counter) lcl_data <= {1'b1, lcl_data[7:1]};
  initial o_uart_tx = 1'b1;
  always @(posedge i_clk)
    if ((i_wr) && (!r_busy)) o_uart_tx <= 1'b0;
    else if (zero_baud_counter) o_uart_tx <= lcl_data[0];
  initial zero_baud_counter = 1'b1;
  initial baud_counter = 0;
  always @(posedge i_clk) begin
    zero_baud_counter <= (baud_counter == 1);
    if (state == TXUL_IDLE) begin
      baud_counter <= 0;
      zero_baud_counter <= 1'b1;
      if ((i_wr) && (!r_busy)) begin
        baud_counter <= CLOCKS_PER_BAUD - 1'b1;
        zero_baud_counter <= 1'b0;
      end
    end else if (!zero_baud_counter) baud_counter <= baud_counter - 1'b1;
    else if (state > TXUL_STOP) begin
      baud_counter <= 0;
      zero_baud_counter <= 1'b1;
    end else if (state == TXUL_STOP) baud_counter <= CLOCKS_PER_BAUD - 2;
    else baud_counter <= CLOCKS_PER_BAUD - 1'b1;
  end
`ifdef FORMAL
`ifdef TXUARTLITE
  `define ASSUME assume
`else
  `define ASSUME assert
`endif
  reg f_past_valid, f_last_clk;
  reg [(TB-1):0] f_baud_count;
  reg [9:0] f_txbits;
  reg [3:0] f_bitcount;
  reg [7:0] f_request_tx_data;
  wire [3:0] subcount;
  initial f_past_valid = 1'b0;
  always @(posedge i_clk) f_past_valid <= 1'b1;
  initial `ASSUME(!i_wr);
  always @(posedge i_clk)
    if ((f_past_valid) && ($past(i_wr)) && ($past(o_busy))) begin
      `ASSUME(i_wr == $past(i_wr));
      `ASSUME(i_data == $past(i_data));
    end
  always @(posedge i_clk) assert (zero_baud_counter == (baud_counter == 0));
  always @(posedge i_clk)
    if ((f_past_valid) && ($past(baud_counter != 0)) && ($past(state != TXUL_IDLE)))
      assert (baud_counter == $past(baud_counter - 1'b1));
  always @(posedge i_clk)
    if ((f_past_valid) && (!$past(zero_baud_counter)) && ($past(state != TXUL_IDLE)))
      assert ($stable(o_uart_tx));
  initial f_baud_count = 1'b0;
  always @(posedge i_clk)
    if (zero_baud_counter) f_baud_count <= 0;
    else f_baud_count <= f_baud_count + 1'b1;
  always @(posedge i_clk) assert (f_baud_count < CLOCKS_PER_BAUD);
  always @(posedge i_clk) if (baud_counter != 0) assert (o_busy);
  initial f_txbits = 0;
  always @(posedge i_clk) if (zero_baud_counter) f_txbits <= {o_uart_tx, f_txbits[9:1]};
  always @(posedge i_clk)
    if ((f_past_valid) && (!$past(zero_baud_counter)) && (!$past(state == TXUL_IDLE)))
      assert (state == $past(state));
  initial f_bitcount = 0;
  always @(posedge i_clk)
    if ((!f_past_valid) || (!$past(f_past_valid))) f_bitcount <= 0;
    else if ((state == TXUL_IDLE) && (zero_baud_counter)) f_bitcount <= 0;
    else if (zero_baud_counter) f_bitcount <= f_bitcount + 1'b1;
  always @(posedge i_clk) assert (f_bitcount <= 4'ha);
  always @(*) if (!o_busy) assert (zero_baud_counter);
  always @(posedge i_clk) if ((i_wr) && (!o_busy)) f_request_tx_data <= i_data;
  assign subcount = 10 - f_bitcount;
  always @(posedge i_clk) if (f_bitcount > 0) assert (!f_txbits[subcount]);
  always @(posedge i_clk)
    if (f_bitcount == 4'ha) begin
      assert (f_txbits[8:1] == f_request_tx_data);
      assert (f_txbits[9]);
    end
  always @(posedge i_clk) assert ((state <= TXUL_STOP + 1'b1) || (state == TXUL_IDLE));
  always @(posedge i_clk)
    if ((f_past_valid) && ($past(f_past_valid)) && ($past(o_busy)))
      cover (!o_busy);
`endif
`ifdef VERIFIC_SVA
  reg [7:0] fsv_data;
  always @(posedge i_clk) if ((i_wr) && (!o_busy)) fsv_data <= i_data;
  sequence BAUD_INTERVAL(CKS, DAT, SR, ST);
    ((o_uart_tx == DAT)&&(state == ST)
&&(lcl_data == SR)
&&(!zero_baud_counter))[*(CKS-1)]
##1 (o_uart_tx == DAT)&&(state == ST)
&&(lcl_data == SR)
&&(zero_baud_counter);
  endsequence
  sequence SEND(CKS, DATA);
    BAUD_INTERVAL(
        CKS, 1'b0, DATA, 4'h0
    ) ##1 BAUD_INTERVAL(
        CKS, DATA[0], {{(1) {1'b1}}, DATA[7:1]}, 4'h1
    ) ##1 BAUD_INTERVAL(
        CKS, DATA[1], {{(2) {1'b1}}, DATA[7:2]}, 4'h2
    ) ##1 BAUD_INTERVAL(
        CKS, DATA[2], {{(3) {1'b1}}, DATA[7:3]}, 4'h3
    ) ##1 BAUD_INTERVAL(
        CKS, DATA[3], {{(4) {1'b1}}, DATA[7:4]}, 4'h4
    ) ##1 BAUD_INTERVAL(
        CKS, DATA[4], {{(5) {1'b1}}, DATA[7:5]}, 4'h5
    ) ##1 BAUD_INTERVAL(
        CKS, DATA[5], {{(6) {1'b1}}, DATA[7:6]}, 4'h6
    ) ##1 BAUD_INTERVAL(
        CKS, DATA[6], {{(7) {1'b1}}, DATA[7:7]}, 4'h7
    ) ##1 BAUD_INTERVAL(
        CKS, DATA[7], 8'hff, 4'h8
    ) ##1 BAUD_INTERVAL(
        CKS - 1, 1'b1, 8'hff, 4'h9
    );
  endsequence
  assert property (@(posedge i_clk) (i_wr) && (!o_busy) |=> ((o_busy) throughout SEND(
      CLOCKS_PER_BAUD, fsv_data
  )) ##1 (!o_busy) && (o_uart_tx) && (zero_baud_counter));
  assume property (@(posedge i_clk) (i_wr) && (o_busy) |=> (i_wr) && ($stable(i_data)));
  always @(*) assert ((o_busy) || (zero_baud_counter));
  always @(*) assert (zero_baud_counter == (baud_counter == 0));
  always @(*) assert (baud_counter < CLOCKS_PER_BAUD);
  always @(*) assert ((state <= TXUL_STOP + 1'b1) || (state == TXUL_IDLE));
`endif
endmodule
