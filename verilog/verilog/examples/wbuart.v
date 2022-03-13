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
module wbuart #(
    parameter [30:0] INITIAL_SETUP = 31'd25,
    parameter [3:0] LGFLEN = 4,
    parameter [0:0] HARDWARE_FLOW_CONTROL_PRESENT = 1'b1,
    parameter [3:0] LCLLGFLEN = (LGFLEN > 4'ha) ? 4'ha : ((LGFLEN < 4'h2) ? 4'h2 : LGFLEN)
) (
    input wire i_clk,
    i_reset,
    input wire i_wb_cyc,
    input wire i_wb_stb,
    i_wb_we,
    input wire [1:0] i_wb_addr,
    input wire [31:0] i_wb_data,
    input wire [3:0] i_wb_sel,
    output wire o_wb_stall,
    output reg o_wb_ack,
    output reg [31:0] o_wb_data,
    input wire i_uart_rx,
    output wire o_uart_tx,
    input wire i_cts_n,
    output reg o_rts_n,
    output wire o_uart_rx_int,
    o_uart_tx_int,
    o_uart_rxfifo_int,
    o_uart_txfifo_int
);
  localparam [1:0] UART_SETUP = 2'b00, UART_FIFO = 2'b01, UART_RXREG = 2'b10, UART_TXREG = 2'b11;
  wire tx_busy;
  reg [30:0] uart_setup;
  wire rx_stb, rx_break, rx_perr, rx_ferr, ck_uart;
  wire [7:0] rx_uart_data;
  reg rx_uart_reset;
  wire rx_empty_n, rx_fifo_err;
  wire [7:0] rxf_wb_data;
  wire [15:0] rxf_status;
  reg rxf_wb_read;
  wire [(LCLLGFLEN-1):0] check_cutoff;
  reg r_rx_perr, r_rx_ferr;
  wire [31:0] wb_rx_data;
  wire tx_empty_n, txf_err, tx_break;
  wire [ 7:0] tx_data;
  wire [15:0] txf_status;
  reg txf_wb_write, tx_uart_reset;
  reg [7:0] txf_wb_data;
  wire [31:0] wb_tx_data;
  wire [31:0] wb_fifo_data;
  reg [1:0] r_wb_addr;
  reg r_wb_ack;
  initial uart_setup = INITIAL_SETUP | ((HARDWARE_FLOW_CONTROL_PRESENT == 1'b0) ? 31'h40000000 : 0);
  always @(posedge i_clk)
    if ((i_wb_stb) && (i_wb_addr == UART_SETUP) && (i_wb_we)) begin
      if (i_wb_sel[0]) uart_setup[7:0] <= i_wb_data[7:0];
      if (i_wb_sel[1]) uart_setup[15:8] <= i_wb_data[15:8];
      if (i_wb_sel[2]) uart_setup[23:16] <= i_wb_data[23:16];
      if (i_wb_sel[3])
        uart_setup[30:24] <= {
          (i_wb_data[30]) || (!HARDWARE_FLOW_CONTROL_PRESENT), i_wb_data[29:24]
        };
    end
`ifdef USE_LITE_UART
  rxuartlite #(
      .CLOCKS_PER_BAUD(INITIAL_SETUP[23:0])
  ) rx (
      i_clk,
      i_uart_rx,
      rx_stb,
      rx_uart_data
  );
  assign rx_break = 1'b0;
  assign rx_perr  = 1'b0;
  assign rx_ferr  = 1'b0;
  assign ck_uart  = 1'b0;
`else
  rxuart #(
      .INITIAL_SETUP(INITIAL_SETUP)
  ) rx (
      i_clk,
      (i_reset) || (rx_uart_reset),
      uart_setup,
      i_uart_rx,
      rx_stb,
      rx_uart_data,
      rx_break,
      rx_perr,
      rx_ferr,
      ck_uart
  );
`endif
  ufifo #(
      .LGFLEN(LCLLGFLEN),
      .RXFIFO(1)
  ) rxfifo (
      i_clk,
      (i_reset) || (rx_break) || (rx_uart_reset),
      rx_stb,
      rx_uart_data,
      rx_empty_n,
      rxf_wb_read,
      rxf_wb_data,
      rxf_status,
      rx_fifo_err
  );
  assign o_uart_rxfifo_int = rxf_status[1];
  assign o_uart_rx_int = rxf_status[0];
  assign check_cutoff = -3;
  always @(posedge i_clk)
    o_rts_n <= ((HARDWARE_FLOW_CONTROL_PRESENT)
&&(!uart_setup[30])
&&(rxf_status[(LCLLGFLEN+1):2] > check_cutoff));
  initial rxf_wb_read = 1'b0;
  always @(posedge i_clk) rxf_wb_read <= (i_wb_stb) && (i_wb_addr[1:0] == UART_RXREG) && (!i_wb_we);
  initial r_rx_perr = 1'b0;
  initial r_rx_ferr = 1'b0;
  always @(posedge i_clk)
    if ((rx_uart_reset) || (rx_break)) begin
      r_rx_perr <= 1'b0;
      r_rx_ferr <= 1'b0;
    end else if ((i_wb_stb) && (i_wb_addr[1:0] == UART_RXREG) && (i_wb_we)) begin
      if (i_wb_sel[1]) begin
        r_rx_perr <= (r_rx_perr) && (~i_wb_data[9]);
        r_rx_ferr <= (r_rx_ferr) && (~i_wb_data[10]);
      end
    end else if (rx_stb) begin
      r_rx_perr <= (r_rx_perr) || (rx_perr);
      r_rx_ferr <= (r_rx_ferr) || (rx_ferr);
    end
  initial rx_uart_reset = 1'b1;
  always @(posedge i_clk)
    if ((i_reset) || ((i_wb_stb) && (i_wb_addr[1:0] == UART_SETUP) && (i_wb_we)))
      rx_uart_reset <= 1'b1;
    else if ((i_wb_stb) && (i_wb_addr[1:0] == UART_RXREG) && (i_wb_we) && i_wb_sel[1])
      rx_uart_reset <= i_wb_data[12];
    else rx_uart_reset <= 1'b0;
  assign wb_rx_data = {
    16'h00, 3'h0, rx_fifo_err, rx_break, rx_ferr, r_rx_perr, !rx_empty_n, rxf_wb_data
  };
  initial txf_wb_write = 1'b0;
  always @(posedge i_clk) begin
    txf_wb_write <= (i_wb_stb) && (i_wb_addr == UART_TXREG) && (i_wb_we) && (i_wb_sel[0]);
    txf_wb_data  <= i_wb_data[7:0];
  end
  ufifo #(
      .LGFLEN(LGFLEN),
      .RXFIFO(0)
  ) txfifo (
      i_clk,
      (tx_break) || (tx_uart_reset),
      txf_wb_write,
      txf_wb_data,
      tx_empty_n,
      (!tx_busy) && (tx_empty_n),
      tx_data,
      txf_status,
      txf_err
  );
  assign o_uart_tx_int = txf_status[0];
  assign o_uart_txfifo_int = txf_status[1];
`ifndef USE_LITE_UART
  reg r_tx_break;
  initial r_tx_break = 1'b0;
  always @(posedge i_clk)
    if (i_reset) r_tx_break <= 1'b0;
    else if ((i_wb_stb) && (i_wb_addr[1:0] == UART_TXREG) && (i_wb_we) && (i_wb_sel[1]))
      r_tx_break <= i_wb_data[9];
  assign tx_break = r_tx_break;
`else
  assign tx_break = 1'b0;
`endif
  initial tx_uart_reset = 1'b1;
  always @(posedge i_clk)
    if ((i_reset) || ((i_wb_stb) && (i_wb_addr == UART_SETUP) && (i_wb_we))) tx_uart_reset <= 1'b1;
    else if ((i_wb_stb) && (i_wb_addr[1:0] == UART_TXREG) && (i_wb_we) && i_wb_sel[1])
      tx_uart_reset <= i_wb_data[12];
    else tx_uart_reset <= 1'b0;
`ifdef USE_LITE_UART
  txuartlite #(
      .CLOCKS_PER_BAUD(INITIAL_SETUP[23:0])
  ) tx (
      i_clk,
      (tx_empty_n),
      tx_data,
      o_uart_tx,
      tx_busy
  );
`else
  wire cts_n;
  assign cts_n = (HARDWARE_FLOW_CONTROL_PRESENT) && (i_cts_n);
  txuart #(
      .INITIAL_SETUP(INITIAL_SETUP)
  ) tx (
      i_clk,
      1'b0,
      uart_setup,
      r_tx_break,
      (tx_empty_n),
      tx_data,
      cts_n,
      o_uart_tx,
      tx_busy
  );
`endif
  assign wb_tx_data = {
    16'h00,
    i_cts_n,
    txf_status[1:0],
    txf_err,
    ck_uart,
    o_uart_tx,
    tx_break,
    (tx_busy | txf_status[0]),
    (tx_busy | txf_status[0]) ? txf_wb_data : 8'b00
  };
  assign wb_fifo_data = {txf_status, rxf_status};
  always @(posedge i_clk) r_wb_addr <= i_wb_addr;
  initial r_wb_ack = 1'b0;
  always @(posedge i_clk) r_wb_ack <= i_wb_stb;
  initial o_wb_ack = 1'b0;
  always @(posedge i_clk) o_wb_ack <= i_wb_cyc && r_wb_ack;
  always @(posedge i_clk)
    casez (r_wb_addr)
      UART_SETUP: o_wb_data <= {1'b0, uart_setup};
      UART_FIFO:  o_wb_data <= wb_fifo_data;
      UART_RXREG: o_wb_data <= wb_rx_data;
      UART_TXREG: o_wb_data <= wb_tx_data;
    endcase
  assign o_wb_stall = 1'b0;
  wire unused;
  assign unused = &{1'b0, i_wb_data[31]};
endmodule
