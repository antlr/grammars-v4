// Author: Mustafa Said Ağca
// License: MIT
module async_fifo #(
    parameter DATA_WIDTH = 8,
    parameter ADDR_WIDTH = 3,
    parameter PIPELINE_LENGTH = 1
) (
    input wire async_rst,
    // write interface
    input wire wr_clk,
    input wire write,
    input wire [DATA_WIDTH-1:0] wr_data,
    output reg full = 0,
    // read interface
    input wire rd_clk,
    input wire read,
    output reg [DATA_WIDTH-1:0] rd_data,
    output wire empty,
    // status signals
    output reg overflow = 0,
    output reg underflow = 0
);
  logic wr_rst;
  logic rd_rst;
  reset_sync wr_rst_sync (
      .clk(wr_clk),
      .async_rst(async_rst),
      .sync_rst(wr_rst)
  );
  reset_sync rd_rst_sync (
      .clk(rd_clk),
      .async_rst(async_rst),
      .sync_rst(rd_rst)
  );
  logic [DATA_WIDTH-1:0] mem[0:2**ADDR_WIDTH-1];
	initial begin
		for(int ii = 0; ii < 2**ADDR_WIDTH; ii++) begin
			mem[ii] <= {DATA_WIDTH{1'b0}};
    end
  end
  logic [ADDR_WIDTH-1:0] wr_addr_bin;
  logic [ADDR_WIDTH:0] wr_ptr_bin = 0;
  logic [ADDR_WIDTH:0] wr_ptr_bin_next;
  logic [ADDR_WIDTH:0] wr_ptr_gray = 0;
  logic [ADDR_WIDTH:0] wr_ptr_gray_next;
  (* ASYNC_REG = "TRUE" *) logic [ADDR_WIDTH:0] wr_ptr_gray_meta = 0;
  (* ASYNC_REG = "TRUE" *) logic [ADDR_WIDTH:0] wr_ptr_gray_sync = 0;
  logic [ADDR_WIDTH-1:0] rd_addr_bin;
  logic [ADDR_WIDTH:0] rd_ptr_bin = 0;
  logic [ADDR_WIDTH:0] rd_ptr_bin_next;
  logic [ADDR_WIDTH:0] rd_ptr_gray = 0;
  logic [ADDR_WIDTH:0] rd_ptr_gray_next;
  (* ASYNC_REG = "TRUE" *) logic [ADDR_WIDTH:0] rd_ptr_gray_meta = 0;
  (* ASYNC_REG = "TRUE" *) logic [ADDR_WIDTH:0] rd_ptr_gray_sync = 0;
  logic wr_en;
  logic rd_en;
  logic full_next;
  logic empty_next;
  logic empty_reg = 1;
  assign wr_en = write && !full;
  assign wr_ptr_bin_next = wr_ptr_bin + wr_en;
  assign wr_addr_bin = wr_ptr_bin[ADDR_WIDTH-1:0];
  always_ff @(posedge wr_clk) begin
    if (wr_en) begin
      mem[wr_addr_bin] <= wr_data;
    end
  end
  always_ff @(posedge wr_clk) begin
    if (wr_rst) begin
      wr_ptr_bin <= 0;
    end else begin
      wr_ptr_bin <= wr_ptr_bin_next;
    end
  end
  assign rd_ptr_bin_next = rd_ptr_bin + rd_en;
  assign rd_addr_bin = rd_ptr_bin[ADDR_WIDTH-1:0];
  generate
    if (PIPELINE_LENGTH > 0) begin
      logic [DATA_WIDTH-1:0] rd_data_pipe[PIPELINE_LENGTH-1:0];
      logic [PIPELINE_LENGTH-1:0] data_valid_pipe = 0;
      assign rd_en = !empty_reg && (read || ~&data_valid_pipe);
      genvar gg;
      generate
        for (gg = PIPELINE_LENGTH - 1; gg > 0; gg--) begin
          always_ff @(posedge rd_clk) begin
            if (rd_rst) begin
              data_valid_pipe[gg] <= 0;
            end else if (read || ~&data_valid_pipe[gg:PIPELINE_LENGTH-1]) begin
              rd_data_pipe[gg]  <= rd_data_pipe[gg-1];
              data_valid_pipe[gg] <= data_valid_pipe[gg-1];
            end
          end
        end
      endgenerate
      always_ff @(posedge rd_clk) begin
        if (rd_rst) begin
          data_valid_pipe[0] <= 0;
        end else if (read || ~&data_valid_pipe) begin
          rd_data_pipe[0] <= mem[rd_addr_bin];
          if (!empty_reg) begin
            data_valid_pipe[0] <= 1;
          end else begin
            data_valid_pipe[0] <= 0;
          end
        end
      end
      always_ff @(posedge rd_clk) begin
        if (read && !empty) begin
          rd_data <= rd_data_pipe[PIPELINE_LENGTH-1];
        end
      end
      assign empty = !data_valid_pipe[PIPELINE_LENGTH-1];
    end else begin
      assign rd_en = !empty && read;
      always_ff @(posedge rd_clk) begin
        if (rd_en) begin
          rd_data <= mem[rd_addr_bin];
        end
      end
      assign empty = empty_reg;
    end
  endgenerate
  always_ff @(posedge rd_clk) begin
    if (rd_rst) begin
      rd_ptr_bin <= 0;
    end else begin
      rd_ptr_bin <= rd_ptr_bin_next;
    end
  end
  always_ff @(posedge wr_clk) begin
    if (wr_rst) begin
      wr_ptr_gray <= 0;
    end else begin
      wr_ptr_gray <= wr_ptr_gray_next;
    end
  end
  always_ff @(posedge rd_clk) begin
    if (rd_rst) begin
      {wr_ptr_gray_sync, wr_ptr_gray_meta} <= 0;
    end else begin
      {wr_ptr_gray_sync, wr_ptr_gray_meta} <= {wr_ptr_gray_meta, wr_ptr_gray};
    end
  end
  always_ff @(posedge rd_clk) begin
    if (rd_rst) begin
      rd_ptr_gray <= 0;
    end else begin
      rd_ptr_gray <= rd_ptr_gray_next;
    end
  end
  always_ff @(posedge wr_clk) begin
    if (wr_rst) begin
      {rd_ptr_gray_sync, rd_ptr_gray_meta} <= 0;
    end else begin
      {rd_ptr_gray_sync, rd_ptr_gray_meta} <= {rd_ptr_gray_meta, rd_ptr_gray};
    end
  end
  assign wr_ptr_gray_next = wr_ptr_bin_next ^ (wr_ptr_bin_next >> 1);
  assign full_next = (wr_ptr_gray_next[ADDR_WIDTH-:1] != rd_ptr_gray_sync[ADDR_WIDTH-:1]) && (wr_ptr_gray_next[ADDR_WIDTH-1-:1] != rd_ptr_gray_sync[ADDR_WIDTH-1-:1]) && (wr_ptr_gray_next[ADDR_WIDTH-2:0] == rd_ptr_gray_sync[ADDR_WIDTH-2:0]);
  always_ff @(posedge wr_clk) begin
    if (wr_rst) begin
      full <= 0;
    end else begin
      full <= full_next;
    end
  end
  assign rd_ptr_gray_next = rd_ptr_bin_next ^ (rd_ptr_bin_next >> 1);
  assign empty_next = rd_ptr_gray_next == wr_ptr_gray_sync;
  always_ff @(posedge rd_clk) begin
    if (rd_rst) begin
      empty_reg <= 1;
    end else begin
      empty_reg <= empty_next;
    end
  end
  always_ff @(posedge wr_clk) begin
    overflow <= write && full;
  end
  always_ff @(posedge rd_clk) begin
    underflow <= read && empty;
  end
endmodule : async_fifo
