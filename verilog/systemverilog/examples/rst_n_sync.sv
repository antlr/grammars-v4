// Author: Mustafa Said Ağca
// License: MIT
module rst_n_sync #(
    parameter RESET_WIDTH = 1
) (
    input wire clk,
    input wire async_rst_n,
    (* ASYNC_REG = "TRUE" *) output reg sync_rst_n = 1'b0
);
  (* ASYNC_REG = "TRUE" *) logic [0:RESET_WIDTH-1] rst_sync_n = {RESET_WIDTH{1'b0}};
  always_ff @(posedge clk or negedge async_rst_n) begin
    if (!async_rst_n) begin
      {sync_rst_n, rst_sync_n} <= {(RESET_WIDTH+1){1'b0}};
    end else begin
      {sync_rst_n, rst_sync_n} <= {rst_sync_n, 1'b1};
    end
  end
endmodule : rst_n_sync
