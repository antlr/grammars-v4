// http://www.asic-world.com/code/hdl_models/cam.v
//-----------------------------------------------------
// Design Name : cam
// File Name   : cam.v
// Function    : CAM
// Coder       : Deepak Kumar Tala
//-----------------------------------------------------
module cam (
    clk,  // Cam clock
    cam_enable,  // Cam enable
    cam_data_in,  // Cam data to match
    cam_hit_out,  // Cam match has happened
    cam_addr_out  // Cam output address
);
  parameter ADDR_WIDTH = 8;
  parameter DEPTH = 1 << ADDR_WIDTH;
  //------------Input Ports--------------
  input clk;
  input cam_enable;
  input [DEPTH-1:0] cam_data_in;
  //----------Output Ports--------------
  output cam_hit_out;
  output [ADDR_WIDTH-1:0] cam_addr_out;
  //------------Internal Variables--------
  reg     [ADDR_WIDTH-1:0] cam_addr_out;
  reg                      cam_hit_out;
  reg     [ADDR_WIDTH-1:0] cam_addr_combo;
  reg                      cam_hit_combo;
  reg                      found_match;
  integer                  i;
  //-------------Code Starts Here-------
  always @(cam_data_in) begin
    cam_addr_combo = {ADDR_WIDTH{1'b0}};
    found_match    = 1'b0;
    cam_hit_combo  = 1'b0;
    for (i = 0; i < DEPTH; i = i + 1) begin
      if (cam_data_in[i] && !found_match) begin
        found_match    = 1'b1;
        cam_hit_combo  = 1'b1;
        cam_addr_combo = i;
      end else begin
        found_match    = found_match;
        cam_hit_combo  = cam_hit_combo;
        cam_addr_combo = cam_addr_combo;
      end
    end
  end
  // Register the outputs
  always @(posedge clk) begin
    if (cam_enable) begin
      cam_hit_out  <= cam_hit_combo;
      cam_addr_out <= cam_addr_combo;
    end else begin
      cam_hit_out  <= 1'b0;
      cam_addr_out <= {ADDR_WIDTH{1'b0}};
    end
  end
endmodule
