// Author: Mustafa Said Ağca
// License: MIT
package ip_pkg;
  class ipv4_header;
    localparam VERSION = 4;
    localparam IHL = 5;
    localparam DSCP = 0;
    localparam ECN = 0;
    localparam MTU_SIZE = 1500;
    const static bit [3:0] version = VERSION;
    const static bit [3:0] ihl = IHL;
    const static bit [5:0] dscp = DSCP;
    const static bit [1:0] ecn = ECN;
    rand bit [15:0] total_length;
    rand bit [15:0] identification;
    rand bit [2:0] flags;
    rand bit [12:0] fragment_offset;
    rand bit [7:0] ttl;
    rand bit [7:0] protocol;
    bit [15:0] header_checksum;
    rand bit [31:0] source_address;
    rand bit [31:0] destination_address;
    constraint c_length {
      if (flags == 3'b100) {(total_length - 20) % 8 == 0;}
      total_length > 20;
      total_length <= MTU_SIZE;
    }
    constraint c_flags {flags inside {3'b000, 3'b010, 3'b100};}
    constraint c_offset {if (flags == 3'b010) {fragment_offset == 0;}}
    constraint c_protocol {protocol inside {8'h11};}
    virtual function automatic void calculate_checksum;
      bit [16:0] checksum;
      checksum = 0;
      checksum = checksum[15:0] + {version, ihl, dscp, ecn};
      checksum = checksum[15:0] + total_length + checksum[16];
      checksum = checksum[15:0] + identification + checksum[16];
      checksum = checksum[15:0] + {flags, fragment_offset} + checksum[16];
      checksum = checksum[15:0] + {ttl, protocol} + checksum[16];
      checksum = checksum[15:0] + 16'd0 + checksum[16];
      checksum = checksum[15:0] + source_address[31:16] + checksum[16];
      checksum = checksum[15:0] + source_address[15:0] + checksum[16];
      checksum = checksum[15:0] + destination_address[31:16] + checksum[16];
      checksum = checksum[15:0] + destination_address[15:0] + checksum[16];
      header_checksum = ~checksum[15:0];
    endfunction : calculate_checksum
    function void post_randomize;
      calculate_checksum();
    endfunction : post_randomize
  endclass : ipv4_header
  class ipv4_frame extends ipv4_header;
    rand bit [7:0] payload[];
    constraint c_payload {payload.size() == total_length - 20;}
  endclass : ipv4_frame
endpackage : ip_pkg
