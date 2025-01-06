module top ();
  wire [2:0] out;
  wire [2:0] out1;
  wire [2:0] out2;
  genvar i;
  generate
    for (i = 0; i < 3; i = i + 1) assign out[i] = i;
  endgenerate
  generate
    for (i = 0; i < 3; i = i + 1) begin
      assign out1[i] = i;
    end
  endgenerate
  generate
    for (i = 0; i < 3; i = i + 1) begin : test1
      assign out2[i] = i;
    end
  endgenerate
endmodule
