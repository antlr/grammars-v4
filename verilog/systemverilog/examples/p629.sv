module printval;
  logic [11:0] r1;
  initial begin
    r1 = 10;
    $display("Printing with maximum size - :%d: :%h:", r1, r1);
    $display("Printing with minimum size - :%0d: :%0h:", r1, r1);
  end
endmodule
