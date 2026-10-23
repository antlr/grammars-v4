module bitlength ();
  logic [3:0] a, b, c;
  logic [4:0] d;
  initial begin
    a = 9;
    b = 8;
    c = 1;
    $display("answer = %b", c ? (a & b) : d);
  end
endmodule
