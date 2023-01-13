module capacitor;
  logic data, gate;
  trireg (large) #(0, 0, 50) cap1;
  nmos nmos1 (cap1, data, gate);
  initial begin
    $monitor("%0d data=%v gate=%v cap1=%v", $time, data, gate, cap1);
    data = 1;
    gate = 1;
    #10 gate = 0;
    #30 gate = 1;
    #10 gate = 0;
    #100 $finish;
  end
endmodule
