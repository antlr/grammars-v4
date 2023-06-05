module top;
  logic phi1, phi2;
  bus_A a (phi1);
  bus_B b (phi2);
  test main (
      a,
      b
  );
  cpu cpu1 (a);
  mem mem1 (b);
endmodule
