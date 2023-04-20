module tb3;
  vdff #(10, 15) mod_a (
      .out(out_a),
      .in (in_a),
      .clk(clk)
  );
  vdff mod_b (
      .out(out_b),
      .in (in_b),
      .clk(clk)
  );
  vdff #(
      .delay(12)
  ) mod_c (
      .out(out_c),
      .in (in_c),
      .clk(clk)
  );
endmodule
