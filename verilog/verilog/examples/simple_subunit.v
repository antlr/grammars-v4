module subunit0 (
    input  a,
    output b
);
  assign b = a;
endmodule
module SimpleSubunit (
    input  a,
    output b
);
  wire sig_subunit0_a;
  wire sig_subunit0_b;
  subunit0 subunit0_inst (
      .a(sig_subunit0_a),
      .b(sig_subunit0_b)
  );
  assign b = sig_subunit0_b;
  assign sig_subunit0_a = a;
endmodule
