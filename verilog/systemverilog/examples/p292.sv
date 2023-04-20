package pex_gen9_common_expressions;
  let valid_arb(request, valid, override) = |(request & valid) || override;
endpackage
module my_checker;
  import pex_gen9_common_expressions::*;
  logic a, b;
  wire [1:0] req;
  wire [1:0] vld;
  logic ovr;
  initial begin
    if (valid_arb(.request(req), .valid(vld), .override(ovr))) begin
    end
  end
endmodule
