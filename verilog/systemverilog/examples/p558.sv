class C1;
  bit [7:0] x;
  covergroup cv(int arg) @(posedge clk);
    option.at_least = arg;
    coverpoint x;
  endgroup
  function new(int p1);
    cv = new(p1);
  endfunction
endclass
module top;
  initial begin
    C1 obj = new(4);
  end
endmodule
