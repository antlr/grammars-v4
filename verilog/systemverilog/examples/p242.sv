module dff (
    q,
    d,
    clear,
    preset,
    clock
);
  output q;
  input d, clear, preset, clock;
  logic q;
  always @(clear or preset)
    if (!clear) assign q = 0;
    else if (!preset) assign q = 1;
    else deassign q;
  always @(posedge clock) q = d;
endmodule
