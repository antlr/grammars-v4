module module0 ();
  function clkout_duty_chk;
    input CLKOUT_DIVIDE;
    input CLKOUT_DUTY_CYCLE;
    input reg [160:0] CLKOUT_DUTY_CYCLE_N;
    integer CLKOUT_DIVIDE, step_tmp;
    real CLKOUT_DUTY_CYCLE;
    real CLK_DUTY_CYCLE_MIN, CLK_DUTY_CYCLE_MAX, CLK_DUTY_CYCLE_STEP;
    real CLK_DUTY_CYCLE_MIN_rnd;
    reg  clk_duty_tmp_int;
    begin
      clkout_duty_chk = 1'b1;
    end
  endfunction
endmodule
