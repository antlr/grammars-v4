module rand_sequence1 ();
  initial begin
    randsequence (bin_op) void bin_op : value operator value{
      $display("%s %b %b", operator, value[1], value[2]);
    }; bit [7:0] value : {
      return $urandom;
    }; string operator : add := 5{
      return "+";
    } | dec := 2{
      return "-";
    } | mult := 1{
      return "*";
    }; endsequence
  end
endmodule
