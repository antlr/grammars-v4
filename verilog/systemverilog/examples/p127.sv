module top_legal;
  int svar1 = 1;
  initial begin
    for (int i = 0; i < 3; i++) begin
      automatic int loop3 = 0;
      for (int j = 0; j < 3; j++) begin
        loop3++;
        $display(loop3);
      end
    end
    for (int i = 0; i < 3; i++) begin
      static int loop2 = 0;
      for (int j = 0; j < 3; j++) begin
        loop2++;
        $display(loop2);
      end
    end
  end
endmodule : top_legal
