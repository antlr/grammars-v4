module top;
  parameter genblk2 = 0;
  genvar i;
  if (genblk2) logic a;
  else logic b;
  if (genblk2) logic a;
  else logic b;
  for (i = 0; i < 1; i = i + 1) begin : g1
    if (1) logic a;
  end
  for (i = 0; i < 1; i = i + 1) if (1) logic a;
  if (1) logic a;
endmodule
