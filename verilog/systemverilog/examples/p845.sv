module ALU (
    o1,
    i1,
    i2,
    opcode
);
  input [7:0] i1, i2;
  input [2:1] opcode;
  output [7:0] o1;
  specify
    if (opcode == 2'b00) (i1, i2 *> o1) = (25.0, 25.0);
    if (opcode == 2'b01) (i1 => o1) = (5.6, 8.0);
    if (opcode == 2'b10) (i2 => o1) = (5.6, 8.0);
    (opcode *> o1) = (6.1, 6.5);
  endspecify
endmodule
