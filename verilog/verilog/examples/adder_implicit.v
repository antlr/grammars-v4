//-----------------------------------------------------
// http://www.asic-world.com/verilog/syntax2.html
// This is simple adder Program
// Design Name : adder_implicit
// File Name   : adder_implicit.v
// Function    : This program shows how implicit
//               port connection are done
// Coder       : Deepak Kumar Tala
//-----------------------------------------------------
module adder_implicit (
    result,  // Output of the adder
    carry,  // Carry output of adder
    r1,  // first input
    r2,  // second input
    ci  // carry input
);
  // Input Port Declarations       
  input [3:0] r1;
  input [3:0] r2;
  input ci;
  // Output Port Declarations
  output [3:0] result;
  output carry;
  // Port Wires
  wire [3:0] r1;
  wire [3:0] r2;
  wire       ci;
  wire [3:0] result;
  wire       carry;
  // Internal variables
  wire       c1;
  wire       c2;
  wire       c3;
  // Code Starts Here
  addbit u0 (
      r1[0],
      r2[0],
      ci,
      result[0],
      c1
  );
  addbit u1 (
      r1[1],
      r2[1],
      c1,
      result[1],
      c2
  );
  addbit u2 (
      r1[2],
      r2[2],
      c2,
      result[2],
      c3
  );
  addbit u3 (
      r1[3],
      r2[3],
      c3,
      result[3],
      carry
  );
endmodule  // End Of Module adder
