/*
MIT License

Copyright (c) 2021 layup

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/
class TestStat;
  static int pass_cnt = 0, fail_cnt = 0;
  static function void report();
    $display("[RPT]test done with %0d PASSs, %0d FAILs", pass_cnt, fail_cnt);
  endfunction : report
endclass
`define REPORT_TEST(args) TestStat::report();
`define EXPECT_EQ_BASE(equality, expect, actual, format) \
do begin \
if ((equality) == 1) begin \
TestStat::pass_cnt++; \
end else begin \
TestStat::fail_cnt++; \
$write("<%s:%0d>", `__FILE__, `__LINE__); \
$display($sformatf("[ASSERT_FAIL] exp: %s act: %s", format, format), expect, actual); \
end \
end while(0);
`define EXPECT_EQ_INT(expect, actual) `EXPECT_EQ_BASE((expect)==(actual), expect, actual, "%0d")
`define EXPECT_EQ_DOUBLE(expect, actual) `EXPECT_EQ_BASE((expect)==(actual), expect, actual, "%.17g")
function bit strEqu(string lhs, rhs);
  if (lhs.len() != rhs.len()) begin
    return 0;
  end
  foreach (lhs[i]) begin
    if (lhs[i] != rhs[i]) begin
      return 0;
    end
  end
  return 1;
endfunction : strEqu
`define EXPECT_EQ_STRING(expect, actual) `EXPECT_EQ_BASE((strEqu(expect, actual)), expect, actual, "%s")
`define EXPECT_NEQ_BASE(equality, expect, actual, format) \
do begin \
if ((equality) == 0) begin \
TestStat::pass_cnt++; \
end else begin \
TestStat::fail_cnt++; \
$write("<%s:%0d>", `__FILE__, `__LINE__); \
$display({"[ASSERT_FAIL]", "exp: ", format, " act: ", format}, expect, actual); \
end \
end while(0);
`define EXPECT_NEQ_INT(expect, actual) `EXPECT_NEQ_BASE((expect)==(actual), expect, actual, "%d")
`define EXPECT_NEQ_DOUBLE(expect, actual) `EXPECT_NEQ_BASE((expect)==(actual), expect, actual, "%.17g")
`define EXPECT_NEQ_STRING(expect, actual) `EXPECT_NEQ_BASE((strEqu(expect, actual)), expect, actual, "%s")
