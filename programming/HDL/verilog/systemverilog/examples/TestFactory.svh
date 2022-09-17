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
virtual class TestPrototype;
  pure virtual task test();
endclass : TestPrototype
class TestFactory;
  static local TestPrototype tests[string];
  static function void register(TestPrototype obj, string test_name);
    assert (obj != null)
    else begin
      $display("Can't register null object in factory!!");
      return;
    end
    if (tests.exists(test_name)) begin
      $display("Override obj registered with '%s'!!", test_name);
    end
    tests[test_name] = obj;
  endfunction : register
  static task run_test(string test_name);
    if (!tests.exists(test_name)) begin
      $display("Can't find %s in factory!!", test_name);
      listTests();
      return;
    end
    tests[test_name].test();
  endtask : run_test
  static task run_all_test();
    string tn;
    if (tests.first(tn)) begin
      $display("Running test: %s", tn);
      do begin
        tests[tn].test();
      end while (tests.next(
          tn
      ));
    end
  endtask
  static function void listTests();
    string tn;
    if (tests.first(tn)) begin
      $display("Tests available: ");
      do begin
        $display("   %s", tn);
      end while (tests.next(
          tn
      ));
    end else begin
      $display("No test in factory!!");
    end
  endfunction : listTests
endclass
`define __register(T) \
static local T reg_obj = get(); \
static local function T get(); \
if (reg_obj == null) begin \
reg_obj = new();  \
TestFactory::register(reg_obj, `"T`"); \
end \
return reg_obj; \
endfunction
task factory_run_test();
  string tn;
  if ($value$plusargs("TEST=%s", tn)) begin
    if (tn == "test_all") begin
      TestFactory::run_all_test();
    end else begin
      TestFactory::run_test(tn);
    end
  end else begin
    $display("Please offer a +TEST=<test_name> in simulation arguments");
    TestFactory::listTests();
  end
endtask : factory_run_test
