local bench = script and require(script.Parent.bench_support) or require("bench_support")

function test()

  local ts0 = os.clock()
  for i=1,100000 do
    local t = { a = 1, b = 2, c = 3, d = 4, e = 5, f = 6 }
  end
  local ts1 = os.clock()

  return ts1-ts0
end

bench.runCode(test, "LargeTableCtor: hash")