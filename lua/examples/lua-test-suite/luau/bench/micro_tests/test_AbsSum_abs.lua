local bench = script and require(script.Parent.bench_support) or require("bench_support")

function test()

    local abs = math.abs
    local ts0 = os.clock()
    local sum = 0
    for i=-500000,500000 do sum = sum + abs(i) end
    local ts1 = os.clock()

    return ts1-ts0
end

bench.runCode(test, "AbsSum: abs")