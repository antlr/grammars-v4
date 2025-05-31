local bench = script and require(script.Parent.bench_support) or require("bench_support")

function test()

    local t = {}

    for i=1,1000000 do t[i] = i end

    local ts0 = os.clock()
    local sum = 0
    for k,v in pairs(t) do sum = sum + v end
    local ts1 = os.clock()

    return ts1-ts0
end

bench.runCode(test, "LargeTableSum: for k,v in pairs")