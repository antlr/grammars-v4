local bench = script and require(script.Parent.bench_support) or require("bench_support")

function test()

    local t = {}

    for i=1,100 do t[tostring(i)] = i end

    local ts0 = os.clock()
    local sum = 0
    for i=1,10000 do
    for k,v in pairs(t) do sum = sum + v end
    end
    local ts1 = os.clock()

    return ts1-ts0
end

bench.runCode(test, "TableIteration")