local bench = script and require(script.Parent.bench_support) or require("bench_support")

function test()
    local t = table.create(250001, 0)

    for i=1,250000 do
        t[i] = i
    end

    local t2 = {}

    local ts0 = os.clock()
    table.move(t, 1, 250000, 1, t2)
    local ts1 = os.clock()

    for i=1,250000-1 do
        assert(t2[i] == i)
    end

    return ts1-ts0
end

bench.runCode(test, "TableMove: {}")
