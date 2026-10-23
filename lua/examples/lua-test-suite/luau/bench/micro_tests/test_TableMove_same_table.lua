local bench = script and require(script.Parent.bench_support) or require("bench_support")

function test()
    local t = table.create(5000001, 0)

    for i=0,5000000 do
        t[i] = i
    end

    local ts0 = os.clock()
    table.move(t, 1, 250000, 250001, t)
    local ts1 = os.clock()

    for i=250001,(500000-1) do
        assert(t[i] == (i - 250001) + 1)
    end

    return ts1-ts0
end

bench.runCode(test, "TableMove: same table")
