local bench = script and require(script.Parent.bench_support) or require("bench_support")

function test()

    local ts0 = os.clock()

    local iterations = 25000

    local t = table.create(iterations, 100)

    for j=1,100 do
        table.remove(t, 1)
    end

    assert(#t == (iterations - 100))

    local ts1 = os.clock()

    return ts1-ts0
end

bench.runCode(test, "TableRemoval: table.remove")
