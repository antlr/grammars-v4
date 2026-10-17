local bench = script and require(script.Parent.bench_support) or require("bench_support")

function test()

    local function consume(...)
    end

    local t = {n=10,1,2,3,4,5,6,7,8,9,10}

    local ts0 = os.clock()

    for i=1,1000000 do
        consume(table.unpack(t, 1, t.n))
    end

    local ts1 = os.clock()

    return ts1-ts0
end

bench.runCode(test, "TableMarshal: table.unpack/n")