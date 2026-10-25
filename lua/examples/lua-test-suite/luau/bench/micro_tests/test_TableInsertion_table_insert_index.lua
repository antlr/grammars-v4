local bench = script and require(script.Parent.bench_support) or require("bench_support")

function test()

    local ts0 = os.clock()

    for i=1,5 do
        local t = {}
        for j=1,1000 do
            table.insert(t, 1, j)
        end
    end

    local ts1 = os.clock()

    for i=1,5 do
        local t = {}
        for j=1,1000 do
            table.insert(t, 1, j)
        end

        for j=1,1000 do
            assert(t[j] == (1000- (j - 1) ) )
        end
    end

    return ts1-ts0
end

bench.runCode(test, "TableInsertion: table.insert(pos)")