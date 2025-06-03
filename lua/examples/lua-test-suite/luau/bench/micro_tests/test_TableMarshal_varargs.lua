local bench = script and require(script.Parent.bench_support) or require("bench_support")

function test()

    local function pack(...)
        return {...}
    end

    local ts0 = os.clock()

    for i=1,100000 do
        local t = pack(1,2,3,4,5,6,7,8,9,10)
    end

    local ts1 = os.clock()

    return ts1-ts0
end

bench.runCode(test, "TableMarshal: {...}")