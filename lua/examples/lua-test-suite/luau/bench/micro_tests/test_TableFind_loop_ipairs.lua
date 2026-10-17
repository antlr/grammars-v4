local bench = script and require(script.Parent.bench_support) or require("bench_support")

function test()

    local function find(t, v)
        for i,e in ipairs(t) do
            if e == v then
                return i
            end
        end

        return nil
    end

    local t = {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20}

    local ts0 = os.clock()
    for i=1,100000 do find(t,15) end
    local ts1 = os.clock()

    return ts1-ts0
end

bench.runCode(test, "TableFind: for+ipairs")