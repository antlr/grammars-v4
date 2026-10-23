local bench = script and require(script.Parent.bench_support) or require("bench_support")

function test()

    function test(a) return a.bar end
    function err(e) return e end

    local ts0 = os.clock()
    for i=0,10000 do xpcall(test, err) end
    local ts1 = os.clock()

    return ts1-ts0
end

bench.runCode(test, "Failure: xpcall a.bar")