local bench = script and require(script.Parent.bench_support) or require("bench_support")

function test()

    function fact (n)
        if n == 0 then
            return 1
        else
            return n * fact(n-1)
        end
    end

    local ts0 = os.clock()
    for loops=1,500 do
        for i=1,100 do
            fact(i)
        end
    end
    local ts1 = os.clock()

    return ts1 - ts0
end

bench.runCode(test, "Factorial")