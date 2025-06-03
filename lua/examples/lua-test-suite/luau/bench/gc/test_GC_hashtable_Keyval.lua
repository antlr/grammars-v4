local bench = script and require(script.Parent.bench_support) or require("bench_support")

function test()
    local t = {}

    local max = 10000
    local iters = 50000

    for i = 1,iters do
        local is = tostring(i)
        local input = string.rep(is, 1000 / #is)

        t[is] = input

        -- remove old entries
        if i > max then
            t[tostring(i - max)] = nil
        end
    end
end

bench.runCode(test, "GC: hashtable keys and values")
