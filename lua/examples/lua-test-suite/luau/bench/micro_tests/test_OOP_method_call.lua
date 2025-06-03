local bench = script and require(script.Parent.bench_support) or require("bench_support")

function test()

    local Number = {}
    Number.__index = Number

    function Number.new(v)
        local self = {
            value = v
        }
        setmetatable(self, Number)
        return self
    end

    function Number:Get()
        return self.value
    end

    local n = Number.new(42)

    local ts0 = os.clock()
    for i=1,1000000 do
        local nv = n:Get()
    end
    local ts1 = os.clock()

    return ts1-ts0
end

bench.runCode(test, "OOP: method call")