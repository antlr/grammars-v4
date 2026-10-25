local bench = script and require(script.Parent.bench_support) or require("bench_support")

function test()

    local Number = {}
    Number.__index = Number

    function Number:new(class, v)
        local self = {
            value = v
        }
        setmetatable(self, Number)
        return self
    end

    function Number:Get()
        return self.value
    end

    local ts0 = os.clock()
    for i=1,100000 do
        local n = Number:new(42)
    end
    local ts1 = os.clock()

    return ts1-ts0
end

bench.runCode(test, "OOP: virtual constructor")