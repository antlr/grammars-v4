local bench = script and require(script.Parent.bench_support) or require("bench_support")

function test()

	function sum(...)
		local res = 0
		local length = select("#", ...)
		for i = 1, length do
			local item = select(i, ...)
			res += item
		end
		return res
	end

	local ts0 = os.clock()

	for i=1, 100_000 do
		sum(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
	end

	local ts1 = os.clock()

	return ts1-ts0
end

bench.runCode(test, "VariadicSelect")
