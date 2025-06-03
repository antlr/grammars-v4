local bench = script and require(script.Parent.bench_support) or require("bench_support")

function test()

	local tab = {}

	local ts0 = os.clock()

	for i=1, 1_000_000 do
		local j = i + 1
		tab[i] = function() return i,j end
	end

	local ts1 = os.clock()

	return ts1-ts0
end

bench.runCode(test, "UpvalueCapture")