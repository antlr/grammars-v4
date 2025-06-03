local bench = script and require(script.Parent.bench_support) or require("bench_support")

function test()

	local ts0 = os.clock()
	for i=1,100000 do
		local t = table.create(100)
		for j=1,100 do t[j] = 0 end
	end
	local ts1 = os.clock()

	return ts1-ts0
end

bench.runCode(test, "TableCreate: zerofill")