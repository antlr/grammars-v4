local bench = script and require(script.Parent.bench_support) or require("bench_support")

bench.runCode(function()
	for outer=1,28,3 do
		for inner=1,28,3 do
			local t2 = table.create(20, string.rep("n", outer))
			local str = ""
			for i=1,500 do
				str = table.concat(t2, string.rep("!", inner))
			end
			assert(#str)
		end
	end
end, "table: concat (small)")

bench.runCode(function()
	for outer=1,21,3 do
		for inner=1,21,3 do
			local t2 = table.create(200, string.rep("n", outer))
			local str = ""
			for i=1,100 do
				str = table.concat(t2, string.rep("!", inner))
			end
			assert(#str)
		end
	end
end, "table: concat (big)")
