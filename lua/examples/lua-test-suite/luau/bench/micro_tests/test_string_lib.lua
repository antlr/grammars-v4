local bench = script and require(script.Parent.bench_support) or require("bench_support")

bench.runCode(function()
	local src = string.rep("abcdefghijklmnopqrstuvwxyz", 100)
	local str = ""
	for i=1,1000 do
		str = string.upper(src)
		str = string.reverse(str)
		str = string.lower(str)
	end
	assert(#str)
end, "string: reverse/upper/lower (large)")

bench.runCode(function()
	local str = ""
	for i=1,100000 do
		src = "abcdefghijklmnopqrstuvwxyz" .. i
		str = string.upper(src)
		str = string.reverse(str)
		str = string.lower(str)
	end
	assert(#str)
end, "string: reverse/upper/lower (unique)")

bench.runCode(function()
	local str = ""
	for i=1,1000000 do
		str = string.rep("_", 19)
	end
	assert(#str)
end, "string: rep (small)")

bench.runCode(function()
	local str = ""
	for i=1,100 do
		str = string.rep("abcd", 100000)
	end
	assert(#str)
end, "string: rep (large)")
