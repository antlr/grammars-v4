-- This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
local function checkerror(msg, f, ...)
	local s, err = pcall(f, ...)
	assert(not s and string.find(err, msg))
end

function generateClearedTable(arraySize, hashSize)
	local tb = {}
	for i = 1, arraySize do
		tb[i] = i
	end
	for i = 1, hashSize do
		tb[tostring(i)] = i
	end
	table.clear(tb)
	return tb
end

do
	checkerror("table expected", table.clear)
	checkerror("table expected", table.clear, 1, 2)

	assert(#generateClearedTable(0, 0) == 0, "table array part should stay empty")
	assert(#generateClearedTable(10, 0) == 0, "table array part should be empty (had array)")
	assert(#generateClearedTable(0, 10) == 0, "table array part should be empty (had hash)")
	assert(#generateClearedTable(10, 10) == 0, "table array part should be empty (had both)")

	assert(next(generateClearedTable(0, 0)) == nil, "table hash part should stay empty")
	assert(next(generateClearedTable(10, 0)) == nil, "table hash part should be empty (had array)")
	assert(next(generateClearedTable(0, 10)) == nil, "table hash part should be empty (had hash)")
	assert(next(generateClearedTable(10, 10)) == nil, "table hash part should be empty (had both)")

	
	for i = 1, 16 do
		local t1 = generateClearedTable(16, 0)
		local t2 = table.create(16)
		t1[i] = true
		t2[i] = true
		assert(#t1 == #t2, "table length mismatch with i=" .. i .. "(" .. #t1 .. " vs " .. #t2 .. ")")
	end

	do
		local things = {"foo", "bar", "baz", "foobar", "a", "b", "c", "d", "e", "f", "g"}
		local tb = generateClearedTable(0, 20)
		local containsAll = {}
		for _, v in ipairs(things) do
			tb[v] = true
		end
		for k, _ in pairs(tb) do
			containsAll[k] = true
		end
		for _, v in ipairs(things) do
			assert(tb[v], "key `" .. v .. "` doesn't show up in index")
			assert(containsAll[v], "key `" .. v .. "` didn't show up in iteration")
		end
	end

	do -- Check expanding the array part after clear
		local tb = generateClearedTable(10, 0)
		for i = 1, 40 do
			tb[i] = i
		end
		assert(#tb == 40, "wrong array part size after expand")
	end

	do -- Check expanding the hash part after clear
		local tb = generateClearedTable(0, 10)
		for i = 1, 40 do
			tb[tostring(i)] = i
		end
		local count = 0
		for _ in pairs(tb) do
			count = count + 1
		end
		assert(count == 40, "wrong hash part size after expand")
	end
end

return "OK"
