-- This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
print("testing coverage")

function foo()
	local x = 1
	local y = 2
	assert(x + y)
end

function bar()
	local function one(x)
		return x
	end

	local two = function(x)
		return x
	end

	one(1)
end

function validate(stats, hits, misses)
	local checked = {}

	for _,l in ipairs(hits) do
		if not (stats[l] and stats[l] > 0) then
			return false, string.format("expected line %d to be hit", l)
		end
		checked[l] = true
	end

	for _,l in ipairs(misses) do
		if not (stats[l] and stats[l] == 0) then
			return false, string.format("expected line %d to be missed", l)
		end
		checked[l] = true
	end

	for k,v in pairs(stats) do
		if type(k) == "number" and not checked[k] then
			return false, string.format("expected line %d to be absent", k)
		end
	end

	return true
end

foo()
c = getcoverage(foo)
assert(#c == 1)
assert(c[1].name == "foo")
assert(c[1].linedefined == 4)
assert(c[1].depth == 0)
assert(validate(c[1], {5, 6, 7}, {}))

bar()
c = getcoverage(bar)
assert(#c == 3)
assert(c[1].name == "bar")
assert(c[1].linedefined == 10)
assert(c[1].depth == 0)
assert(validate(c[1], {11, 15, 19}, {}))
assert(c[2].name == "one")
assert(c[2].linedefined == 11)
assert(c[2].depth == 1)
assert(validate(c[2], {12}, {}))
assert(c[3].name == nil)
assert(c[3].linedefined == 15)
assert(c[3].depth == 1)
assert(validate(c[3], {}, {16}))

return 'OK'
