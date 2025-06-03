local function foo(a, b, c)
end

foo(a, call())
foo(a, call(), c)
foo(a, b, call())
foo(a, b, c, call())

local function bar(...)
	foo(1, 2, 3, ...)
end
