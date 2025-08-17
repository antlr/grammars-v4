local function foo(...)
end

foo()
foo(1)
foo(1, 2, 3, 4, 5)

local function bar(a, b, ...)
end

bar()
bar(1, 2)
bar(1, 2, 3)
bar(1, 2, 3, 4, 5)
