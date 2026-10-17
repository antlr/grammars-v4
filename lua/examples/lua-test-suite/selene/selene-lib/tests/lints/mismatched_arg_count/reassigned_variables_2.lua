local function foo(a, b, c)
	print("hello from a")
end

function updateFoo(a, b, c, d)
	foo = function()
		print("UPDATED FOO")
	end
end

foo(1, 2, 3, 4)
updateFoo()
foo(1, 2, 3, 4)
