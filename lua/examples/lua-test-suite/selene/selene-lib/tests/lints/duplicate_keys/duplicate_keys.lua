local foo = {
	a = true,
	b = false,
	a = false,
}

local bar = {
	a = true,
	b = false,
	a = false,
	c = "hello",
	a = false,
}

local baz = {
	a = true,
	b = false,
	["a"] = true,
}

-- Ignore complex expressions
local barbaz = {
	[call()] = true,
	[call()] = false,
}
