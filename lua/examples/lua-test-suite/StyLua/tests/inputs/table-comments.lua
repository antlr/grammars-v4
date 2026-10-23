local x = {
	"foo", -- comment
	"bar", -- test
	"baz" -- test
}

local foo = {
	MinSize = call(0, 0),
	MaxSize = call(math.huge, 500) -- TODO: Set this up
}

local x = { -- comment
    hello = "world",
    foo = "bar",
}

local foo = { -- bar
}

local bar = { baz -- bar
}

local baz = {
	-- foo
}

local foobar = {
	"string"
} -- trailing comment


local tbl = Roact.createElement({
	-- comment
	a = test
	-- comment
})