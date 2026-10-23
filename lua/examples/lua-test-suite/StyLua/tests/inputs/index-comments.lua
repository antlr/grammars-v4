local x = foo[
	index -- test
]

foo[
	var -- string
] = baz

local x = foo[
	x -- string
][y][z][p
-- string
]


local x = foo[index --[[comment]]]
