local foo = bar -- trailing comment

-- leading comment
local bar = baz
local baz = foo

do
	local foo = bar
	-- comment
	local bar = baz
end