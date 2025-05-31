-- https://github.com/JohnnyMorganz/StyLua/issues/489
do
	local result = diff(
		{ test = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 } },
		{ test = { 1, 2, 3, 4, 5, 6, 7, 8, 10, 9 } },
		options
	)
end

do
	local diff = createANewTableFromThisOne { thisIsAField = true, thisIsAnotherField = true, thisIsAFinalField = true, x = y }
end
