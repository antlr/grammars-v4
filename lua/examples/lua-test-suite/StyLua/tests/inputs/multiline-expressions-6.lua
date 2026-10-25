-- https://github.com/JohnnyMorganz/StyLua/issues/432: shape was not correctly reset for the new line of hanging expression
local function test()
	return "test"
		.. "test"
		.. "test"
		.. "test"
		.. "test"
		.. "test"
		.. "test"
		.. "test"
		.. "test"
		.. "test"
		.. "test"
		.. "test"
		.. "test"
		.. "test"
		.. "test"
		.. "test"
		.. "test"
		.. "test"
		.. foo(long_function_name_aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa())
end
