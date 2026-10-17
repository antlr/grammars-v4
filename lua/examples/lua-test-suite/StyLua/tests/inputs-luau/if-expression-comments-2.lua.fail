-- https://github.com/JohnnyMorganz/StyLua/issues/375
local x = if true
	then foo -- comment
	else nil

-- https://github.com/JohnnyMorganz/StyLua/issues/374
context:reportError(("Required input field %s.%s cannot be deprecated."):format(inputObj.name, field.name), {
	getDeprecatedNode((field :: InputField).astNode),
	if field.astNode ~= nil
			-- ROBLOX FUNTIME START: Luau
			then (field :: any).astNode.type
			-- ROBLOX FUNTIME END
			else nil,
})
