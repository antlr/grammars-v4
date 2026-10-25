-- This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
-- This file is based on Lua 5.x tests -- https://github.com/lua/lua/tree/master/testes

-- Generate an error (i.e. throw an exception) inside a tag method which is indirectly
-- called via pcall.
-- This test is meant to detect a regression in handling errors inside a tag method

local testtable = {}
setmetatable(testtable, { __index = function() error("Error") end })

pcall(function()
	testtable.missingmethod()
end)

return('OK')
