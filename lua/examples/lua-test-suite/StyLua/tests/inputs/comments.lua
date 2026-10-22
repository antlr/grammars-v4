--[[Testing this]]
local function foo(bar, baz) print(bar,baz) end --this is a nice function  
local test = {}--this comment should stay  


local y = foo
-- comment line 1
-- comment line 2, should not be split from above comment
local x = test