-- https://github.com/JohnnyMorganz/StyLua/issues/254
if condition1 then
	print("Do something")

--[[
	my multiline comment
]]
elseif condition2 then
	print("Do something else")

-- my single line comment
elseif condition3 then
	print("Do some final thing")
end

if condition then
	-- this comment should be indent
elseif x == true then
-- this comment should not be indented
elseif x == true then
				-- this comment should be indented, but only by one
else
	print("hi")
end
