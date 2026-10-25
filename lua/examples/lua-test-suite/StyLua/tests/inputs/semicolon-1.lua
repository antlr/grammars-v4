local x = 1; -- comment
do
	return; -- bad
end; -- comment

local y; -- comment
z = 5; -- comment

repeat x = x + 1 until x > 5; -- comment

for x,y in pairs(z) do
	break; -- comment
end; -- comment

if x then end; -- comment

function foo()
end; -- comment

local function bar()
end; -- comment

for i = 1, 10 do
end; -- comment

while true do
end; -- comment

call("hello"); -- comment
call"hello"; -- comment
call { foo = bar }; -- comment

return x; -- comment
