if
	this -- foobar
then
elseif
	foobar or code == 10 -- \n
then
	pos = 1
	lexer.line = 1
	lexer.lineStart = pos - 1
end

local function coerceToMap(mapLike)
	return instanceOf(mapLike, Map) and mapLike -- ROBLOX: order is preservered
		or Map.new(Object.entries(mapLike)) -- ROBLOX: order is not preserved
end

if -- comment
	foo
then
end

if
	foo
	-- comment
then
end

while -- commend
	foo
do
end

while
	foo
	-- comment
do
end

do
	return foo -- comment
		or bar, -- comment
		baz and foo
end

local x = foo -- comment
		or bar, -- comment
		baz and foo
