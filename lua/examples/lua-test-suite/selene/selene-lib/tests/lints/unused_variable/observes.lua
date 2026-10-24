local writtenOnlyA = {}
table.insert(writtenOnlyA, 3)

local readA = {}
print(table.insert(readA, 3))

-- Make sure doing it inside other statements doesn't trigger false negatives
print(function()
	local writtenOnlyB = {}
	table.insert(writtenOnlyB, 1)

	local readB = {}
	return table.insert(readB, 1)
end)

if true then
	local writtenOnlyC = {}
	table.insert(writtenOnlyC, 1)

	local readC = {}
	print(table.insert(readC, 1))
end

local insertButReadLater = {}
table.insert(insertButReadLater, 1)
print(insertButReadLater)

local ambiguous = external()
table.insert(ambiguous, 1)

local rooms = {
	boss = {},
}

for index, room in pairs(rooms) do
	table.insert(room, 1)

	local roomByIndex = rooms[index]
	table.insert(roomByIndex, 2)
end

local nested = {
	key = external(),
}

table.insert(nested.key, 1)
