-- Unused
local localA, localB

-- Used
local localC = 1
print(localC)

-- Mutated, but never read
local localD = 1
localD = 2
localD = 3

-- Read, mutated, read
local localE = 1
print(localE)
localE = 2
print(localE)

-- Read, mutated, unread
local localF = 1
print(localF)
localF = 2

-- Called function
local localG = function() end
localG()

-- Put into a table
local localH = 1
local localI = { localH }
