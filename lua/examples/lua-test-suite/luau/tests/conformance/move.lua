-- This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
-- This file is based on Lua 5.x tests -- https://github.com/lua/lua/tree/master/testes
local function checkerror (msg, f, ...)
  local s, err = pcall(f, ...)
  assert(not s and string.find(err, msg))
end

print("testing move")

local maxI = 2147483647
local minI = -2147483648

-- testing move
do

  checkerror("table expected", table.move, 1, 2, 3, 4)
  checkerror("table expected", table.move, {}, 2, 3, 4, "foo")

  local function eqT (a, b)
    for k, v in pairs(a) do assert(b[k] == v) end 
    for k, v in pairs(b) do assert(a[k] == v) end 
  end

  local a = table.move({10,20,30}, 1, 3, 2)  -- move forward
  eqT(a, {10,10,20,30})

  -- move forward with overlap of 1
  a = table.move({10, 20, 30}, 1, 3, 3)
  eqT(a, {10, 20, 10, 20, 30})

  -- moving to the same table (not being explicit about it)
  a = {10, 20, 30, 40}
  table.move(a, 1, 4, 2, a)
  eqT(a, {10, 10, 20, 30, 40})

  a = table.move({10,20,30}, 2, 3, 1)   -- move backward
  eqT(a, {20,30,30})

  a = {}   -- move to new table
  assert(table.move({10,20,30}, 1, 3, 1, a) == a)
  eqT(a, {10,20,30})

  a = {}
  assert(table.move({10,20,30}, 1, 0, 3, a) == a)  -- empty move (no move)
  eqT(a, {})

  a = table.move({10,20,30}, 1, 10, 1)   -- move to the same place
  eqT(a, {10,20,30})

  -- moving on the fringes
  a = table.move({[maxI - 2] = 1, [maxI - 1] = 2, [maxI] = 3},
                 maxI - 2, maxI, -10, {})
  eqT(a, {[-10] = 1, [-9] = 2, [-8] = 3})

  a = table.move({[minI] = 1, [minI + 1] = 2, [minI + 2] = 3},
                 minI, minI + 2, -10, {})
  eqT(a, {[-10] = 1, [-9] = 2, [-8] = 3})

  a = table.move({45}, 1, 1, maxI)
  eqT(a, {45, [maxI] = 45})

  a = table.move({[maxI] = 100}, maxI, maxI, minI)
  eqT(a, {[minI] = 100, [maxI] = 100})

  a = table.move({[minI] = 100}, minI, minI, maxI)
  eqT(a, {[minI] = 100, [maxI] = 100})
end

checkerror("too many", table.move, {}, 0, maxI, 1)
checkerror("too many", table.move, {}, -1, maxI - 1, 1)
checkerror("too many", table.move, {}, minI, -1, 1)
checkerror("too many", table.move, {}, minI, maxI, 1)
checkerror("wrap around", table.move, {}, 1, maxI, 2)
checkerror("wrap around", table.move, {}, 1, 2, maxI)
checkerror("wrap around", table.move, {}, minI, -2, 2)

return"OK"
