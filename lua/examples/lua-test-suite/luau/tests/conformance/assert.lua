-- This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
print("testing asserts") -- someone has to

if pcall(assert, false) or pcall(function() assert(false) end) then
  error('catastrophic assertion failure') -- surely error() can't be broken
end

function ecall(fn, ...)
  local ok, err = pcall(fn, ...)
  assert(not ok)
  return err:sub(err:find(": ") + 2, #err)
end

-- zero-ret calls work
assert(1)
assert(true)

-- returns first arg
assert(assert(1) == 1)
assert(type(assert({})) == 'table')

-- fails correctly
assert(ecall(function() assert() end) == "missing argument #1")
assert(ecall(function() assert(nil) end) == "assertion failed!")
assert(ecall(function() assert(false) end) == "assertion failed!")

-- fails with a message
assert(ecall(function() assert(nil, "epic fail") end) == "epic fail")

-- returns all arguments for multi-arg calls
assert(select('#', assert(1, 2, 3)) == 3)
assert(table.concat(table.pack(assert(1, 2, 3)), "") == "123")

return('OK')
