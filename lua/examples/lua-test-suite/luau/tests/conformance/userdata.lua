-- This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
print('testing userdata')

-- int64 is a userdata type defined in C++

-- equality
assert(int64(1) == int64(1))
assert(int64(1) ~= int64(2))

-- relational
assert(not (int64(1) < int64(1)))
assert(int64(1) < int64(2))
assert(int64(1) <= int64(1))
assert(int64(1) <= int64(2))

-- arithmetics
assert(-int64(2) == int64(-2))

assert(int64(1) + int64(2) == int64(3))
assert(int64(1) - int64(2) == int64(-1))
assert(int64(2) * int64(3) == int64(6))
assert(int64(4) / int64(2) == int64(2))
assert(int64(4) % int64(3) == int64(1))
assert(int64(2) ^ int64(3) == int64(8))

assert(int64(1) + 2 == int64(3))
assert(int64(1) - 2 == int64(-1))
assert(int64(2) * 3 == int64(6))
assert(int64(4) / 2 == int64(2))
assert(int64(4) % 3 == int64(1))
assert(int64(2) ^ 3 == int64(8))

-- tostring
assert(tostring(int64(2)) == "2")

-- index/newindex; note, mutable userdatas aren't very idiomatic but we need to test this
do
  local v = int64(42)
  assert(v.value == 42)
  v.value = 4
  assert(v.value == 4)
  assert(v == int64(4))
end

return 'OK'
