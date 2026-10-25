-- This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
-- This file is based on Lua 5.x tests -- https://github.com/lua/lua/tree/master/testes
print("testing bitwise operations")

assert(bit32.band() == bit32.bnot(0))
assert(bit32.btest() == true)
assert(bit32.bor() == 0)
assert(bit32.bxor() == 0)

assert(bit32.band() == bit32.band(0xffffffff))
assert(bit32.band(1,2) == 0)


-- out-of-range numbers
assert(bit32.band(-1) == 0xffffffff)
assert(bit32.band(2^33 - 1) == 0xffffffff)
assert(bit32.band(-2^33 - 1) == 0xffffffff)
assert(bit32.band(2^33 + 1) == 1)
assert(bit32.band(-2^33 + 1) == 1)
assert(bit32.band(-2^40) == 0)
assert(bit32.band(2^40) == 0)
assert(bit32.band(-2^40 - 2) == 0xfffffffe)
assert(bit32.band(2^40 - 4) == 0xfffffffc)

assert(bit32.lrotate(0, -1) == 0)
assert(bit32.lrotate(0, 7) == 0)
assert(bit32.lrotate(0x12345678, 4) == 0x23456781)
assert(bit32.rrotate(0x12345678, -4) == 0x23456781)
assert(bit32.lrotate(0x12345678, -8) == 0x78123456)
assert(bit32.rrotate(0x12345678, 8) == 0x78123456)
assert(bit32.lrotate(0xaaaaaaaa, 2) == 0xaaaaaaaa)
assert(bit32.lrotate(0xaaaaaaaa, -2) == 0xaaaaaaaa)
for i = -50, 50 do
  assert(bit32.lrotate(0x89abcdef, i) == bit32.lrotate(0x89abcdef, i%32))
end

assert(bit32.lshift(0x12345678, 4) == 0x23456780)
assert(bit32.lshift(0x12345678, 8) == 0x34567800)
assert(bit32.lshift(0x12345678, -4) == 0x01234567)
assert(bit32.lshift(0x12345678, -8) == 0x00123456)
assert(bit32.lshift(0x12345678, 32) == 0)
assert(bit32.lshift(0x12345678, -32) == 0)
assert(bit32.rshift(0x12345678, 4) == 0x01234567)
assert(bit32.rshift(0x12345678, 8) == 0x00123456)
assert(bit32.rshift(0x12345678, 32) == 0)
assert(bit32.rshift(0x12345678, -32) == 0)
assert(bit32.arshift(0x12345678, 0) == 0x12345678)
assert(bit32.arshift(0x12345678, 1) == 0x12345678 / 2)
assert(bit32.arshift(0x12345678, -1) == 0x12345678 * 2)
assert(bit32.arshift(-1, 1) == 0xffffffff)
assert(bit32.arshift(-1, 24) == 0xffffffff)
assert(bit32.arshift(-1, 32) == 0xffffffff)
assert(bit32.arshift(-1, -1) == (-1 * 2) % 2^32)

print("+")
-- some special cases
local c = {0, 1, 2, 3, 10, 0x80000000, 0xaaaaaaaa, 0x55555555,
           0xffffffff, 0x7fffffff}

for _, b in pairs(c) do
  assert(bit32.band(b) == b)
  assert(bit32.band(b, b) == b)
  assert(bit32.btest(b, b) == (b ~= 0))
  assert(bit32.band(b, b, b) == b)
  assert(bit32.btest(b, b, b) == (b ~= 0))
  assert(bit32.band(b, bit32.bnot(b)) == 0)
  assert(bit32.bor(b, bit32.bnot(b)) == bit32.bnot(0))
  assert(bit32.bor(b) == b)
  assert(bit32.bor(b, b) == b)
  assert(bit32.bor(b, b, b) == b)
  assert(bit32.bxor(b) == b)
  assert(bit32.bxor(b, b) == 0)
  assert(bit32.bxor(b, 0) == b)
  assert(bit32.bnot(b) ~= b)
  assert(bit32.bnot(bit32.bnot(b)) == b)
  assert(bit32.bnot(b) == 2^32 - 1 - b)
  assert(bit32.lrotate(b, 32) == b)
  assert(bit32.rrotate(b, 32) == b)
  assert(bit32.lshift(bit32.lshift(b, -4), 4) == bit32.band(b, bit32.bnot(0xf)))
  assert(bit32.rshift(bit32.rshift(b, 4), -4) == bit32.band(b, bit32.bnot(0xf)))
  for i = -40, 40 do
    assert(bit32.lshift(b, i) == math.floor((b * 2^i) % 2^32))
  end
end

assert(not pcall(bit32.band, {}))
assert(not pcall(bit32.bnot, "a"))
assert(not pcall(bit32.lshift, 45))
assert(not pcall(bit32.lshift, 45, print))
assert(not pcall(bit32.rshift, 45, print))

print("+")


-- testing extract/replace

assert(bit32.extract(0x12345678, 0, 4) == 8)
assert(bit32.extract(0x12345678, 4, 4) == 7)
assert(bit32.extract(0xa0001111, 28, 4) == 0xa)
assert(bit32.extract(0xa0001111, 31, 1) == 1)
assert(bit32.extract(0x50000111, 31, 1) == 0)
assert(bit32.extract(0xf2345679, 0, 32) == 0xf2345679)

assert(not pcall(bit32.extract, 0, -1))
assert(not pcall(bit32.extract, 0, 32))
assert(not pcall(bit32.extract, 0, 0, 33))
assert(not pcall(bit32.extract, 0, 31, 2))

assert(bit32.replace(0x12345678, 5, 28, 4) == 0x52345678)
assert(bit32.replace(0x12345678, 0x87654321, 0, 32) == 0x87654321)
assert(bit32.replace(0, 1, 2) == 2^2)
assert(bit32.replace(0, -1, 4) == 2^4)
assert(bit32.replace(-1, 0, 31) == 2^31 - 1)
assert(bit32.replace(-1, 0, 1, 2) == 2^32 - 7)

-- testing countlz/countrc
assert(bit32.countlz(0) == 32)
assert(bit32.countlz(42) == 26)
assert(bit32.countlz(0xffffffff) == 0)
assert(bit32.countlz(0x80000000) == 0)
assert(bit32.countlz(0x7fffffff) == 1)

assert(bit32.countrz(0) == 32)
assert(bit32.countrz(1) == 0)
assert(bit32.countrz(42) == 1)
assert(bit32.countrz(0x80000000) == 31)
assert(bit32.countrz(0x40000000) == 30)
assert(bit32.countrz(0x7fffffff) == 0)

--[[
This test verifies a fix in luauF_replace() where if the 4th
parameter was not a number, but the first three are numbers, it will
cause the Luau math library to crash.
]]--

assert(bit32.replace(-1, 0, 1, "2") == 2^32 - 7)

-- many of the tests above go through fastcall path
-- to make sure the basic implementations are also correct we test some functions with string->number coercions
assert(bit32.lrotate("0x12345678", 4) == 0x23456781)
assert(bit32.rrotate("0x12345678", -4) == 0x23456781)
assert(bit32.arshift("0x12345678", 1) == 0x12345678 / 2)
assert(bit32.arshift("-1", 32) == 0xffffffff)
assert(bit32.bnot("1") == 0xfffffffe)
assert(bit32.band("1", 3) == 1)
assert(bit32.band(1, "3") == 1)
assert(bit32.bor("1", 2) == 3)
assert(bit32.bor(1, "2") == 3)
assert(bit32.bxor("1", 3) == 2)
assert(bit32.bxor(1, "3") == 2)
assert(bit32.btest(1, "3") == true)
assert(bit32.btest("1", 3) == true)
assert(bit32.countlz("42") == 26)
assert(bit32.countrz("42") == 1)

return('OK')
