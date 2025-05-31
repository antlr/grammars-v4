-- This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
-- This file is based on Lua 5.x tests -- https://github.com/lua/lua/tree/master/testes
local pack = string.pack
local packsize = string.packsize
local unpack = string.unpack

print "testing pack/unpack"

-- maximum size for integers
local NB = 16

local sizeshort = packsize("h")
local sizeint = packsize("i")
local sizelong = packsize("l")
local sizesize_t = packsize("T")
local sizeLI = packsize("j")
local sizeMI = packsize("l")
local sizefloat = packsize("f")
local sizedouble = packsize("d")
local sizenumber = packsize("n")
local little = (pack("i2", 1) == "\1\0")
local align = packsize("!xXi16")

assert(1 <= sizeshort and sizeshort <= sizeint and sizeint <= sizelong and
       sizefloat <= sizedouble)

print("platform:")
print(string.format(
  "\tshort %d, int %d, long %d, size_t %d, float %d, double %d,\n\z
   \tlua Integer %d, lua Number %d",
   sizeshort, sizeint, sizelong, sizesize_t, sizefloat, sizedouble,
   sizeLI, sizenumber))
print("\t" .. (little and "little" or "big") .. " endian")
print("\talignment: " .. align)


-- check errors in arguments
function checkerror (msg, f, ...)
  local status, err = pcall(f, ...)
  -- print(status, err, msg)
  assert(not status and string.find(err, msg))
end

-- minimum behavior for integer formats
assert(unpack("B", pack("B", 0xff)) == 0xff)
assert(unpack("b", pack("b", 0x7f)) == 0x7f)
assert(unpack("b", pack("b", -0x80)) == -0x80)

assert(unpack("H", pack("H", 0xffff)) == 0xffff)
assert(unpack("h", pack("h", 0x7fff)) == 0x7fff)
assert(unpack("h", pack("h", -0x8000)) == -0x8000)

assert(unpack("L", pack("L", 0xffffffff)) == 0xffffffff)
assert(unpack("l", pack("l", 0x7fffffff)) == 0x7fffffff)
assert(unpack("l", pack("l", -0x80000000)) == -0x80000000)

assert(unpack("J", pack("J", 0xffffffff)) == 0xffffffff)
assert(unpack("j", pack("j", 0x7fffffff)) == 0x7fffffff)
assert(unpack("j", pack("j", -0x80000000)) == -0x80000000)

for i = 1, NB do
  -- small numbers with signal extension ("\xFF...")
  local s = string.rep("\xff", i)
  assert(pack("i" .. i, -1) == s)
  assert(packsize("i" .. i) == #s)
  assert(unpack("i" .. i, s) == -1)

  -- small unsigned number ("\0...\xAA")
  s = "\xAA" .. string.rep("\0", i - 1)
  assert(pack("<I" .. i, 0xAA) == s)
  assert(unpack("<I" .. i, s) == 0xAA)
  assert(pack(">I" .. i, 0xAA) == s:reverse())
  assert(unpack(">I" .. i, s:reverse()) == 0xAA)
end

do
  local lnum = 0x060504030201 -- 48-bit
  local s = pack("<l", lnum)
  assert(unpack("<l", s) == lnum)
  assert(unpack("<i" .. sizeMI + 1, s .. "\0") == lnum)
  assert(unpack("<i" .. sizeMI + 1, s .. "\0") == lnum)

  for i = sizeMI + 1, NB do
    local s = pack("<l", -lnum)
    assert(unpack("<l", s) == -lnum)
    -- strings with (correct) extra bytes
    assert(unpack("<i" .. i, s .. ("\xFF"):rep(i - sizeMI)) == -lnum)
    assert(unpack(">i" .. i, ("\xFF"):rep(i - sizeMI) .. s:reverse()) == -lnum)
    assert(unpack("<I" .. i, s .. ("\0"):rep(i - sizeMI)) == 2^64-lnum)

    -- overflows
    checkerror("does not fit", unpack, "<I" .. i, ("\x00"):rep(i - 1) .. "\1")
    checkerror("does not fit", unpack, ">i" .. i, "\1" .. ("\x00"):rep(i - 1))
  end
end

for i = 1, 4 do
  local lstr = "\1\2\3\4"
  local lnum = 0x04030201
  local n = bit32.band(lnum, bit32.bnot(bit32.lshift(-1, i * 8)))
  local s = string.sub(lstr, 1, i)
  assert(pack("<i" .. i, n) == s)
  assert(pack(">i" .. i, n) == s:reverse())
  assert(unpack(">i" .. i, s:reverse()) == n)
end

-- sign extension
do
  local u = 0xf0
  for i = 1, sizeLI - 1 do
    assert(unpack("<i"..i, "\xf0"..("\xff"):rep(i - 1)) == -16)
    assert(unpack(">I"..i, "\xf0"..("\xff"):rep(i - 1)) == u)
    u = u * 256 + 0xff
  end
end

-- mixed endianness
do
  assert(pack(">i2 <i2", 10, 20) == "\0\10\20\0")
  local a, b = unpack("<i2 >i2", "\10\0\0\20")
  assert(a == 10 and b == 20)
  assert(pack("=i4", 2001) == pack("i4", 2001))
end

print("testing invalid formats")

checkerror("out of limits", pack, "i0", 0)
checkerror("out of limits", pack, "i" .. NB + 1, 0)
checkerror("out of limits", pack, "!" .. NB + 1, 0)
checkerror("%(17%) out of limits %[1,16%]", pack, "Xi" .. NB + 1)
checkerror("invalid format option 'r'", pack, "i3r", 0)
checkerror("16%-byte integer", unpack, "i16", string.rep('\3', 16))
checkerror("not power of 2", pack, "!4i3", 0);
checkerror("missing size", pack, "c", "")
checkerror("variable%-length format", packsize, "s")
checkerror("variable%-length format", packsize, "z")

if packsize("i") == 4 then
  -- result would be 2^31  (2^3 repetitions of 2^28 strings)
  local s = string.rep("c268435456", 2^3)
  checkerror("too large", packsize, s)
  -- one less would be OK... except that we limit string sizes to 1GB
  s = string.rep("c268435456", 2^3 - 1) .. "c268435453"
  checkerror("too large", packsize, s)
  -- but 1GB is reachable
  assert(packsize("c1073741824") == 2^30)
end

-- overflow in packing
for i = 1, 3 do
  local umax = bit32.lshift(1, i * 8) - 1
  local max = bit32.rshift(umax, 1)
  local min = -max-1
  checkerror("overflow", pack, "<I" .. i, -1)
  checkerror("overflow", pack, "<I" .. i, min)
  checkerror("overflow", pack, ">I" .. i, umax + 1)

  checkerror("overflow", pack, ">i" .. i, umax)
  checkerror("overflow", pack, ">i" .. i, max + 1)
  checkerror("overflow", pack, "<i" .. i, min - 1)

  assert(unpack(">i" .. i, pack(">i" .. i, max)) == max)
  assert(unpack("<i" .. i, pack("<i" .. i, min)) == min)
  assert(unpack(">I" .. i, pack(">I" .. i, umax)) == umax)
end

if little then
  assert(pack("f", 24) == pack("<f", 24))
else
  assert(pack("f", 24) == pack(">f", 24))
end

print "testing pack/unpack of floating-point numbers" 

for _, n in ipairs{0, -1.1, 1.9, 1/0, -1/0, 1e20, -1e20, 0.1, 2000.7} do
    assert(unpack("n", pack("n", n)) == n)
    assert(unpack("<n", pack("<n", n)) == n)
    assert(unpack(">n", pack(">n", n)) == n)
    assert(pack("<f", n) == pack(">f", n):reverse())
    assert(pack(">d", n) == pack("<d", n):reverse())
end

-- for non-native precisions, test only with "round" numbers
for _, n in ipairs{0, -1.5, 1/0, -1/0, 1e10, -1e9, 0.5, 2000.25} do
  assert(unpack("<f", pack("<f", n)) == n)
  assert(unpack(">f", pack(">f", n)) == n)
  assert(unpack("<d", pack("<d", n)) == n)
  assert(unpack(">d", pack(">d", n)) == n)
end

print "testing pack/unpack of strings"
do
  local s = string.rep("abc", 1000)
  assert(pack("zB", s, 247) == s .. "\0\xF7")
  local s1, b = unpack("zB", s .. "\0\xF9")
  assert(b == 249 and s1 == s)
  s1 = pack("s", s)
  assert(unpack("s", s1) == s)

  checkerror("does not fit", pack, "s1", s)

  checkerror("contains zeros", pack, "z", "alo\0");

  checkerror("unfinished string", unpack, "zc10000000", "alo")

  for i = 2, NB do
    local s1 = pack("s" .. i, s)
    assert(unpack("s" .. i, s1) == s and #s1 == #s + i)
  end
end

do
  local x = pack("s", "alo")
  checkerror("too short", unpack, "s", x:sub(1, -2))
  checkerror("too short", unpack, "c5", "abcd")
  checkerror("out of limits", pack, "s100", "alo")
end

do
  assert(pack("c0", "") == "")
  assert(packsize("c0") == 0)
  assert(unpack("c0", "") == "")
  assert(pack("<! c3", "abc") == "abc")
  assert(packsize("<! c3") == 3)
  assert(pack(">!4 c6", "abcdef") == "abcdef")
  assert(pack("c3", "123") == "123")
  assert(pack("c0", "") == "")
  assert(pack("c8", "123456") == "123456\0\0")
  assert(pack("c88", "") == string.rep("\0", 88))
  assert(pack("c188", "ab") == "ab" .. string.rep("\0", 188 - 2))
  local a, b, c = unpack("!4 z c3", "abcdefghi\0xyz")
  assert(a == "abcdefghi" and b == "xyz" and c == 14)
  checkerror("longer than", pack, "c3", "1234")
end


-- testing multiple types and sequence
do
  local x = pack("<b h b f d f n i", 1, 2, 3, 4, 5, 6, 7, 8)
  assert(#x == packsize("<b h b f d f n i"))
  local a, b, c, d, e, f, g, h = unpack("<b h b f d f n i", x)
  assert(a == 1 and b == 2 and c == 3 and d == 4 and e == 5 and f == 6 and
         g == 7 and h == 8) 
end

print "testing alignment"
do
  assert(pack(" < i1 i2 ", 2, 3) == "\2\3\0")   -- no alignment by default
  local x = pack(">!8 b Xh i4 i8 c1 Xi8", -12, 100, 200, "\xEC")
  assert(#x == packsize(">!8 b Xh i4 i8 c1 Xi8"))
  assert(x == "\xf4" .. "\0\0\0" ..
              "\0\0\0\100" ..
              "\0\0\0\0\0\0\0\xC8" .. 
              "\xEC" .. "\0\0\0\0\0\0\0")
  local a, b, c, d, pos = unpack(">!8 c1 Xh i4 i8 b Xi8 XI XH", x)
  assert(a == "\xF4" and b == 100 and c == 200 and d == -20 and (pos - 1) == #x)

  x = pack(">!4 c3 c4 c2 z i4 c5 c2 Xi4",
                  "abc", "abcd", "xz", "hello", 5, "world", "xy")
  assert(x == "abcabcdxzhello\0\0\0\0\0\5worldxy\0")
  local a, b, c, d, e, f, g, pos = unpack(">!4 c3 c4 c2 z i4 c5 c2 Xh Xi4", x)
  assert(a == "abc" and b == "abcd" and c == "xz" and d == "hello" and
         e == 5 and f == "world" and g == "xy" and (pos - 1) % 4 == 0)

  x = pack(" b b Xd b Xb x", 1, 2, 3)
  assert(packsize(" b b Xd b Xb x") == 4)
  assert(x == "\1\2\3\0")
  a, b, c, pos = unpack("bbXdb", x)
  assert(a == 1 and b == 2 and c == 3 and pos == #x)

  -- only alignment
  assert(packsize("!8 xXi8") == 8)
  local pos = unpack("!8 xXi8", "0123456701234567"); assert(pos == 9)
  assert(packsize("!8 xXi2") == 2)
  local pos = unpack("!8 xXi2", "0123456701234567"); assert(pos == 3)
  assert(packsize("!2 xXi2") == 2)
  local pos = unpack("!2 xXi2", "0123456701234567"); assert(pos == 3)
  assert(packsize("!2 xXi8") == 2)
  local pos = unpack("!2 xXi8", "0123456701234567"); assert(pos == 3)
  assert(packsize("!16 xXi16") == 16)
  local pos = unpack("!16 xXi16", "0123456701234567"); assert(pos == 17)

  checkerror("invalid next option", pack, "X")
  checkerror("invalid next option", unpack, "XXi", "")
  checkerror("invalid next option", unpack, "X i", "")
  checkerror("invalid next option", pack, "Xc1")
end

do    -- testing initial position
  local x = pack("i4i4i4i4", 1, 2, 3, 4)
  for pos = 1, 16, 4 do
    local i, p = unpack("i4", x, pos)
    assert(i == math.floor(pos/4) + 1 and p == pos + 4)
  end

  -- with alignment
  for pos = 0, 12 do    -- will always round position to power of 2
    local i, p = unpack("!4 i4", x, pos + 1)
    assert(i == math.floor((pos + 3)/4) + 1 and p == i*4 + 1)
  end

  -- negative indices
  local i, p = unpack("!4 i4", x, -4)
  assert(i == 4 and p == 17)
  local i, p = unpack("!4 i4", x, -7)
  assert(i == 4 and p == 17)
  local i, p = unpack("!4 i4", x, -#x)
  assert(i == 1 and p == 5)

  -- limits
  for i = 1, #x + 1 do
    assert(unpack("c0", x, i) == "")
  end
  checkerror("out of string", unpack, "c0", x, #x + 2)
 
end

do    -- testing out of range values
  checkerror("out of limits", unpack, "i17", "")
  checkerror("out of limits", unpack, "i987654321", "")
  checkerror("too large", unpack, "i9876543210", "")
  checkerror("too large", unpack, "c9876543210", "")
  checkerror("too large", packsize, "c1" .. string.rep("0", 40))
  checkerror("missing size", unpack, "c-2", "")
end

return "OK"
