--[[lit-meta
  name = "luvit/base64"
  description = "A pure lua implemention of base64 using bitop"
  tags = {"crypto", "base64", "bitop"}
  version = "2.0.0"
  license = "MIT"
  author = { name = "Tim Caswell" }
]]

local bit = require 'bit'
local rshift = bit.rshift
local lshift = bit.lshift
local bor = bit.bor
local band = bit.band
local char = string.char
local byte = string.byte
local concat = table.concat
local codes = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/='

-- Loop over input 3 bytes at a time
-- a,b,c are 3 x 8-bit numbers
-- they are encoded into groups of 4 x 6-bit numbers
-- aaaaaa aabbbb bbbbcc cccccc
-- if there is no c, then pad the 4th with =
-- if there is also no b then pad the 3rd with =
local function base64Encode(str)
  local parts = {}
  local j = 1
  for i = 1, #str, 3 do
    local a, b, c = byte(str, i, i + 2)
    parts[j] = char(
      -- Higher 6 bits of a
      byte(codes, rshift(a, 2) + 1),
      -- Lower 2 bits of a + high 4 bits of b
      byte(codes, bor(
        lshift(band(a, 3), 4),
        b and rshift(b, 4) or 0
      ) + 1),
      -- Low 4 bits of b + High 2 bits of c
      b and byte(codes, bor(
        lshift(band(b, 15), 2),
        c and rshift(c, 6) or 0
      ) + 1) or 61, -- 61 is '='
      -- Lower 6 bits of c
      c and byte(codes, band(c, 63) + 1) or 61 -- 61 is '='
    )
    j = j + 1
  end
  return concat(parts)
end

-- Reverse map from character code to 6-bit integer
local map = {}
for i = 1, #codes do
  map[byte(codes, i)] = i - 1
end

-- loop over input 4 characters at a time
-- The characters are mapped to 4 x 6-bit integers a,b,c,d
-- They need to be reassalbled into 3 x 8-bit bytes
-- aaaaaabb bbbbcccc ccdddddd
-- if d is padding then there is no 3rd byte
-- if c is padding then there is no 2nd byte
local function base64Decode(data)
  local bytes = {}
  local j = 1
  for i = 1, #data, 4 do
    local a = map[byte(data, i)]
    local b = map[byte(data, i + 1)]
    local c = map[byte(data, i + 2)]
    local d = map[byte(data, i + 3)]

    -- higher 6 bits are the first char
    -- lower 2 bits are upper 2 bits of second char
    bytes[j] = char(bor(lshift(a, 2), rshift(b, 4)))

    -- if the third char is not padding, we have a second byte
    if c < 64 then
      -- high 4 bits come from lower 4 bits in b
      -- low 4 bits come from high 4 bits in c
      bytes[j + 1] = char(bor(lshift(band(b, 0xf), 4), rshift(c, 2)))

      -- if the fourth char is not padding, we have a third byte
      if d < 64 then
        -- Upper 2 bits come from Lower 2 bits of c
        -- Lower 6 bits come from d
        bytes[j + 2] = char(bor(lshift(band(c, 3), 6), d))
      end
    end
    j = j + 3
  end
  return concat(bytes)
end

assert(base64Encode("") == "")
assert(base64Encode("f") == "Zg==")
assert(base64Encode("fo") == "Zm8=")
assert(base64Encode("foo") == "Zm9v")
assert(base64Encode("foob") == "Zm9vYg==")
assert(base64Encode("fooba") == "Zm9vYmE=")
assert(base64Encode("foobar") == "Zm9vYmFy")

assert(base64Decode("") == "")
assert(base64Decode("Zg==") == "f")
assert(base64Decode("Zm8=") == "fo")
assert(base64Decode("Zm9v") == "foo")
assert(base64Decode("Zm9vYg==") == "foob")
assert(base64Decode("Zm9vYmE=") == "fooba")
assert(base64Decode("Zm9vYmFy") == "foobar")

return {
  encode = base64Encode,
  decode = base64Decode,
}
