--[[lit-meta
  name = "luvit/sha1"
  version = "1.0.4"
  homepage = "https://github.com/luvit/lit/blob/master/deps/sha1.lua"
  description = "Pure Lua implementation of SHA1 using bitop"
  authors = {
    "Tim Caswell"
  }
]]

-- http://csrc.nist.gov/groups/ST/toolkit/documents/Examples/SHA_All.pdf

local bit = require('bit')
local band = bit.band
local bor = bit.bor
local bxor = bit.bxor
local lshift = bit.lshift
local rshift = bit.rshift
local rol = bit.rol
local tobit = bit.tobit
local tohex = bit.tohex

local byte = string.byte
local concat = table.concat
local floor = math.floor

local hasFFi, ffi = pcall(require, "ffi")
local newBlock = hasFFi and function ()
  return ffi.new("uint32_t[80]")
end or function ()
  local t = {}
  for i = 0, 79 do
    t[i] = 0
  end
  return t
end

local shared = newBlock()

local function unsigned(n)
  return n < 0 and (n + 0x100000000) or n
end

local function create(sync)
  local h0 = 0x67452301
  local h1 = 0xEFCDAB89
  local h2 = 0x98BADCFE
  local h3 = 0x10325476
  local h4 = 0xC3D2E1F0
  -- The first 64 bytes (16 words) is the data chunk
  local W = sync and shared or newBlock()
  local offset = 0
  local shift = 24
  local totalLength = 0

  local update, write, processBlock, digest

  -- The user gave us more data.  Store it!
  function update(chunk)
    local length = #chunk
    totalLength = totalLength + length * 8
    for i = 1, length do
      write(byte(chunk, i))
    end
  end

  function write(data)
    W[offset] = bor(W[offset], lshift(band(data, 0xff), shift))
    if shift > 0 then
      shift = shift - 8
    else
      offset = offset + 1
      shift = 24
    end
    if offset == 16 then
      return processBlock()
    end
  end

  -- No more data will come, pad the block, process and return the result.
  function digest()
    -- Pad
    write(0x80)
    if offset > 14 or (offset == 14 and shift < 24) then
      processBlock()
    end
    offset = 14
    shift = 24

    -- 64-bit length big-endian
    write(0x00) -- numbers this big aren't accurate in lua anyway
    write(0x00) -- ..So just hard-code to zero.
    write(totalLength > 0xffffffffff and floor(totalLength / 0x10000000000) or 0x00)
    write(totalLength > 0xffffffff and floor(totalLength / 0x100000000) or 0x00)
    for s = 24, 0, -8 do
      write(rshift(totalLength, s))
    end

    -- At this point one last processBlock() should trigger and we can pull out the result.
    return concat {
      tohex(h0),
      tohex(h1),
      tohex(h2),
      tohex(h3),
      tohex(h4)
    }
  end

  -- We have a full block to process.  Let's do it!
  function processBlock()

    -- Extend the sixteen 32-bit words into eighty 32-bit words:
    for i = 16, 79, 1 do
      W[i] =
        rol(bxor(W[i - 3], W[i - 8], W[i - 14], W[i - 16]), 1)
    end

    -- print("Block Contents:")
    -- for i = 0, 15 do
    --   print(string.format("  W[%d] = %s", i, tohex(W[i])))
    -- end
    -- print()

    -- Initialize hash value for this chunk:
    local a = h0
    local b = h1
    local c = h2
    local d = h3
    local e = h4
    local f, k

    -- print("         A         B         C         D         E")
    -- local format =
    --   "t=%02d: %s  %s  %s  %s  %s"
    -- Main loop:
    for t = 0, 79 do
      if t < 20 then
        f = bxor(d, band(b, bxor(c, d)))
        k = 0x5A827999
      elseif t < 40 then
        f = bxor(b, c, d)
        k = 0x6ED9EBA1
      elseif t < 60 then
        f = bor(band(b, c), (band(d, bor(b, c))))
        k = 0x8F1BBCDC
      else
        f = bxor(b, c, d)
        k = 0xCA62C1D6
      end
      e, d, c, b, a =
        d,
        c,
        rol(b, 30),
        a,
        tobit(
          unsigned(rol(a, 5)) +
          unsigned(f) +
          unsigned(e) +
          unsigned(k) +
          W[t]
        )
      -- print(string.format(format, t, tohex(a), tohex(b), tohex(c), tohex(d), tohex(e)))
    end

    -- Add this chunk's hash to result so far:
    h0 = tobit(unsigned(h0) + a)
    h1 = tobit(unsigned(h1) + b)
    h2 = tobit(unsigned(h2) + c)
    h3 = tobit(unsigned(h3) + d)
    h4 = tobit(unsigned(h4) + e)

    -- The block is now reusable.
    offset = 0
    for i = 0, 15 do
      W[i] = 0
    end
  end

  return {
    update = update,
    digest = digest
  }

end

return function (buffer)
  -- Pass in false or nil to get a streaming interface.
  if not buffer then
    return create(false)
  end
  local shasum = create(true)
  shasum.update(buffer)
  return shasum.digest()
end
