--[[lit-meta
  name = "luvit/websocket-codec"
  description = "A codec implementing websocket framing and helpers for handshakeing"
  version = "3.0.3"
  dependencies = {
    "luvit/base64@2.0.0",
    "luvit/sha1@1.0.0",
  }
  homepage = "https://github.com/luvit/lit/blob/master/deps/websocket-codec.lua"
  tags = {"http", "websocket", "codec"}
  license = "MIT"
  author = { name = "Tim Caswell" }
]]

local base64 = require('base64').encode
local sha1 = require('sha1')
local bit = require('bit')

local band = bit.band
local bor = bit.bor
local bxor = bit.bxor
local rshift = bit.rshift
local lshift = bit.lshift
local char = string.char
local byte = string.byte
local sub = string.sub
local gmatch = string.gmatch
local lower = string.lower
local gsub = string.gsub
local concat = table.concat
local floor = math.floor
local random = math.random
local unpack = unpack or table.unpack

local function rand4()
  -- Generate 32 bits of pseudo random data
  local num = floor(random() * 0x100000000)
  -- Return as a 4-byte string
  return char(
    rshift(num, 24),
    band(rshift(num, 16), 0xff),
    band(rshift(num, 8), 0xff),
    band(num, 0xff)
  )
end

local function applyMask(data, mask)
  local bytes = {
    [0] = byte(mask, 1),
    [1] = byte(mask, 2),
    [2] = byte(mask, 3),
    [3] = byte(mask, 4)
  }
  local out = {}
  for i = 1, #data do
    out[i] = char(
      bxor(byte(data, i), bytes[(i - 1) % 4])
    )
  end
  return concat(out)
end

local function decode(chunk, index)
  local start = index - 1
  local length = #chunk - start
  if length < 2 then return end
  local second = byte(chunk, start + 2)
  local len = band(second, 0x7f)
  local offset
  if len == 126 then
    if length < 4 then return end
    len = bor(
      lshift(byte(chunk, start + 3), 8),
      byte(chunk, start + 4))
    offset = 4
  elseif len == 127 then
    if length < 10 then return end
    len = bor(
      lshift(byte(chunk, start + 3), 24),
      lshift(byte(chunk, start + 4), 16),
      lshift(byte(chunk, start + 5), 8),
      byte(chunk, start + 6)
    ) * 0x100000000 + bor(
      lshift(byte(chunk, start + 7), 24),
      lshift(byte(chunk, start + 8), 16),
      lshift(byte(chunk, start + 9), 8),
      byte(chunk, start + 10)
    )
    offset = 10
  else
    offset = 2
  end
  local mask = band(second, 0x80) > 0
  if mask then
    offset = offset + 4
  end
  offset = offset + start
  if #chunk < offset + len then return end

  local first = byte(chunk, start + 1)
  local payload = sub(chunk, offset + 1, offset + len)
  assert(#payload == len, "Length mismatch")
  if mask then
    payload = applyMask(payload, sub(chunk, offset - 3, offset))
  end
  return {
    fin = band(first, 0x80) > 0,
    rsv1 = band(first, 0x40) > 0,
    rsv2 = band(first, 0x20) > 0,
    rsv3 = band(first, 0x10) > 0,
    opcode = band(first, 0xf),
    mask = mask,
    len = len,
    payload = payload
  }, offset + len + 1
end

local function encode(item)
  if type(item) == "string" then
    item = {
      opcode = 2,
      payload = item
    }
  end
  local payload = item.payload
  assert(type(payload) == "string", "payload must be string")
  local len = #payload
  local fin = item.fin
  if fin == nil then fin = true end
  local rsv1 = item.rsv1
  local rsv2 = item.rsv2
  local rsv3 = item.rsv3
  local opcode = item.opcode or 2
  local mask = item.mask
  local chars = {
    char(bor(
      fin and 0x80 or 0,
      rsv1 and 0x40 or 0,
      rsv2 and 0x20 or 0,
      rsv3 and 0x10 or 0,
      opcode
    )),
    char(bor(
      mask and 0x80 or 0,
      len < 126 and len or (len < 0x10000) and 126 or 127
    ))
  }
  if len >= 0x10000 then
    local high = len / 0x100000000
    chars[3] = char(band(rshift(high, 24), 0xff))
    chars[4] = char(band(rshift(high, 16), 0xff))
    chars[5] = char(band(rshift(high, 8), 0xff))
    chars[6] = char(band(high, 0xff))
    chars[7] = char(band(rshift(len, 24), 0xff))
    chars[8] = char(band(rshift(len, 16), 0xff))
    chars[9] = char(band(rshift(len, 8), 0xff))
    chars[10] = char(band(len, 0xff))
  elseif len >= 126 then
    chars[3] = char(band(rshift(len, 8), 0xff))
    chars[4] = char(band(len, 0xff))
  end
  if mask then
    local key = rand4()
    return concat(chars) .. key .. applyMask(payload, key)
  end
  return concat(chars) .. payload
end

local websocketGuid = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"

-- Given two hex characters, return a single character
local function hexToBin(cc)
  return string.char(tonumber(cc, 16))
end

local function decodeHex(hex)
  local bin = string.gsub(hex, "..", hexToBin)
  return bin
end

local function acceptKey(key)
  return gsub(base64(decodeHex(sha1(key .. websocketGuid))), "\n", "")
end

-- Make a client handshake connection
local function handshake(options, request)
  -- Generate 16 bytes of pseudo-random data
  local key = concat({rand4(), rand4(), rand4(), rand4()})
  key = base64(key)
  local host = options.host
  local path = options.path or "/"
  local protocol = options.protocol
  local req = {
    method = "GET",
    path = path,
    {"Connection", "Upgrade"},
    {"Upgrade", "websocket"},
    {"Sec-WebSocket-Version", "13"},
    {"Sec-WebSocket-Key", key},
  }
  for i = 1, #options do
    req[#req + 1] = options[i]
  end
  if host then
    req[#req + 1] = {"Host", host}
  end
  if protocol then
    req[#req + 1] = {"Sec-WebSocket-Protocol", protocol}
  end
  local res = request(req)
  if not res then
    return nil, "Missing response from server"
  end
  -- Parse the headers for quick reading
  if res.code ~= 101 then
    return nil, "response must be code 101"
  end

  local headers = {}
  for i = 1, #res do
    local name, value = unpack(res[i])
    headers[lower(name)] = value
  end

  if not headers.connection or lower(headers.connection) ~= "upgrade" then
    return nil, "Invalid or missing connection upgrade header in response"
  end
  if headers["sec-websocket-accept"] ~= acceptKey(key) then
    return nil, "challenge key missing or mismatched"
  end
  if protocol and headers["sec-websocket-protocol"] ~= protocol then
    return nil, "protocol missing or mistmatched"
  end
  return true
end

local function handleHandshake(head, protocol)

  -- WebSocket connections must be GET requests
  if not head.method == "GET" then return end

  -- Parse the headers for quick reading
  local headers = {}
  for i = 1, #head do
    local name, value = unpack(head[i])
    headers[lower(name)] = value
  end

  -- Must have 'Upgrade: websocket' and 'Connection: Upgrade' headers
  if not (headers.connection and headers.upgrade and
          headers.connection:lower():find("upgrade", 1, true) and
          headers.upgrade:lower():find("websocket", 1, true)) then return end

  -- Make sure it's a new client speaking v13 of the protocol
  if tonumber(headers["sec-websocket-version"]) < 13 then
    return nil, "only websocket protocol v13 supported"
  end

  local key = headers["sec-websocket-key"]
  if not key then
    return nil, "websocket security key missing"
  end

  -- If the server wants a specified protocol, check for it.
  if protocol then
    local foundProtocol = false
    local list = headers["sec-websocket-protocol"]
    if list then
      for item in gmatch(list, "[^, ]+") do
        if item == protocol then
          foundProtocol = true
          break
        end
      end
    end
    if not foundProtocol then
      return nil, "specified protocol missing in request"
    end
  end

  local accept = acceptKey(key)

  local res = {
    code = 101,
    {"Upgrade", "websocket"},
    {"Connection", "Upgrade"},
    {"Sec-WebSocket-Accept", accept},
  }
  if protocol then
    res[#res + 1] = {"Sec-WebSocket-Protocol", protocol}
  end

  return res
end

return {
  decode = decode,
  encode = encode,
  acceptKey = acceptKey,
  handshake = handshake,
  handleHandshake = handleHandshake,
}
