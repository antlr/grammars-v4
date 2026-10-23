--[[

Copyright 2014-2015 The Luvit Authors. All Rights Reserved.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS-IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

--]]

local hex = require('openssl').hex
local deflate = require('miniz').deflate
local inflate = require('miniz').inflate

local byte = string.byte
local sub = string.sub

-- ERROR - '\0' error
-- MESSAGE - COMMAND data
local function decodeText(message)
  if byte(message, 1) == 0 then
    return "error", sub(message, 2)
  end
  local name, data = string.match(message, "^([^ ]+) *(.*)$")
  assert(name, "Invalid message")
  return name, #data > 0 and data or nil
end

-- WANTS - 0x00 len (20 bytes) * len
-- SEND - raw deflated data
local function decodeBinary(message)
  if #message >= 2 and byte(message, 1) == 0 then
    local wants = {}
    for i = 1, byte(message, 2) do
      local start = i * 20 - 17
      wants[i] = hex(sub(message, start, start + 19), true)
    end
    return "wants", wants
  end
  local inflated = inflate(message, 1)
  if #inflated == 0 then
    p {
      message = message,
      inflated = inflated,
    }
    error("Inflate error")
  end
  return "send", inflate(message, 1)
end

local encoders = {}

-- WANTS -  0x00 len (20 bytes) * len
function encoders.wants(hashes)
  assert(#hashes > 0, "Can't sent empty wants list")
  local data = {}
  for i = 1, #hashes do
    data[i] = hex(hashes[i], false)
  end
  return {
    opcode = 2,
    payload = string.char(0, #hashes) .. table.concat(data),
  }
end

-- SEND - raw deflated data
function encoders.send(data)
  -- TDEFL_WRITE_ZLIB_HEADER             = 0x01000,
  -- 4095=Huffman+LZ (slowest/best compression)
  return {
    opcode = 2,
    payload = deflate(data, 0x01000 + 4095)
  }
end

-- ERROR - '\0' error
function encoders.error(message)
  return {
    opcode = 1,
    payload = "\0" .. message
  }
end

-- MESSAGE - COMMAND data
local function encode(name, data)
  local encoder = encoders[name]
  if encoder then return encoder(data) end
  local payload = data and (name .. ' ' .. data) or name
  return {
    opcode = 1,
    payload = payload
  }
end

local function makeRemote(webRead, webWrite, isClient)

  -- read
  local function innerRead()
    while true do
      local frame = webRead()
      -- p("INPUT", frame)
      if not frame then return end
      assert(isClient or frame.mask, "all frames sent by client must be masked")
      if frame.opcode == 1 then
        return decodeText(frame.payload)
      elseif frame.opcode == 2 then
        return decodeBinary(frame.payload)
      end
    end
  end

  local function read()
    local name, data = innerRead()
    -- p("network read", name, data and (#data <= 60 and data or # data))
    assert(name ~= "error", data)
    return name, data
  end

  local function readAs(expectedName)
    local name, data = read()
    assert(expectedName == name, name
      and ("Expected " .. expectedName .. ", but found " .. name)
      or ("Disconnected while waiting for " .. expectedName))
    return data
  end

  local function writeAs(name, data)
    -- p("network write", name, data and (#data <= 60 and data or #data))
    if not name then return webWrite() end
    local frame = encode(name, data)
    frame.mask = isClient
    -- p("OUTPUT", frame)
    return webWrite(frame)
  end

  local function close()
    return webWrite()
  end

  return {
    read = read,
    readAs = readAs,
    writeAs = writeAs,
    close = close,
  }
end

return {
  decodeText = decodeText,
  decodeBinary = decodeBinary,
  encoders = encoders,
  makeRemote = makeRemote,
}
