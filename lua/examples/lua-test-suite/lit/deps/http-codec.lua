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

--[[lit-meta
  name = "luvit/http-codec"
  version = "3.0.8"
  homepage = "https://github.com/luvit/luvit/blob/master/deps/http-codec.lua"
  description = "A simple pair of functions for converting between hex and raw strings."
  tags = {"codec", "http"}
  license = "Apache 2"
  author = { name = "Tim Caswell" }
]]

local sub = string.sub
local gsub = string.gsub
local lower = string.lower
local find = string.find
local format = string.format
local concat = table.concat
local match = string.match
local unpack = unpack or table.unpack

local STATUS_CODES = {
  [100] = 'Continue',
  [101] = 'Switching Protocols',
  [102] = 'Processing',                 -- RFC 2518, obsoleted by RFC 4918
  [200] = 'OK',
  [201] = 'Created',
  [202] = 'Accepted',
  [203] = 'Non-Authoritative Information',
  [204] = 'No Content',
  [205] = 'Reset Content',
  [206] = 'Partial Content',
  [207] = 'Multi-Status',               -- RFC 4918
  [300] = 'Multiple Choices',
  [301] = 'Moved Permanently',
  [302] = 'Moved Temporarily',
  [303] = 'See Other',
  [304] = 'Not Modified',
  [305] = 'Use Proxy',
  [307] = 'Temporary Redirect',
  [400] = 'Bad Request',
  [401] = 'Unauthorized',
  [402] = 'Payment Required',
  [403] = 'Forbidden',
  [404] = 'Not Found',
  [405] = 'Method Not Allowed',
  [406] = 'Not Acceptable',
  [407] = 'Proxy Authentication Required',
  [408] = 'Request Time-out',
  [409] = 'Conflict',
  [410] = 'Gone',
  [411] = 'Length Required',
  [412] = 'Precondition Failed',
  [413] = 'Request Entity Too Large',
  [414] = 'Request-URI Too Large',
  [415] = 'Unsupported Media Type',
  [416] = 'Requested Range Not Satisfiable',
  [417] = 'Expectation Failed',
  [418] = "I'm a teapot",                       -- RFC 2324
  [422] = 'Unprocessable Entity',               -- RFC 4918
  [423] = 'Locked',                             -- RFC 4918
  [424] = 'Failed Dependency',                  -- RFC 4918
  [425] = 'Unordered Collection',               -- RFC 4918
  [426] = 'Upgrade Required',                   -- RFC 2817
  [428] = 'Precondition Required',              -- RFC 6585
  [429] = 'Too Many Requests',                  -- RFC 6585
  [431] = 'Request Header Fields Too Large',    -- RFC 6585
  [500] = 'Internal Server Error',
  [501] = 'Not Implemented',
  [502] = 'Bad Gateway',
  [503] = 'Service Unavailable',
  [504] = 'Gateway Time-out',
  [505] = 'HTTP Version not supported',
  [506] = 'Variant Also Negotiates',            -- RFC 2295
  [507] = 'Insufficient Storage',               -- RFC 4918
  [509] = 'Bandwidth Limit Exceeded',
  [510] = 'Not Extended',                       -- RFC 2774
  [511] = 'Network Authentication Required'     -- RFC 6585
}

local function encoder()

  local mode
  local encodeHead, encodeRaw, encodeChunked

  function encodeHead(item)
    if not item or item == "" then
      return item
    elseif not (type(item) == "table") then
      error("expected a table but got a " .. type(item) .. " when encoding data")
    end
    local head, chunkedEncoding
    local version = item.version or 1.1
    if item.method then
      local path = item.path
      assert(path and #path > 0, "expected non-empty path")
      head = { item.method .. ' ' .. item.path .. ' HTTP/' .. version .. '\r\n' }
    else
      local reason = item.reason or STATUS_CODES[item.code]
      head = { 'HTTP/' .. version .. ' ' .. item.code .. ' ' .. reason .. '\r\n' }
    end
    for i = 1, #item do
      local key, value = unpack(item[i])
      local lowerKey = lower(key)
      if lowerKey == "transfer-encoding" then
        chunkedEncoding = lower(value) == "chunked"
      end
      value = gsub(tostring(value), "[\r\n]+", " ")
      head[#head + 1] = key .. ': ' .. tostring(value) .. '\r\n'
    end
    head[#head + 1] = '\r\n'

    mode = chunkedEncoding and encodeChunked or encodeRaw
    return concat(head)
  end

  function encodeRaw(item)
    if type(item) ~= "string" then
      mode = encodeHead
      return encodeHead(item)
    end
    return item
  end

  function encodeChunked(item)
    if type(item) ~= "string" then
      mode = encodeHead
      local extra = encodeHead(item)
      if extra then
        return "0\r\n\r\n" .. extra
      else
        return "0\r\n\r\n"
      end
    end
    if #item == 0 then
      mode = encodeHead
    end
    return format("%x", #item) .. "\r\n" .. item .. "\r\n"
  end

  mode = encodeHead
  return function (item)
    return mode(item)
  end
end

local function decoder()

  -- This decoder is somewhat stateful with 5 different parsing states.
  local decodeHead, decodeEmpty, decodeRaw, decodeChunked, decodeCounted
  local mode -- state variable that points to various decoders
  local bytesLeft -- For counted decoder

  -- This state is for decoding the status line and headers.
  function decodeHead(chunk, index)
    if not chunk or index > #chunk then return end

    local _, last = find(chunk, "\r?\n\r?\n", index)
    -- First make sure we have all the head before continuing
    if not last then
      if (#chunk - index) <= 8 * 1024 then return end
      -- But protect against evil clients by refusing heads over 8K long.
      error("entity too large")
    end

    -- Parse the status/request line
    local head = {}
    local _, offset
    local version
    _, offset, version, head.code, head.reason =
      find(chunk, "^HTTP/(%d%.%d) (%d+) ([^\r\n]*)\r?\n", index)
    if offset then
      head.code = tonumber(head.code)
    else
      _, offset, head.method, head.path, version =
        find(chunk, "^(%u+) ([^ ]+) HTTP/(%d%.%d)\r?\n", index)
      if not offset then
        error("expected HTTP data")
      end
    end
    version = tonumber(version)
    head.version = version
    head.keepAlive = version > 1.0

    -- We need to inspect some headers to know how to parse the body.
    local contentLength
    local chunkedEncoding

    -- Parse the header lines
    while true do
      local key, value
      _, offset, key, value = find(chunk, "^([^:\r\n]+): *([^\r\n]*)\r?\n", offset + 1)
      if not offset then break end
      local lowerKey = lower(key)

      -- Inspect a few headers and remember the values
      if lowerKey == "content-length" then
        contentLength = tonumber(value)
      elseif lowerKey == "transfer-encoding" then
        chunkedEncoding = lower(value) == "chunked"
      elseif lowerKey == "connection" then
        head.keepAlive = lower(value) == "keep-alive"
      end
      head[#head + 1] = {key, value}
    end

    if head.keepAlive and (not (chunkedEncoding or (contentLength and contentLength > 0)))
       or (head.method == "GET" or head.method == "HEAD") then
      mode = decodeEmpty
    elseif chunkedEncoding then
      mode = decodeChunked
    elseif contentLength then
      bytesLeft = contentLength
      mode = decodeCounted
    elseif not head.keepAlive then
      mode = decodeRaw
    end
    return head, last + 1

  end

  -- This is used for inserting a single empty string into the output string for known empty bodies
  function decodeEmpty(chunk, index)
    mode = decodeHead
    return "", index
  end

  function decodeRaw(chunk, index)
    if #chunk < index then return end
    return sub(chunk, index)
  end

  function decodeChunked(chunk, index)
    local header = match(chunk, "^[^\r\n]+\r\n", index)
    if not header then
      if #chunk - index > 8192 then
        error("chunk-size header too large")
      end

      return
    end

    -- we ignore chunk extensions
    local len = match(header, "^(%x+)")
    -- But protect against evil clients by refusing chunk-sizes longer than 16 hex digits.
    if not len or #len > 16 then
      error("invalid chunk-size")
    end

    index = index + #header
    local offset = index - 1
    local length = tonumber(len, 16)
    if #chunk < offset + length + 2 then return end
    if length == 0 then
      mode = decodeHead
    end
    assert(sub(chunk, index + length, index + length + 1) == "\r\n")
    local piece = sub(chunk, index, index + length - 1)
    return piece, index + length + 2
  end

  function decodeCounted(chunk, index)
    if bytesLeft == 0 then
      mode = decodeEmpty
      return mode(chunk, index)
    end
    local offset = index - 1
    local length = #chunk - offset
    -- Make sure we have at least one byte to process
    if length == 0 then return end

    -- If there isn't enough data left, emit what we got so far
    if length < bytesLeft then
      bytesLeft = bytesLeft - length
      return sub(chunk, index)
    end

    mode = decodeEmpty
    return sub(chunk, index, offset + bytesLeft), index + bytesLeft
  end

  -- Switch between states by changing which decoder mode points to
  mode = decodeHead
  return function (chunk, index)
    return mode(chunk, index)
  end

end

return {
  encoder = encoder,
  decoder = decoder,
}
