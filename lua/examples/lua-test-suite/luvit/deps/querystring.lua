--[[

Copyright 2015 The Luvit Authors. All Rights Reserved.

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
  name = "luvit/querystring"
  version = "2.0.1"
  license = "Apache 2"
  homepage = "https://github.com/luvit/luvit/blob/master/deps/querystring.lua"
  description = "Node-style query-string codec for luvit"
  tags = {"luvit", "url", "codec"}
]]

local format = string.format
local byte = string.byte
local char = string.char
local gsub = string.gsub
local gmatch = string.gmatch
local find = string.find
local match = string.match
local insert = table.insert
local concat = table.concat


local function hexToChar(hex)
  return char(tonumber(hex, 16))
end

local function charToHex(character)
  return format('%%%02X', byte(character))
end


local function urldecode(str)
  if str then
    str = gsub(str, '+', ' ')
    str = gsub(str, '%%(%x%x)', hexToChar)
  end
  return str
end

local function urlencode(str)
  if str then
    str = gsub(str, '[^a-zA-Z0-9*%-%._]', charToHex)
  end
  return str
end


local function stringify(tbl, sep, eq)
  if type(tbl) ~= 'table' then
    return ''
  end

  sep = sep or '&'
  eq = eq or '='

  local fields = {}
  for key, value in pairs(tbl) do
    local keyString = urlencode(tostring(key)) .. eq

    if type(value) == 'table' then
      for _, subValue in ipairs(value) do
        insert(fields, keyString .. urlencode(tostring(subValue)))
      end
    else
      insert(fields, keyString .. urlencode(tostring(value)))
    end
  end

  return concat(fields, sep)
end

local function parse(str, sep, eq)
  sep = sep or '&'
  eq = eq or '='

  local keyValuePat = '([^' .. eq .. ']*)' .. eq .. '(.*)'

  local parsed = {}
  for pair in gmatch(tostring(str), '[^' .. sep .. ']+') do
    if not find(pair, eq) then
      parsed[urldecode(pair)] = ''
    else
      local key, value = match(pair, keyValuePat)

      if key then
        key = urldecode(key)
        value = urldecode(value)

        local existingValue = parsed[key]
        if existingValue == nil then
          parsed[key] = value
        elseif type(existingValue) == 'table' then
          insert(existingValue, value)
        else
          parsed[key] = {existingValue, value}
        end
      end
    end
  end

  return parsed
end


return {
  urldecode = urldecode,
  urlencode = urlencode,
  stringify = stringify,
  parse = parse,
}
