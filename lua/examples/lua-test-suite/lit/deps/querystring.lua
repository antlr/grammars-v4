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

local find = string.find
local gsub = string.gsub
local char = string.char
local byte = string.byte
local format = string.format
local match = string.match
local gmatch = string.gmatch

local function urldecode(str)
  str = gsub(str, '+', ' ')
  str = gsub(str, '%%(%x%x)', function(h)
    return char(tonumber(h, 16))
  end)
  str = gsub(str, '\r\n', '\n')
  return str
end

local function urlencode(str)
  if str then
    str = gsub(str, '\n', '\r\n')
    str = gsub(str, '([^%w-_.~])', function(c)
      return format('%%%02X', byte(c))
    end)
  end
  return str
end

local function stringifyPrimitive(v)
  return tostring(v)
end

local function stringify(params, sep, eq)
  if not sep then sep = '&' end
  if not eq then eq = '=' end
  if type(params) == "table" then
    local fields = {}
    for key,value in pairs(params) do
      local keyString = urlencode(stringifyPrimitive(key)) .. eq
      if type(value) == "table" then
        for _, v in ipairs(value) do
          table.insert(fields, keyString .. urlencode(stringifyPrimitive(v)))
        end
      else
        table.insert(fields, keyString .. urlencode(stringifyPrimitive(value)))
      end
    end
    return table.concat(fields, sep)
  end
  return ''
end

-- parse querystring into table. urldecode tokens
local function parse(str, sep, eq)
  if not sep then sep = '&' end
  if not eq then eq = '=' end
  local vars = {}
  for pair in gmatch(tostring(str), '[^' .. sep .. ']+') do
    if not find(pair, eq) then
      vars[urldecode(pair)] = ''
    else
      local key, value = match(pair, '([^' .. eq .. ']*)' .. eq .. '(.*)')
      if key then
        key = urldecode(key)
        value = urldecode(value)
        local type = type(vars[key])
        if type=='nil' then
          vars[key] = value
        elseif type=='table' then
          table.insert(vars[key], value)
        else
          vars[key] = {vars[key],value}
        end
      end
    end
  end
  return vars
end

return {
  urldecode = urldecode,
  urlencode = urlencode,
  stringify = stringify,
  parse = parse,
}
