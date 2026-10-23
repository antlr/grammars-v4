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
  name = "luvit/http-header"
  version = "1.0.0"
  license = "Apache 2"
  homepage = "https://github.com/luvit/luvit/blob/master/deps/http-header.lua"
  description = "Utilities for dealing with HTTP headers in Luvit"
  tags = {"luvit", "http"}
]]

-- Provide a nice case insensitive interface to headers.
-- Pulled from https://github.com/creationix/weblit/blob/master/libs/weblit-app.lua
local headerMeta = {
  __index = function (list, name)
    if type(name) ~= "string" then
      return rawget(list, name)
    end
    name = name:lower()
    for i = 1, #list do
      local key, value = list[i][1], list[i][2]
      if key:lower() == name then return value end
    end
  end,
  __newindex = function (list, name, value)
    -- non-string keys go through as-is.
    if type(name) ~= "string" then
      return rawset(list, name, value)
    end
    -- First remove any existing pairs with matching key
    local lowerName = name:lower()
    for i = #list, 1, -1 do
      if list[i][1]:lower() == lowerName then
        table.remove(list, i)
      end
    end
    -- If value is nil, we're done
    if value == nil then return end
    -- Otherwise, set the key(s)
    if (type(value) == "table") then
      -- We accept a table of strings
      for i = 1, #value do
        rawset(list, #list + 1, {name, tostring(value[i])})
      end
    else
      -- Or a single value interperted as string
      rawset(list, #list + 1, {name, tostring(value)})
    end
  end,
}

-- Creates a new headers table or sets the metatable of `tbl` to headerMeta
local function newHeaders(tbl)
  return setmetatable(tbl or {}, headerMeta)
end

-- Adds all header information found in `tbl` into `headers`, which should be a table
-- with headerMeta as its metatable.
--
-- Note: String keys in `tbl` will overwrite the key's value(s) in `headers` if it exists
local function appendToHeaders(tbl, headers)
    if tbl then
      for k,v in pairs(tbl) do
        if type(k) == "number" then k = #headers + 1 end
        headers[k] = v
      end
    end
    return headers
end

-- Converts a table of headers into a headers table.
-- The input tables can have keys in any of the following formats:
--
--   {
--     ["name"] = value,
--     ["name"] = {multiple, values},
--     {"name", value},
--   }
local function toHeaders(tbl)
  return appendToHeaders(tbl, newHeaders())
end

-- Converts and combines any table(s) of headers to a single headers table.
-- The input tables can have keys in any of the following formats:
--
--   {
--     ["name"] = value,
--     ["name"] = {multiple, values},
--     {"name", value},
--   }
--
-- Note: Duplicate string keys will overwrite eachother, with the last duplicate 
-- key of the last table taking precedence
local function combineHeaders(...)
  local combined = newHeaders()
  for _, tbl in ipairs({...}) do
    appendToHeaders(tbl, combined)
  end
  return combined
end

-- Extracts headers from a table that has array-like keys of {headerName, value} tables.
-- Ignores any non-array-like keys of the table.
local function getHeaders(tbl)
  local headers = newHeaders()
  if tbl then
    for i,header in ipairs(tbl) do
      if type(header) == "table" then
        headers[i] = header
      end
    end
  end
  return headers
end

return {
  headerMeta = headerMeta,
  newHeaders = newHeaders,
  toHeaders = toHeaders,
  combineHeaders = combineHeaders,
  getHeaders = getHeaders
}
