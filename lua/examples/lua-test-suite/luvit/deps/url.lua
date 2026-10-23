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
  name = "luvit/url"
  version = "2.1.3"
  dependencies = {
    "luvit/querystring@2.0.0",
  }
  license = "Apache 2"
  homepage = "https://github.com/luvit/luvit/blob/master/deps/url.lua"
  description = "Node-style url codec for luvit"
  tags = {"luvit", "url", "codec"}
]]

local querystring = require('querystring')

local URL = {}

local function encodeAuth(str)
  if str then
    str = string.gsub(str, '\n', '\r\n')
    str = string.gsub(str, '([^%w:!-.~\'()*])', function(c)
      return string.format('%%%02X', string.byte(c))
    end)
  end
  return str
end

local function isTruthyString(obj)
  return obj and obj ~= ""
end

-- add the prefix if it doesnt already exist
local function conditionallyPrefix(str, prefix)
  if isTruthyString(str) and string.sub(str, 1, #prefix) ~= prefix then
    str = prefix .. str
  end
  return str
end

-- add the suffix if it doesnt already exist
local function conditionallySuffix(str, suffix)
  if isTruthyString(str) and string.sub(str, -(#suffix)) ~= suffix then
    str = str .. suffix
  end
  return str
end

local function parse(url, parseQueryString)
  if type(url) == "table" then return url end
  assert(type(url) == "string", "url must be a string")

  local chunk, protocol = url:match("^(([a-z0-9+]+)://)")
  if chunk then url = url:sub(#chunk + 1) end

  local auth
  local host
  local hostname
  local port
  if protocol then
    chunk, auth = url:match('(([%w%p]+:?[%w%p]+)@)')
    if chunk then url = url:sub(#chunk + 1) end

    host = url:match("^([%a%.%d-]+:?%d*)")
    if host then
      hostname = host:match("^([^:/]+)")
      port = host:match(":(%d+)$")
      url = url:sub(#host + 1)
    end
  end

  local path
  local pathname
  local search
  local query
  local hash
  hash = url:match("(#.*)$")
  url = url:sub(1, (#url - (hash and #hash or 0)))

  if url ~= '' then
    path = url
    local temp
    temp = url:match("^[^?]*")
    if temp ~= '' then
      pathname = temp
    end
    temp = url:sub((pathname and #pathname or 0) + 1)
    if temp ~= '' then
      search = temp
    end
    if search then
    temp = search:sub(2)
      if temp ~= '' then
        query = temp
      end
    end
  end

  if parseQueryString then
    query = querystring.parse(query)
  end

  local parsed = {
    protocol = protocol,
    host = host,
    hostname = hostname,
    port = port,
    path = path or '/',
    pathname = pathname or '/',
    search = search,
    query = query,
    auth = auth,
    hash = hash
  }
  parsed.href = URL.format(parsed)

  return parsed
end

local function format(parsed)
  if type(parsed) == "string" then parsed = URL.parse(parsed) end
  assert(type(parsed) == "table", "parsed must be a table")

  local auth = parsed.auth or ""
  if auth ~= "" then
    auth = encodeAuth(auth)
    auth = auth .. '@'
  end

  local protocol = parsed.protocol or ""
  local pathname = parsed.pathname or ""
  local hash = parsed.hash or ""
  local host = false
  local query = ""
  local port = parsed.port

  if isTruthyString(parsed.host) then
    host = auth .. parsed.host
  elseif isTruthyString(parsed.hostname) then
    host = auth .. parsed.hostname
    if port then
      host = host .. ':' .. port
    end
  end

  if parsed.query and type(parsed.query) == "table" then
    query = querystring.stringify(parsed.query)
  end

  local search = parsed.search or (query ~= "" and ('?' .. query)) or ""

  protocol = conditionallySuffix(protocol, ':')

  -- urlencode # and ? characters only
  pathname = string.gsub(pathname, '([?#])', function(c)
    return string.format('%%%02X', string.byte(c))
  end)

  -- add slashes
  if host then
    host = '//' .. host
    pathname = conditionallyPrefix(pathname, '/')
  else
    host = ""
  end

  search = string.gsub(search, '#', '%23')
  hash = conditionallyPrefix(hash, '#')
  search = conditionallyPrefix(search, '?')

  return protocol .. host .. pathname .. search .. hash
end

-- split at every separator rather than get tokens between separators
-- e.g. splitString("/a/b//c/", "/") will return {"", "a", "b", "", "c", ""}
local function splitString(str, sep)
  local parts = {}
  local pos = 0
  local splitIterator = function() return str:find(sep, pos, true) end
  for sepStart, sepEnd in splitIterator do
    table.insert(parts, str:sub(pos, sepStart - 1))
    pos = sepEnd + 1
  end
  table.insert(parts, str:sub(pos))
  return parts
end

-- source and relative can either be strings or parsed tables from url.parse
-- resolveObject will return a table (in the same format as url.parse)
local function resolveObject(source, relative)
  source = URL.parse(source)
  relative = URL.parse(relative)

  local result = {}
  for k,v in pairs(source) do
    result[k] = v
  end

  -- hash is always overriden, even when href=""
  result.hash = relative.hash

  if relative.href == "" then
    result.href = URL.format(result)
    return result
  end

  if relative.protocol and relative.protocol ~= result.protocol then
    result.protocol = relative.protocol
    result.pathname = relative.pathname
    result.search = relative.search
    result.query = relative.query
    result.host = relative.host or ""
    result.auth = relative.auth
    result.hostname = relative.hostname or relative.host
    result.port = relative.port
    if result.pathname or result.search then
      local p = result.pathname or ""
      local s = result.search or ""
      result.path = p .. s
    end
    result.href = URL.format(result)
    return result
  end

  local isSourceAbs = isTruthyString(result.pathname) and string.sub(result.pathname, 1, 1) == '/'
  local isRelAbs = isTruthyString(relative.host) or isTruthyString(relative.pathname) and string.sub(relative.pathname, 1, 1) == '/'
  local mustEndAbs = isRelAbs or isSourceAbs or (isTruthyString(result.host) and isTruthyString(relative.pathname))
  local removeAllDots = mustEndAbs
  local srcPath = (isTruthyString(result.pathname) and splitString(result.pathname, '/')) or {}
  local relPath = (isTruthyString(relative.pathname) and splitString(relative.pathname, '/')) or {}

  if isRelAbs then
    if relative.host then
      result.host = relative.host
      result.auth = nil
    end
    if relative.hostname then
      result.hostname = relative.hostname
      result.auth = nil
    end
    result.search = relative.search
    result.query = relative.query
    srcPath = relPath
  elseif #relPath > 0 then
    table.remove(srcPath)
    for _,v in ipairs(relPath) do
      table.insert(srcPath, v)
    end
    result.search = relative.search
    result.query = relative.query
  elseif relative.search then
    result.search = relative.search
    result.query = relative.query
    if result.pathname or result.search then
      local p = result.pathname or ""
      local s = result.search or ""
      result.path = p .. s
    end
    result.href = URL.format(result)
    return result
  end

  if #srcPath == 0 then
    result.pathname = nil
    if result.search then
      result.path = '/' .. result.search
    else
      result.path = nil
    end
    result.href = URL.format(result)
  end

  local last = srcPath[#srcPath]
  local hasTrailingSlash = (isTruthyString(result.host) or isTruthyString(relative.host) or #srcPath > 1) and (last == "." or last == ".." or last == "")

  local up = 0
  for i=#srcPath,1,-1 do
    last = srcPath[i]
    if last == "." then
      table.remove(srcPath, i)
    elseif last == ".." then
      table.remove(srcPath, i)
      up = up + 1
    elseif up > 0 then
      table.remove(srcPath, i)
      up = up - 1
    end
  end

  if not mustEndAbs and not removeAllDots then
    while up > 0 do
      table.insert(srcPath, 1, "..")
      up = up - 1
    end
  end

  if mustEndAbs and srcPath[1] ~= "" and (not srcPath[1] or string.sub(srcPath[1], 1, 1) ~= "/") then
    table.insert(srcPath, 1, "")
  end

  if hasTrailingSlash and table.concat(srcPath, '/'):sub(-1) ~= '/' then
    table.insert(srcPath, "")
  end

  local isAbsolute = srcPath[1] == "" or (srcPath[1] and string.sub(srcPath[1], 1, 1) == "/")
  mustEndAbs = mustEndAbs or (isTruthyString(result.host) and #srcPath > 0)

  if mustEndAbs and not isAbsolute then
    table.insert(srcPath, 1, "")
  end

  if #srcPath == 0 then
    result.pathname = nil
    result.path = nil
  else
    result.pathname = table.concat(srcPath, '/')
  end

  if result.pathname or result.search then
    local p = result.pathname or ""
    local s = result.search or ""
    result.path = p .. s
  end
  result.auth = relative.auth or result.auth
  result.href = URL.format(result)
  return result
end

-- source and relative can either be strings or parsed tables from url.parse
-- resolve will return a string (in the same format as url.format)
local function resolve(source, relative)
  local resolved = URL.resolveObject(source, relative)
  return resolved.href
end

URL.parse = parse
URL.format = format
URL.resolveObject = resolveObject
URL.resolve = resolve

return URL
