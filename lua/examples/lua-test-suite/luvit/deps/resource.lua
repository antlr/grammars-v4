--[[

Copyright 2014-2016 The Luvit Authors. All Rights Reserved.

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
  name = "luvit/resource"
  version = "2.1.1"
  license = "Apache 2"
  homepage = "https://github.com/luvit/luvit/blob/master/deps/resource.lua"
  description = "Utilities for loading relative resources"
  dependencies = {
    "luvit/pathjoin@2.0.0"
  }
  tags = {"luvit", "relative", "resource"}
]]

local pathJoin = require('pathjoin').pathJoin
local bundle = require('luvi').bundle
local uv = require('uv')

local function getPath()
  local caller = debug.getinfo(2, "S").source
  if caller:sub(1,1) == "@" then
    return caller:sub(2)
  elseif caller:sub(1, 7) == "bundle:" then
    return caller
  end
  error("Unknown file path type: " .. caller)
end

local function getDir()
  local caller = debug.getinfo(2, "S").source
  if caller:sub(1,1) == "@" then
    return pathJoin(caller:sub(2), "..")
  elseif caller:sub(1, 7) == "bundle:" then
    return "bundle:" .. pathJoin(caller:sub(8), "..")
  end
  error("Unknown file path type: " .. caller)
end

local function innerResolve(path, resolveOnly)
  local caller = debug.getinfo(2, "S").source
  if caller:sub(1,1) == "@" then
    path = pathJoin(caller:sub(2), "..", path)
    if resolveOnly then return path end
    local fd = assert(uv.fs_open(path, "r", 420))
    local stat = assert(uv.fs_fstat(fd))
    local data = assert(uv.fs_read(fd, stat.size, 0))
    uv.fs_close(fd)
    return data, path
  elseif caller:sub(1, 7) == "bundle:" then
    path = pathJoin(caller:sub(8), "..", path)
    if resolveOnly then return path end
    return bundle.readfile(path), "bundle:" .. path
  end
end

local function resolve(path)
  return innerResolve(path, true)
end

local function load(path)
  return innerResolve(path, false)
end

local function getProp(self, key)
  if key == "path" then return getPath() end
  if key == "dir" then return getDir() end
end

return setmetatable({
  resolve = resolve,
  load = load,
}, { __index = getProp })
