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

local git = require('git')
local listToMap = git.listToMap
local modeToType = git.modes.toType
local modes = git.modes
local unpack = unpack or table.unpack

return function (db, root)
  local cache = {}

  local function loadAny(hash)
    local cached = cache[hash]
    if cached then return unpack(cached) end
    local kind, value = db.loadAny(hash)
    cache[hash] = {kind, value}
    return kind, value
  end

  local function loadAs(kind, hash)
    local actual, value = loadAny(hash)
    if not actual then
      error("No such hash: " .. hash)
    end
    if actual ~= kind then
      error("Kind mismatch: expected " .. kind .. " but found " .. actual .. " for " .. hash)
    end
    return value
  end

  local function resolvePath(path)
    local hash = root
    local kind, value = loadAny(hash)
    if kind == "tag" then
      hash = value.object
      kind = value.type
      value = loadAs(kind, hash)
    end
    if kind == "commit" then
      hash = value.tree
      kind = "tree"
      value = loadAs(kind, hash)
    end
    local mode = kind == "tree" and modes.tree or modes.blob
    for part in path:gmatch("[^/]+") do
      if kind == "tree" then
        local tree = listToMap(value)
        local entry = tree[part]
        if not entry then
          return nil, "ENOENT: no such entry: " .. root .. ":" .. path
        end
        if entry.mode == modes.commit then
          error("Submodules not implemented yet")
        end
        hash = entry.hash
        mode = entry.mode
        kind, value = loadAny(hash)
      else
        return nil, "ENOENT: no such entry: " .. root .. ":" .. path
      end
    end
    return kind, mode, hash, value
  end

  local function typeFromMode(mode)
    local t = mode == modes.tree and "directory"
      or modes.isFile(mode) and "file"
      or mode == modes.sym and "symlink"
      or modeToType(mode)
    return t
  end

  local gfs = {}
  function gfs.stat(path)
    local kind, mode, hash = resolvePath(path)
    if not kind then return kind, mode end
    return {
      hash = hash,
      mode = mode,
      type = typeFromMode(mode)
    }
  end
  function gfs.scandir(path)
    if path == "init.lua" then print(debug.traceback()) end
    local kind, mode, _, value = resolvePath(path)
    if not kind then return kind, mode end
    if kind ~= "tree" then
      return nil, "EINVAL: not a tree " .. root .. ":" .. path
    end
    local i = 1
    return function ()
      local entry = value[i]
      if not entry then return end
      i = i + 1
      return {
        hash = entry.hash,
        name = entry.name,
        mode = entry.mode,
        type = typeFromMode(entry.mode)
      }
    end
  end
  function gfs.readFile(path)
    local kind, mode, _, value = resolvePath(path)
    if kind == "blob" then return value end
    if not kind then return kind, mode end
    return nil, "EINVAL: not a file " .. root .. ":" .. path
  end
  return gfs, ""
end
