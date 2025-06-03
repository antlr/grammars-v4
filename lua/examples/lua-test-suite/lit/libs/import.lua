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

local pathJoin = require('luvi').path.join
local isAllowed = require('rules').isAllowed
local compileFilter = require('rules').compileFilter
local modes = require('git').modes
local loadstring = loadstring or load

-- Import a fs path into the database
return function (db, fs, rootpath, rules, nativeOnly)
  if nativeOnly == nil then nativeOnly = false end
  local filters = {}
  if rules then
    filters[#filters + 1] = compileFilter(rootpath, rules, nativeOnly)
  end

  local importEntry, importTree

  function importEntry(path, stat)
    if stat.type == "directory" then
      local hash, err = importTree(path)
      if not hash and err then
        return nil, err
      end
      return modes.tree, hash
    end
    if stat.type == "file" then
      if not stat.mode then
        stat = fs.stat(path)
      end
      local mode = bit.band(stat.mode, 73) > 0 and modes.exec or modes.file
      local data, hash, err
      data, err = fs.readFile(path)
      if not data then
        return nil, err or "Problem reading file: " .. path
      end
      hash, err = db.saveAs("blob", data)
      if not hash then
        return nil, err or "Problem saving blob: " .. path
      end
      return mode, hash
    end
    if stat.type == "link" then
      local data, hash, err
      data, err = fs.readlink(path)
      if not data then
        return nil, err or "Problem reading symlink: " .. path
      end
      hash, err = db.saveAs("blob", data)
      if not hash then
        return nil, err or "Problem saving symlink: " .. path
      end
      return modes.sym, hash
    end
    return nil, "Unsupported entry type at " .. path .. ": " .. tostring(stat.type)
  end

  function importTree(path)
    assert(type(fs) == "table")

    local items = {}
    local meta = fs.readFile(pathJoin(path, "package.lua"))
    if meta then meta = loadstring(meta)() end
    if meta and meta.files then
      filters[#filters + 1] = compileFilter(path, meta.files, nativeOnly)
    end

    local iter, err = fs.scandir(path)
    if not iter then
      return nil, err or "Problem scanning directory: " .. path
    end

    for entry in iter do
      local fullPath = pathJoin(path, entry.name)
      if not entry.type then
        local stat, e = fs.stat(fullPath)
        if not (stat and stat.type) then
          return nil, e or "Cannot determine entry type: " .. fullPath
        end
        entry.type = stat.type
      end
      if isAllowed(fullPath, entry, filters) then
        local mode, hash = importEntry(fullPath, entry)
        if not mode then
          return nil, hash or "Problem importing entry: " .. fullPath
        end
        entry.mode, entry.hash = mode, hash
        if entry.hash then
          items[#items + 1] = entry
        end
      end
    end
    if #items > 0 then
      return db.saveAs("tree", items)
    end
  end

  local stat, err = fs.stat(rootpath)
  if not stat then
    return nil, err or "Problem statting: " .. rootpath
  end
  local mode, hash = importEntry(rootpath, stat)
  if not mode then
    return nil, hash or "Problem importing: " .. rootpath
  end
  if not hash then
    return nil, "Nothing to import"
  end
  return modes.toType(mode), hash
end
