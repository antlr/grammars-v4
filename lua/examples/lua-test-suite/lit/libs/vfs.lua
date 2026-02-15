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

local gfs = require('coro-fs')
local miniz = require('miniz')
local pathJoin = require('luvi').path.join

local function zipFs(zip, autoChroot)
  local zfs = {}

  local prefix = "./"

  local function locateFile(path)
    local index, err = zip:locate_file(path)
    if index then return index end
    if err:match("^Can't find file") then
      err = "ENOENT: " .. err
    end
    return nil, err
  end

  function zfs.stat(path)
    path = pathJoin(prefix .. path)
    if path == "" then
      return {
        type = "directory",
        mode = 493
      }
    end
    local err
    local index = locateFile(path)
    if not index then
      index, err = locateFile(path .. "/")
      if not index then return nil, err end
    end
    local raw = zip:stat(index)
    local typ = raw.filename:sub(-1) == "/" and "directory" or "file"
    return {
      type = typ,
      size = raw.uncomp_size,
      mtime = raw.time,
      mode = typ == "directory" and 493 or 420
    }
  end
  function zfs.scandir(path)
    path = pathJoin(prefix .. path)
    local index, err
    if path == "" then
      index = 0
    else
      path = path .. "/"
      index, err = locateFile(path)
      if not index then return nil, err end
      if not zip:is_directory(index) then
        return nil, path .. " is not a directory"
      end
    end
    local i = index + 1
    local num = zip:get_num_files()
    return function ()
      while i <= num do
        local filename = zip:get_filename(i)
        i = i + 1
        if string.sub(filename, 1, #path) ~= path then return end
        filename = filename:sub(#path + 1)
        local typ = "file"
        local n = string.find(filename, "/")
        if n == #filename then
          filename = string.sub(filename, 1, #filename - 1)
          typ = "directory"
          n = nil
        end
        if not n then
          return {
            name = filename,
            type = typ,
            mode = typ == "directory" and 493 or 420
          }
        end
      end
    end
  end
  function zfs.readFile(path)
    path = pathJoin(prefix .. path)
    local index, err = locateFile(path)
    if not index then return nil, err end
    return zip:extract(index)
  end

  -- Check for zips with single folder at root
  if autoChroot then
    local entries = {}
    for entry in zfs.scandir("") do
      entries[#entries + 1] = entry
      if #entries > 1 then break end
    end
    if #entries == 1 and entries[1].type == "directory" then
      prefix = entries[1].name .. "/"
    end
  end

  return zfs
end

return function (path)
  local zip = miniz.new_reader(path)
  if zip then
    local fs, newPath = zipFs(zip, true), ""
    fs.base = path
    return fs, newPath
  else
    return gfs, path
  end
end
