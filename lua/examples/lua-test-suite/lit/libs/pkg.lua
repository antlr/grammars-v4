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

--[[
Package Metadata Commands
================

These commands work with packages metadata.

pkg.query(fs, path) -> meta, path           - Query an on-disk path for package info.
pkg.queryDb(db, path) -> meta, kind         - Query an in-db hash for package info.
pky.normalize(meta) -> author, tag, version - Extract and normalize pkg info
]]

local isFile = require('git').modes.isFile
local semver = require('semver')
local pathJoin = require('luvi').path.join
local listToMap = require('git').listToMap
local jsonParse = require('json').parse
local load = load
if not load then
  function load(code, name, mode, env)
    local fn, err = loadstring(code, name)
    if not fn then return fn, err end
    if env then setfenv(fn, env) end
    return fn
  end
end

local function evalModule(data, name)
  -- Match multiline lua comments that start with `lit-meta`
  local a, b = data:find("%-%-%[(=*)%[lit%-meta")
  if a then
    local term = "]" .. data:sub(a + 3, b - 9) .."]"
    local c = data:find(term, b + 1, true)
    if c then
      local env = {}
      local fn, err = load(data:sub(b + 1, c - 1), name, "t", env)
      assert(not err, err)
      assert(pcall(fn))
      return env
    end
  end
  local exports = {}
  local module = { exports = exports }
  local fn, err = load(data, name, 't', {
    exports = exports,
  })
  if not fn then return nil, err end
  local success, ret = pcall(fn)

  local meta = success and type(ret) == "table" and ret or module.exports
  if not meta then return nil, "Missing exports in " .. name end
  if not meta.name then return nil, "Missing name in package description in " .. name end
  if not meta.version then return nil, "Missing version in package description in " .. name end
  return meta
end

local validKeys = {
  name = "string",
  version = "string",
  private = "boolean", -- Don't allow publishing.
  obsolete = "boolean", -- Hide from search results.
  description = "string",
  keywords = "table", -- list of strings
  tags = "table", -- list of strings
  homepage = "string",
  license = "string",
  licenses = "table", -- table of strings
  author = "table", -- person {name=name, email=email, url=url}
  contributors = "table", -- list of people
  dependencies = "table", -- list of strings
  luvi = "table", -- {flavor=flavor,version=version},
  files = "table",
}


local function query(fs, path)
  local packagePath = path
  local stat, data, err
  stat, err = fs.stat(path)
  local attempts = {}
  if stat then
    if stat.type == "directory" then
      packagePath = path .. "/"
      local fullPath = pathJoin(path, "package.lua")
      attempts[#attempts + 1] = fullPath
      data, err = fs.readFile(fullPath)
      if err and not err:match("^ENOENT:") then error(err) end
      if not data then
        fullPath = pathJoin(path, "init.lua")
        attempts[#attempts + 1] = fullPath
        data, err = fs.readFile(fullPath)
        if err and not err:match("^ENOENT:") then error(err) end
      end
    else
      attempts[#attempts + 1] = packagePath
      data, err = fs.readFile(packagePath)
    end
  elseif err:match("^ENOENT:") then
    packagePath = packagePath .. ".lua"
    attempts[#attempts + 1] = packagePath
    data, err = fs.readFile(packagePath)
  end
  if not data then
    local sep = "\n  Looked in: "
    local message = "\nCan't find package at " .. path .. sep .. table.concat(attempts, sep)
    if err then message = message .. "\n" .. err end
    return data, message
  end
  local meta, err = evalModule(data, packagePath)
  if err then return meta, err end
  local clean = {}
  if not meta then return nil, "No meta found" end
  for key, value in pairs(meta) do
    if type(value) == validKeys[key] then
      clean[key] = value
    end
  end
  return clean, packagePath
end

local function queryDb(db, hash)
  local kind, value = db.loadAny(hash)
  if kind == "tag" then
    hash = value.object

    -- Use metata data in tag message if found
    local meta = jsonParse(value.message)
    if meta then
      return meta, value.type, hash
    end

    local tagType = value.type
    -- Otherwise search root tree or blob
    kind, value = db.loadAny(hash)
    assert(kind == tagType, "type mismatch")
  end
  local meta
  if kind == "tree" then
    local path = "tree:" .. hash
    local tree = listToMap(value)
    local entry = tree["package.lua"]
    if entry then
      path = path .. "/package.lua"
    else
      entry = tree["init.lua"]
      path = path .. "/init.lua"
    end
    if not (entry and isFile(entry.mode)) then
      return nil, "ENOENT: No package.lua or init.lua in tree:" .. hash
    end
    meta = evalModule(db.loadAs("blob", entry.hash), path)
  elseif kind == "blob" then
    meta = evalModule(value, "blob:" .. hash)
  else
    error("Illegal kind: " .. kind)
  end
  return meta, kind, hash
end

local function normalize(meta)
  local author, tag = meta.name:match("^([^/]+)/(.*)$")
  return author, tag, semver.normalize(meta.version)
end


return {
  query = query,
  queryDb = queryDb,
  normalize = normalize,
}
