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

local log = require('log').log
local pathJoin = require('luvi').path.join
local modes = require('git').modes
local colorize = require('pretty-print').colorize
local unpack = unpack or table.unpack
local loadstring = loadstring or load

local quotepattern = '['..("%^$().[]*+-?"):gsub("(.)", "%%%1")..']'

local native_os, native_arch
do
  local info = require('luv').os_uname()
  native_os = info.sysname
  native_arch = info.machine
end

-- When importing into the db to publish, we want to include binaries for all
-- platforms, but when installing to disk or zip app bundle, we want native only.
local patterns = {
  -- Rough translation of (Linux|Windows|OSX|BSD) and (x86|x64|arm)
  -- This is more liberal than it needs to be, but works mostly in practice.
  all = {"[LWOB][iS][nXD][uxdows]*", "[xa][86r][64m]"},
  native = {native_os, native_arch},
}

local function compileFilter(rootPath, rules, nativeOnly)
  assert(#rules > 0, "Empty files rule list not allowed")
  local os, arch = unpack(patterns[nativeOnly and "native" or "all"])
  for i = 1, #rules do
    local skip, pattern = rules[i]:match("(!*)(.*)")
    local parts = {}
    for glob, text in pattern:gmatch("(%**)([^%*]*)") do
      if #glob == 1 then
        parts[#parts + 1] = "[^\\/]*"
      elseif #glob > 1 then
        parts[#parts + 1] = ".*"
      end
      if #text > 0 then
        parts[#parts + 1] = text:gsub(quotepattern, "%%%1"):gsub("/", "[/\\]")
      end
    end
    pattern = table.concat(parts):gsub("%%%$OS", os):gsub("%%%$ARCH", arch)
    rules[i] = {
      allowed = #skip == 0,
      pattern = "^" .. pattern .. "$"
    }
  end

  local default = not rules[1].allowed
  if not rules.ignore then
    local action, first
    if default then
      action = colorize("string", "includes")
      first = colorize("thread", "negative")
    else
      action = colorize("thread", "excludes")
      first = colorize("string", "positive")
    end
    log("compiling filter", string.format("%s %s by default (first rule is %s)",
      pathJoin(rootPath, "**"), action, first))
  end

  return {
    default = default,
    prefix = "^" .. pathJoin(rootPath:gsub(quotepattern, "%%%1"), '(.*)'),
    match = function (path)
      local allowed
      for i = 1, #rules do
        local rule = rules[i]
        if path:match(rule.pattern) then
          allowed = rule.allowed
        end
      end
      return allowed, path
    end
  }
end

local function isAllowed(path, entry, filters)

  -- Ignore all hidden files and folders always.
  local allow, matchesFilter, default, relativePath
  default = true
  for i = 1, #filters do
    local filter = filters[i]
    relativePath = path:match(filter.prefix)
    if relativePath then
      default = filter.default
      local newAllow = filter.match(relativePath)
      if newAllow ~= nil then
        matchesFilter = true
        allow = newAllow
      end
    end
  end
  local isTree = entry.type == "directory" or entry.mode == modes.tree
  if allow == nil then
    -- If nothing matched, fall back to defaults
    if entry.name:match("^%.") then
      -- Skip hidden files.
      allow = false
    elseif isTree then
      -- Walk all trees except deps
      allow = entry.name ~= "deps"
    else
      allow = default
    end
  end

  if relativePath then
    if allow and not isTree then
      log("including", relativePath)
    elseif not allow and matchesFilter then
      log("skipping", relativePath)
    end
  end

  return allow, default, matchesFilter and relativePath
end

local function filterTree(db, rootPath, rootHash, rules, nativeOnly) --> hash
  local filters = {}
  if rules and #rules > 0 then
    filters[#filters + 1] = compileFilter(rootPath, rules, nativeOnly)
  end

  local function copyTree(path, hash)
    local tree = db.loadAs("tree", hash)

    local meta
    for i = 1, #tree do
      local entry = tree[i]
      if entry.name == "package.lua" then
        if modes.isFile(entry.mode) then
          local lua = db.loadAs("blob", entry.hash)
          meta = assert(loadstring(lua, pathJoin(path, "package.lua")))()
        end
        break
      end
    end
    if meta and meta.files then
      filters[#filters + 1] = compileFilter(path, meta.files, nativeOnly)
    end

    local changed = false
    for i = #tree, 1, -1 do
      local entry = tree[i]
      local fullPath = pathJoin(path, entry.name)
      if isAllowed(fullPath, entry, filters) then
        if entry.mode == modes.tree then
          local newHash = copyTree(fullPath, entry.hash)
          if newHash ~= entry.hash then
            if newHash then
              entry.hash = newHash
            else
              table.remove(tree, i)
            end
            changed = true
          end
        end
      else
        changed = true
        table.remove(tree, i)
      end
    end

    return not changed and hash or #tree > 0 and db.saveAs("tree", tree)
  end

  return copyTree(rootPath, rootHash)
end

return {
  compileFilter = compileFilter,
  isAllowed = isAllowed,
  filterTree = filterTree,
}
