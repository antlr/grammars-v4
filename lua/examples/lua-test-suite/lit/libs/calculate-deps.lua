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

local normalize = require('semver').normalize
local gte = require('semver').gte
local log = require('log').log
local queryDb = require('pkg').queryDb
local colorize = require('pretty-print').colorize

return function (db, deps, newDeps)

  local addDep, processDeps

  function processDeps(dependencies)
    if not dependencies then return end
    for alias, dep in pairs(dependencies) do
      local name, version = dep:match("^([^@]+)@?(.*)$")
      if #version == 0 then
        version = nil
      end
      if type(alias) == "number" then
        alias = name:match("([^/]+)$")
      end
      if not name:find("/") then
        error("Package names must include owner/name at a minimum")
      end
      if version then
        local ok
        ok, version = pcall(normalize, version)
        if not ok then
          error("Invalid dependency version: " .. dep)
        end
      end
      addDep(alias, name, version)
    end
  end

  function addDep(alias, name, version)
    local meta = deps[alias]
    if meta then
      if name ~= meta.name then
        local message = string.format("%s %s ~= %s",
          alias, meta.name, name)
        log("alias conflict", message, "failure")
        return
      end
      if version then
        if not gte(meta.version, version) then
          local message = string.format("%s %s ~= %s",
            alias, meta.version, version)
          log("version conflict", message, "failure")
          return
        end
      end
    else
      local author, pname = name:match("^([^/]+)/(.*)$")
      local match, hash = db.match(author, pname, version)

      if not match then
        error("No such "
          .. (version and "version" or "package") .. ": "
          .. name
          .. (version and '@' .. version or ''))
      end
      local kind
      meta, kind, hash = assert(queryDb(db, hash))
      meta.db = db
      meta.hash = hash
      meta.kind = kind
      deps[alias] = meta
    end

    processDeps(meta.dependencies)

  end

  processDeps(newDeps)

  local names = {}
  for k in pairs(deps) do
    names[#names + 1] = k
  end
  table.sort(names)
  for i = 1, #names do
    local name = names[i]
    local meta = deps[name]
    log("including dependency", string.format("%s (%s)",
      colorize("highlight", name), meta.path or meta.version))
  end


  return deps
end
