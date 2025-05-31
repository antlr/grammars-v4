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

-- like ipairs but omits the index var
local function viter(tbl)
  local i = 0
  return function()
    i = i + 1
    return tbl[i]
  end
end

-- populate a cache table of values from a viter-type iterator
local function vcache(cache, iter)
  for v in iter do
    table.insert(cache, v)
  end
  return cache
end

return function (db)
  local semver = require('semver')
  local normalize = semver.normalize

  db.cache = {}

  db.uncachedRead = db.read
  db.cache.refs = {}
  function db.read(author, name, version)
    version = normalize(version)
    local ref = string.format("refs/tags/%s/%s/v%s", author, name, version)
    if not db.cache.refs[ref] then
      db.cache.refs[ref] = db.uncachedRead(author, name, version)
    end
    return db.cache.refs[ref]
  end

  local doWrite = db.write
  function db.write(author, name, version, hash)
    doWrite(author, name, version, hash)

    -- update relevant caches
    db.cache.authors = vcache({}, db.uncachedAuthors())
    db.cache.names[author] = vcache({}, db.uncachedNames(author))
    db.cache.versions[author] = db.cache.versions[author] or {}
    db.cache.versions[author][name] = vcache({}, db.uncachedVersions(author, name))
  end

  db.uncachedAuthors = db.authors
  db.cache.authors = nil
  function db.authors()
    if not db.cache.authors then
      db.cache.authors = vcache({}, db.uncachedAuthors())
    end
    return viter(db.cache.authors)
  end

  db.uncachedNames = db.names
  db.cache.names = {}
  function db.names(author)
    if not db.cache.names[author] then
      db.cache.names[author] = vcache({}, db.uncachedNames(author))
    end
    return viter(db.cache.names[author])
  end

  db.uncachedVersions = db.versions
  db.cache.versions = {}
  function db.versions(author, name)
    if not db.cache.versions[author] then
      db.cache.versions[author] = {}
    end
    if not db.cache.versions[author][name] then
      db.cache.versions[author][name] = vcache({}, db.uncachedVersions(author, name))
    end
    return viter(db.cache.versions[author][name])
  end

  return db
end
