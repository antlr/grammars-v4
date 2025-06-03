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

Mid Level Storage Commands
=========================

These commands work at a higher level and consume the low-level storage APIs.

db.has(hash) -> bool                   - check if db has an object
db.load(hash) -> raw                   - load raw data, nil if not found
db.loadAny(hash) -> kind, value        - pre-decode data, error if not found
db.loadAs(kind, hash) -> value         - pre-decode and check type or error
db.save(raw) -> hash                   - save pre-encoded and framed data
db.saveAs(kind, value) -> hash         - encode, frame and save to objects/$ha/$sh
db.hashes() -> iter                    - Iterate over all hashes

db.match(author, name, version)
  -> match, hash                       - Find the best version matching the query.
db.read(author, name, version) -> hash - Read from refs/tags/$author/$tag/v$version
db.write(author, name, version, hash)  - Write to refs/tags/$suthor/$tag/v$version
db.authors() -> iter                   - Iterate over refs/tags/*
db.names(author) -> iter               - Iterate nodes in refs/tags/$author/**
db.versions(author, name) -> iter      - Iterate leaves in refs/tags/$author/$tag/*

db.readKey(author, fingerprint) -> key - Read from keys/$author/$fingerprint
db.putKey(author, fingerprint, key)    - Write to keys/$author/$fingerprint
db.revokeKey(author, fingerprint)      - Delete keys/$author/$fingerprint
db.fingerprints(author) -> iter        - iter of fingerprints

db.getEtag(author) -> etag             - Read keys/$author.etag
db.setEtag(author, etag)               - Writes keys/$author.etag

db.owners(org) -> iter                 - Iterates lines of keys/$org.owners
db.isOwner(org, author) -> bool        - Check if a user is an org owner
db.addOwner(org, author)               - Add a new owner
db.removeOwner(org, author)            - Remove an owner

db.import(fs, path) -> kind, hash      - Import a file or tree into database
db.export(hash, path) -> kind          - Export a hash to a path

]]

return function (rootPath)
  local semver = require('semver')
  local normalize = semver.normalize
  local gfs = require('coro-fs')
  local gitMount = require('git').mount
  local import = require('import')
  local export = require('export')

  local db = gitMount(gfs.chroot(rootPath))
  local storage = db.storage

  local function assertHash(hash)
    assert(hash and #hash == 40 and hash:match("^%x+$"), "Invalid hash")
  end

  function db.match(author, name, version)
    local match = semver.match(version, db.versions(author, name))
    if not match then return end
    return match, assert(db.read(author, name, match))
  end

  function db.read(author, name, version)
    version = normalize(version)
    local ref = string.format("refs/tags/%s/%s/v%s", author, name, version)
    return db.getRef(ref)
  end

  function db.write(author, name, version, hash)
    version = normalize(version)
    assertHash(hash)
    local ref = string.format("refs/tags/%s/%s/v%s", author, name, version)
    storage.write(ref, hash .. "\n")
  end

  function db.authors()
    return db.nodes("refs/tags")
  end

  function db.names(author)
    local prefix = "refs/tags/" .. author .. "/"
    local stack = {db.nodes(prefix)}
    return function ()
      while true do
        if #stack == 0 then return end
        local name = stack[#stack]()
        if name then
          local path = stack[#stack - 1]
          local newPath = path and path .. "/" .. name or name
          stack[#stack + 1] = newPath
          stack[#stack + 1] = db.nodes(prefix .. newPath)
          return newPath
        end
        stack[#stack] = nil
        stack[#stack] = nil
      end
    end
  end

  function db.versions(author, name)
    local ref = string.format("refs/tags/%s/%s", author, name)
    local iter = db.leaves(ref)
    return function ()
      local item = iter()
      return item and item:sub(2)
    end
  end

  local function keyPath(author, fingerprint)
    return string.format("keys/%s/%s", author, fingerprint:gsub(":", "_"))
  end

  function db.readKey(author, fingerprint)
    return storage.read(keyPath(author, fingerprint))
  end

  function db.putKey(author, fingerprint, key)
    return storage.put(keyPath(author, fingerprint), key)
  end

  function db.revokeKey(author, fingerprint)
    return storage.delete(keyPath(author, fingerprint))
  end

  function db.fingerprints(author)
    local iter = storage.leaves("keys/" .. author)
    return function ()
      local item = iter()
      return item and item:gsub("_", ":")
    end
  end

  function db.getEtag(author)
    return storage.read("keys/" .. author .. ".etag")
  end

  function db.setEtag(author, etag)
    return storage.write("keys/" .. author .. ".etag", etag)
  end

  local function ownersPath(org)
    return "keys/" .. org .. ".owners"
  end

  function db.owners(org)
    local owners = storage.read(ownersPath(org))
    if not owners then return end
    return owners:gmatch("[^\n]+")
  end

  function db.isOwner(org, author)
    local iter = db.owners(org)
    if not iter then return false end
    for owner in iter do
      if author == owner then return true end
    end
    return false
  end

  function db.addOwner(org, author)
    if db.isOwner(org, author) then return end
    local path = ownersPath(org)
    local owners = storage.read(path)
    owners = (owners or "") .. author .. "\n"
    storage.write(path, owners)
  end

  function db.removeOwner(org, author)
    local list = {}
    for owner in db.owners(org) do
      if owner ~= author then
        list[#list + 1] = owner
      end
    end
    storage.write(ownersPath(org), table.concat(list, "\n") .. "\n")
  end


  function db.import(fs, path) --> kind, hash
    return import(db, fs, path)
  end

  function db.export(hash, path) --> kind
    return export(db, hash, gfs, path)
  end

  return db
end
