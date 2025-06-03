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
REST API
========

This is a simple rest API for reading the remote database over HTTP.
It uses hypermedia in the JSON responses to make linking between requests simple.

GET / -> api json {
  blobs = "/blobs/{hash}"
  trees = "/trees/{hash}"
  packages = "/packages{/author}{/tag}{/version}"
  ...
}
GET /blobs/$HASH -> raw data
GET /trees/$HASH -> tree json {
 foo = { mode = 0644, hash = "...", url="/blobs/..." }
 bar = { mode = 0755, hash = "...", url="/trees/..." }
 ...
}
GET /packages -> authors json {
  creationix = "/packages/creationix"
  ...
}
GET /packages/$AUTHOR -> tags json {
  git = "/packages/creationix/git"
  ...
}
GET /packages/$AUTHOR/$TAG -> versions json {
  v0.1.2 = "/packages/creationix/git/v0.1.2"
  ...
}
GET /packages/$AUTHOR/$TAG/$VERSION -> tag json {
  hash = "..."
  object = "..."
  url = "/trees/..."
  type = "tree"
  tag = "v0.2.3"
  tagger = {
    name = "Tim Caswell",
    email = "tim@creationix.com",
    date = {
      seconds = 1423760148
      offset = -0600
    }
  }
  message = "..."
}
GET /packages/$AUTHOR/$TAG/latest -> tag json of the most recent version
GET /packages/$AUTHOR/$TAG/$VERSION.zip -> zip bundle of app and dependencies
GET /packages/$AUTHOR/$TAG/latest.zip -> zip bundle of the most recent version

GET /search/$query -> list of matches

GET /metrics -> json of currently exposed metrics
]]

local pathJoin = require('luvi').path.join
local jsonStringify = require('json').stringify
local jsonParse = require('json').parse
local modes = require('git').modes
local exportZip = require('export-zip')
local calculateHistoricDeps = require('calculate-historic-deps')
local queryDb = require('pkg').queryDb
local installDeps = require('install-deps').toDb
local ffi = require('ffi')
local fs = require('coro-fs')
local metrics = require('metrics')
local uv = require('uv')
local unpack = unpack or table.unpack

local function hex_to_char(x)
  return string.char(tonumber(x, 16))
end

local function unescape(url)
  return url:gsub("%%(%x%x)", hex_to_char)
end

local function found(terms, data)
  if not (terms and data) then return 0 end
  local count = 0
  if type(data) == "table" then
    for i = 1, #data do
      count = count + found(terms, data[i])
    end
  else
    for i = 1, #terms do
      local term = terms[i]
      if data:match(term) then
        count = count + 1
      end
    end
  end
  return count
end

local quotepattern = '(['..("%^$().[]*+-?"):gsub("(.)", "%%%1")..'])'
local function escape(str)
    return str:gsub(quotepattern, "%%%1")
end

local function compileGlob(glob)
  local parts = {}
  for a, b in glob:gmatch("([^*]*)(%**)") do
    if #a > 0 then
      parts[#parts + 1] = escape(a)
    end
    if #b > 0 then
      parts[#parts + 1] = ".*"
    end
  end
  return table.concat(parts)
end

local metaCache = {}
local urlCache = {}

-- Define the required metrics that must exist for collectMetrics() to work.
metrics.define("lua.mem.used")
metrics.define("lua.fds.used")

-- Define other metrics here.
metrics.define("lit.url._slash") -- just /
local mettab = {"blobs", "trees", "packages.zip", "packages", "search"}
for i = 1, #mettab do
  metrics.define("lit.url."..mettab[i])
end

metrics.define("lit.totals.url")

-- Collect statistics about this server's running instance.
-- Return these metrics in a table.  The intended consumer of this
-- information should render it in JSON format for transmission to
-- a time-series database and/or visualization tools.
local function collectStats()
  -- Strategy: we always calculate memory consumption and FD resource
  -- consumption synchronously with this function.  However, other
  -- parts of the system may update and maintain their own metrics
  -- asynchronously.  If they exist at all, we just return them as-is.

  collectgarbage()
  collectgarbage()
  local memoryUsed = 1024 * collectgarbage("count")

  local function countFDs(path)
    local entries = 0

    -- You might think this leaks a filehandle, but it actually doesn't.
    -- You can confirm this yourself by hitting /metrics several times in a row,
    -- and observing that lua.fds.total does not increase with each GET request.

    local iter = fs.scandir(path)
    for entry in iter do
      entries = entries + 1
    end
    return entries
  end

  local numFDs = -1
  if fs.exists("/proc/self/fd") then
    numFDs = countFDs("/proc/self/fd")
  elseif fs.exists("/dev/fd") then
    numFDs = countFDs("/dev/fd")
  end

  metrics.set("lua.mem.used", memoryUsed)
  metrics.set("lua.fds.used", numFDs)
  return metrics.all()
end

return function (db, prefix)

  local function makeUrlPath(kind, hash, filename)
    return "/" .. kind .. "s/" .. hash .. '/' .. filename
  end

  local function makeUrl(kind, hash, filename)
    return prefix .. makeUrlPath(kind, hash, filename)
  end

  local function loadMeta(author, name, version)
    local hash
    if not version then
      version, hash = (db.offlineMatch or db.match)(author, name)
    else
      hash = db.read(author, name, version)
    end
    if not hash then
      local message = "No such version " .. author .. "/" .. name
      if version then message = message .. "@" .. version end
      return nil, message
    end
    local cached = metaCache[hash]
    if cached then return cached end
    local tag = db.loadAs("tag", hash)
    local meta = tag.message:match("%b{}")
    meta = meta and jsonParse(meta) or {}
    meta.version = version
    meta.hash = hash
    meta.tagger = tag.tagger
    meta.type = tag.type
    meta.object = tag.object
    local filename = author .. "/" .. name .. "/v" .. version
    if meta.type == "blob" then
      filename = filename .. ".lua"
    end
    -- because prefix can change per-request, cache the path
    -- separately and prefix it at response-time
    local urlPath = makeUrlPath(meta.type, meta.object, filename)
    urlCache[meta] = urlPath
    meta.url = urlPath
    metaCache[hash] = meta
    return meta
  end

  local timeStart, memStart = uv.now(), collectgarbage("count")
  -- warm up db cache and meta cache
  for author in db.authors() do
    for name in db.names(author) do
      local _ = loadMeta(author, name)
    end
  end
  print('api cache warmed in ' .. (uv.now() - timeStart) .. 'ms, using ' .. string.format("%0.2f", (collectgarbage("count") - memStart) / 1024) .. ' MB memory')

  local routes = {
    "^/metrics$", collectStats,
    "^/blobs/([0-9a-f]+)/(.*)", function (hash, path)
      metrics.increment("lit.url.blobs")
      metrics.increment("lit.totals.url")
      local body = db.loadAs("blob", hash)
      local filename = path:match("[^/]+$")
      return body, {
        {"Content-Disposition", "attachment; filename=" .. filename}
      }
    end,
    "^/trees/([0-9a-f]+)/(.*)", function (hash, filename)
      metrics.increment("lit.url.trees")
      metrics.increment("lit.totals.url")
      local tree = db.loadAs("tree", hash)
      for i = 1, #tree do
        local entry = tree[i]
        tree[i].url = makeUrl(modes.toType(entry.mode), entry.hash, filename .. '/' .. entry.name)
      end
      return tree
    end,
    "^/$", function ()
      metrics.increment("lit.url._slash")
      metrics.increment("lit.totals.url")
      return  {
        blobs = prefix .. "/blobs/{hash}",
        trees = prefix .. "/trees/{hash}",
        authors = prefix .. "/packages",
        names = prefix .. "/packages/{author}",
        versions = prefix .. "/packages/{author}/{name}",
        package = prefix .. "/packages/{author}/{name}/{version}",
        search = prefix .. "/search/{query}",
        metrics = prefix .. "/metrics",
      }
    end,
    "^/packages/([^/]+)/(.+)/([^/]+)%.zip$", function (author, name, version)
      metrics.increment("lit.url.packages.zip")
      metrics.increment("lit.totals.url")
      if version == "latest" then
        version = (db.offlineMatch or db.match)(author, name)
      elseif version:sub(1,1) == "v" then
        version = version:sub(2)
      end
      local tagHash = db.read(author, name, version)
      local meta, kind, hash = queryDb(db, tagHash)

      if kind ~= "tree" then
        error("Can only create zips from trees")
      end

      -- Use snapshot if there is one
      local snapshotExists = false
      if meta.snapshot then
        snapshotExists = pcall(function() db.loadAny(meta.snapshot) end)
        if snapshotExists then
          hash = meta.snapshot
        end
      end

      if not snapshotExists then
        local tag = db.loadAs('tag', tagHash)
        local tagDate = tag.tagger.date.seconds
        local deps = {}
        calculateHistoricDeps(db, deps, meta.dependencies, tagDate)
        hash = installDeps(db, hash, deps)
      end

      -- Make sure the resolved snapshot hash matches
      if not snapshotExists then
        assert(hash == meta.snapshot, "Snapshot missing and resolved snapshot hash differs from existing hash (hash="..meta.snapshot..", resolved="..hash..")")
      end

      local zip = exportZip(db, hash)
      local filename = meta.name:match("[^/]+$") .. "-v" .. meta.version .. ".zip"

      return zip, {
        {"Content-Type", "application/zip"},
        {"Content-Disposition", "attachment; filename=" .. filename}
      }
    end,
    "^/packages/([^/]+)/(.+)/([^/]+)$", function (author, name, version)
      metrics.increment("lit.url.packages")
      metrics.increment("lit.totals.url")
      if version == "latest" then
        version = nil
      elseif version:sub(1,1) == "v" then
        version = version:sub(2)
      end
      local meta = assert(loadMeta(author, name, version))
      meta.score = nil
      return meta
    end,
    "^/packages/([^/]+)/(.+)$", function (author, name)
      metrics.increment("lit.url.packages")
      metrics.increment("lit.totals.url")
      local versions = {}
      for version in db.versions(author, name) do
        versions[version] = prefix .. "/packages/" .. author .. "/" .. name .. "/v" .. version
      end
      return next(versions) and versions
    end,
    "^/packages/([^/]+)$", function (author)
      metrics.increment("lit.url.packages")
      metrics.increment("lit.totals.url")
      local names = {}
      for name in db.names(author) do
        names[name] = prefix .. "/packages/" .. author .. "/" .. name
      end
      return next(names) and names
    end,
    "^/packages$", function ()
      metrics.increment("lit.url.packages")
      metrics.increment("lit.totals.url")
      local authors = {}
      for author in db.authors() do
        authors[author] =  prefix .. "/packages/" .. author
      end
      return next(authors) and authors
    end,
    "^/search/(.*)$", function (raw)
      metrics.increment("lit.url.search")
      metrics.increment("lit.totals.url")
      local query = { raw = raw }
      local keys = {"author", "tag", "name", "depends"}
      for i = 1, #keys do
        local key = keys[i]
        local terms = {}
        local function replace(match)
          match = compileGlob(match)
          if key == "depends" then
            match = "^" .. match .. "%f[@]"
          else
            match = "^" .. match .. "$"
          end
          terms[#terms + 1] = match
          return ''
        end
        raw = raw:gsub(key .. " *[:=] *([^ ]+) *", replace)
        raw = raw:gsub(key .. ' *[:=] *"([^"]+)" *', replace)
        if #terms > 0 then
          query[key] = terms
        end
      end
      do
        local terms = {}
        local function replace(match)
          terms[#terms + 1] = compileGlob(match, false)
          return ''
        end
        raw = raw:gsub('"([^"]+)" *', replace)
        raw = raw:gsub("([^ ]+) *", replace)
        assert(#raw == 0, "unable to parse query string")
        if #terms > 0 then
          query.search = terms
        end
      end

      local matches = {}
      for author in db.authors() do
        -- If an authors filter is given, restrict to given authors
        -- Otherwise, allow all authors.

        local skip, s1
        if query.author then
          s1 = found(query.author, author)
          skip = s1 == 0
        else
          s1 = 0
          skip = false
        end

        if not skip then
          for name in db.names(author) do
            skip = false
            local s2, s3, s4, s5
            if query.name then
              s2 = found(query.name, name)
              skip = s2 == 0
            else
              s2 = 0
            end
            local meta
            if not skip then
              meta = loadMeta(author, name)
              if not meta or meta.obsolete then
                skip = true
              end
            end
            if not skip then
              if query.tag then
                s3 = found(query.tag, meta.tags) +
                     found(query.tag, meta.keywords)
                skip = s3 == 0
              else
                s3 = 0
              end
            end
            if not skip and query.depends then
              s4 = found(query.depends, meta.dependencies)
              skip = s4 == 0
            else
              s4 = 0
            end
            if not skip and query.search then
              s5 =
                found(query.search, name) +
                found(query.search, meta.description) +
                found(query.search, meta.tags) +
                found(query.search, meta.keywords)
              skip = s5 == 0
            else
              s5 = 0
            end

            if not skip then
              meta.score = s1 + s2 + s3 + s4 + s5
              matches[author .. "/" .. name] = meta
            end
          end
        end
      end
      -- update all urls to use the current prefix
      for match, meta in pairs(matches) do
        meta.url = (prefix or '') .. urlCache[meta]
      end
      local res = {
        query = query,
        matches = matches,
        upstream = db.upstream,
      }
      return res
    end
  }

  return function (req, res)

    if not (req.method == "GET" or req.method == "HEAD") then
      res.body = "Must be GET or HEAD\n"
      return
    end

    local path = pathJoin(req.path)
    local host = req.headers.Host
    if host then
      if host:match("localhost") then
        prefix = "http://" .. host
      else
        prefix = "https://" .. host
      end
    elseif not prefix then
      prefix = ""
    end
    local body, extra
    for i = 1, #routes, 2 do
      local match = {path:match(routes[i])}
      if #match > 0 then
        for j = 1, #match do
          match[j] = unescape(match[j])
        end
        local success, err = pcall(function ()
          body, extra = routes[i + 1](unpack(match))
        end)
        if not success then body = {"error", err} end
        break
      end
    end

    if not body then
      res.code = 404
      return
    end
    res.code = 200

    if extra then
      for i = 1, #extra do
        local k, v = unpack(extra[i])
        res.headers[k] = v
      end
    end
    if type(body) == "table" then
      body = jsonStringify(body) .. "\n"
      res.headers["Content-Type"] = "application/json"
    end
    res.body = body

    return res
  end
end
