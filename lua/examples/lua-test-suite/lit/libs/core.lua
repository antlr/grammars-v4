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

Core Functions
==============

These are the high-level actions.  This consumes a database instance

core.add(path) -> author, name, version, hash - Import a package complete with signed tag.
]]

local uv = require('uv')
local pathJoin = require('luvi').path.join
local jsonStringify = require('json').stringify
local log = require('log').log
local githubQuery = require('github-request')
local pkg = require('pkg')
local sshRsa = require('ssh-rsa')
local git = require('git')
local encoders = git.encoders
local semver = require('semver')
local miniz = require('miniz')
local vfs = require('vfs')
local gfs = require('coro-fs')
local http = require('coro-http')
local exec = require('exec')
local prompt = require('prompt')(require('pretty-print'))
local luvi = require('luvi')
local makeDb = require('db')
local import = require('import')
local getInstalled = require('get-installed')
local calculateDeps = require('calculate-deps')
local queryFs = require('pkg').query
local installDeps = require('install-deps').toDb
local installDepsFs = require('install-deps').toFs
local exportZip = require('export-zip')
local digest = require('openssl').digest.digest
local request = require('coro-http').request
local makeAutoConfig = require('autoconfig')
local unpack = unpack or table.unpack

local quotepattern = '(['..("%^$().[]*+-?"):gsub("(.)", "%%%1")..'])'
local function escape(str)
    return str:gsub(quotepattern, "%%%1")
end

local function run(...)
  local stdout, stderr, code, signal = exec(...)
  if code == 0 and signal == 0 then
    return string.gsub(stdout, "%s*$", "")
  else
    return nil, string.gsub(stderr, "%s*$", "")
  end
end

local isWindows
if _G.jit then
  isWindows = _G.jit.os == "Windows"
else
  isWindows = not not package.path:match("\\")
end

-- Takes a time struct with a date and time in UTC and converts it into
-- seconds since Unix epoch (0:00 1 Jan 1970 UTC).
-- Trickier than you'd think because os.time assumes the struct is in local time.
local function getdate()
  local t_secs = os.time() -- get seconds if t was in local time.
  local t = os.date("*t", t_secs) -- find out if daylight savings was applied.
  local t_UTC = os.date("!*t", t_secs) -- find out what UTC t was converted to.
  t_UTC.isdst = t.isdst -- apply DST to this time if necessary.
  local UTC_secs = os.time(t_UTC) -- find out the converted time in seconds.
  return {
    seconds = t_secs,
    offset = os.difftime(t_secs, UTC_secs) / 60
  }
end

local function confirm(message)
  local res = prompt(message .. " (y/n)")
  return res and res:find("y")
end

local autocore
local function makeCore(config)

  if not config then
    autocore = autocore or makeCore(makeAutoConfig())
    return autocore
  end

  assert(config.database, "config.database is required path to git database")

  local db = makeDb(config.database)
  if config.upstream then
    db = require('rdb')(db, config.upstream, config.timeout)
  end
  local core = {
    config = config,
    db = db,
  }

  local privateKey
  local function getKey()
    if not config.privateKey then return end
    if privateKey then return privateKey end
    local keyData = assert(gfs.readFile(config.privateKey))
    privateKey = assert(require('openssl').pkey.read(keyData, true))
    return privateKey
  end

  function core.add(path)
    local key = getKey()
    if not (key and config.name and config.email) then
      error("Please run `lit auth` to configure your username")
    end
    local fs
    fs, path = vfs(path)
    local meta = pkg.query(fs, path)
    if not meta then
      error("Not a package: " .. path)
    end
    local author, name, version = pkg.normalize(meta)
    if not (author and name) then
      error("Malformed package metadata. Package name must be of the format 'author/package'.")
    end
    if config.upstream then core.sync(author, name) end

    local kind, hash = import(db, fs, path)
    local oldTagHash = db.read(author, name, version)
    local fullTag = author .. "/" .. name .. '/v' .. version
    if oldTagHash then
      local old = db.loadAs("tag", oldTagHash)
      if old.type == kind and old.object == hash then
        -- This package is already imported and tagged
        log("no change", fullTag)
        return author, name, version, oldTagHash
      end
      error("Tag already exists, but there are local changes.\nBump " .. fullTag .. " and try again.")
    end
    if meta.dependencies and kind == "tree" then
      local deps = {}
      calculateDeps(core.db, deps, meta.dependencies)
      meta.snapshot = installDeps(core.db, hash, deps, false)
      log("snapshot hash", meta.snapshot)
    end

    local encoded = encoders.tag({
      object = hash,
      type = kind,
      tag = author .. '/' .. name .. "/v" .. version,
      tagger = {
        name = config.name,
        email = config.email,
        date = getdate()
      },
      message = jsonStringify(meta)
    })
    if key then
      encoded = sshRsa.sign(encoded, key)
    end
    local tagHash = db.saveAs("tag", encoded)
    db.write(author, name, version, tagHash)
    log("new tag", fullTag, "success")
    return author, name, version, tagHash
  end

  local defaultTemplate = "https://github.com/luvit/luvi/releases/download/v%s/luvi-%s-%s"
  -- Given the luvi section of a package metadata, return the fs path to luvi.
  -- This will block and download it if needed.
  function core.getLuvi(meta)
    local flavor = meta and meta.flavor or "regular"
    local version = semver.normalize(meta and meta.version or luvi.version)
    local template = meta and meta.url or defaultTemplate
    local arch
    if isWindows then
      local info = uv.os_uname()
      if info.machine == 'amd64' then
        arch = "Windows-amd64.exe"
      else
        if semver.gte(version, '2.15.0') then
          arch = "Windows-x86.exe" -- the new name scheme
        else
          arch = "Windows-ia32.exe"
        end
      end
    else
      arch = run("uname", "-s") .. "_" .. run("uname", "-m")
    end
    local url = string.format(template, version, flavor, arch)
    local path = pathJoin(db.storage.fs.base, "cache", digest("sha1", url), "luvi")

    -- If it's already cached, return the path
    if gfs.access(path, "rx") then
      return path
    end

    -- If the desired version matches our process try to extract it.
    if template == defaultTemplate and flavor == "regular" and version == semver.normalize(luvi.version) then
      local exe = uv.exepath()
      local stdout = exec(exe, "-v")
      if isWindows then
        stdout = stdout:gsub('.exe','')
      end
      local iversion = stdout:match("luvi version: v(%d+%.%d+%.%d+)")
                    or stdout:match("luvi v(%d+%.%d+%.%d+)")
      if iversion == version then
        log("extracting luvi", exe)
        local reader = miniz.new_reader(exe)
        local binSize
        if reader then
          -- If contains a zip, find where the zip starts
          binSize = reader:get_offset()
        else
          -- Otherwise just read the file size
          binSize = assert(uv.fs_stat(exe)).size
        end
        assert(gfs.mkdirp(pathJoin(path, "..")))
        local fd = assert(gfs.open(path, "w", 493)) -- 0755
        local fd2 = assert(gfs.open(exe, "r", 384)) -- 0600
        assert(uv.fs_sendfile(fd, fd2, 0, binSize))
        gfs.close(fd2)
        gfs.close(fd)
        return path
      end
    end

    -- Otherwise download it fresh
    log("downloading", url)
    local head, body = request("GET", url)
    assert(head.code == 200, "Problem downloading custom luvi: " .. url)
    assert(gfs.mkdirp(pathJoin(path, "..")))
    local fd = assert(gfs.open(path, "w", 493))
    assert(gfs.write(fd, body))
    gfs.close(fd)
    return path
  end

  function core.publish(path)
    if not config.upstream then
      error("Must be configured with upstream to publish")
    end
    local author, name = core.add(path)
    local tagname = author .. '/' .. name

    -- Loop through all local versions that aren't upstream
    local queue = {}
    for version in db.versions(author, name) do
      local hash = db.read(author, name, version)
      local match = db.match(author, name, version)
      local tag = db.loadAs("tag", hash)
      local meta = pkg.queryDb(db, tag.object)
      -- Skip private modules, obsolete modules, and non-signed modules
      local skip = false
      if match ~= version then
        skip = "Obsoleted version"
      elseif not meta then
        skip = "Old style metadata"
      elseif meta.private then
        skip = "Marked private"
      elseif not tag.message:find("-----BEGIN RSA SIGNATURE-----") then
        skip = "Package not signed"
      elseif db.readRemote(author, name, version) then
        skip = "Exists at upstream"
      end
      if skip then
        log("skipping", author .. "/" .. name .. "@" .. version .. ": " .. skip)
      else
        local fulltag = string.format("%s/%s/v%s", author, name, version)
        queue[#queue + 1] = {fulltag, version, hash}
      end
    end

    if #queue == 0 then
      log("nothing to publish", tagname)
      return
    end

    for i = 1, #queue do
      local tag, _, hash = unpack(queue[i])
      if #queue == 1 or confirm(tag .. " -> " .. config.upstream .. "\nDo you wish to publish?") then
        log("publishing", tag, "highlight")
        db.push(hash)
      end
    end

  end


  local lastImport = {}
  function core.importKeys(username)

    local last = lastImport[username]
    local now = uv.now()
    if last and last + 10000 > now then
      return
    end

    lastImport[username] = now

    local path = "/users/" .. username .. "/keys?per_page=100"
    local etag = db.getEtag(username)
    local head, keys, url = githubQuery(path, etag)

    if head.code == 304 then return url end
    if head.code == 404 then
      error("No such username at github: " .. username)
    end

    if head.code ~= 200 then
      p(head)
      error("Invalid http response from github API: " .. head.code)
    end

    local fingerprints = {}
    for i = 1, #keys do
      local sshKey = sshRsa.loadPublic(keys[i].key)
      if sshKey then
        local fingerprint = sshRsa.fingerprint(sshKey)
        fingerprints[fingerprint] = sshKey
      end
    end

    local iter = db.fingerprints(username)
    if iter then
      for fingerprint in iter do
        if fingerprints[fingerprint] then
          fingerprints[fingerprint]= nil
        else
          log("revoking key", username .. ' ' .. fingerprint, "error")
          db.revokeKey(username, fingerprint)
        end
      end
    end

    for fingerprint, sshKey in pairs(fingerprints) do
      db.putKey(username, fingerprint, sshRsa.writePublic(sshKey))
      log("imported key", username .. ' ' .. fingerprint, "highlight")
    end

    for i = 1, #head do
      local name, value = unpack(head[i])
      if name:lower() == "etag" then etag = value end
    end
    db.setEtag(username, etag)

    return url
  end

  db.importKeys = core.importKeys
  db.config = core.config

  function core.authUser()
    local key = assert(getKey(), "No private key")
    local rsa = key:parse().rsa:parse()
    local sshKey = sshRsa.encode(rsa.e, rsa.n)
    local fingerprint = sshRsa.fingerprint(sshKey)
    log("checking ssh fingerprint", fingerprint)
    local url = core.importKeys(config.username)

    if not db.readKey(config.username, fingerprint) then
      error("Private key doesn't match keys at " .. url)
    end
  end

  local function makeZip(rootHash, target, luvi_source)
    if isWindows and (not target:match('%.exe$')) then
      target = target..'.exe'
    end

    log("creating binary", target, "highlight")
    if luvi_source then
      log("using luvi from", luvi_source, "highlight")
    end
    local meta = assert(pkg.queryDb(db, rootHash))

    local tempFile = target:gsub("[^/\\]+$", ".%1.temp")
    local fd = assert(uv.fs_open(tempFile, "w", 511)) -- 0777
    local binSize
    local inline = meta.luvi and meta.luvi.inline or false
    if inline then
      log("using inline luvi from meta", #inline)
      uv.fs_write(fd, inline, 0)
      binSize = #inline
    else
      local source = luvi_source or core.getLuvi(meta.luvi)
      local fd2 = assert(uv.fs_open(source, "r", 384)) -- 0600
      binSize = assert(uv.fs_fstat(fd2)).size
      log("inserting luvi", source)
      assert(uv.fs_sendfile(fd, fd2, 0, binSize))
      uv.fs_close(fd2)
    end
    assert(uv.fs_write(fd, exportZip(db, rootHash, false), binSize))
    uv.fs_close(fd)
    assert(uv.fs_rename(tempFile, target))
    log("done building", target)
  end

  local function defaultTarget(meta)
    local target = meta.target or meta.name:match("[^/]+$")
    if isWindows then
      target = target .. ".exe"
    end
    return target
  end

  function core.make(source, target, luvi_source)
    local zfs
    -- Use vfs so that source can be a zip file or a folder.
    zfs, source = vfs(source)
    local meta = assert(queryFs(zfs, source))
    target = pathJoin(uv.cwd(), target or defaultTarget(meta))

    -- Determine if target is inside the source we're reading fom
    local rules
    local inside = target:match("^" .. escape(source) .. "[/\\](.*)$")
    if inside then
      -- If it is, add an ignore rule for it.
      rules = { "!" .. inside, ignore = true }
    end
    local kind, hash = assert(import(core.db, zfs, source, rules, true))
    assert(kind == "tree", "Only tree packages are supported for now")
    local deps = getInstalled(zfs, source)
    calculateDeps(core.db, deps, meta.dependencies)
    hash = installDeps(core.db, hash, deps, true)
    return makeZip(hash, target, luvi_source)
  end

  local function makeGit(target, luvi_source, url)
    local path = (url:match("([^/]+).git$") or target or "app") .. ".git-clone"
    log("cloning shallow repo", url)
    local stdout, stderr, code, signal = exec("git", "clone", "--depth=1", "--recursive", url, path)
    if code == 0 and signal == 0 then
      core.make(path, target, luvi_source)
    else
      error("Problem cloning: " .. stdout .. stderr)
    end
    assert(gfs.rmrf(path))
  end

  local function makeHttp(target, luvi_source, url)
    log("downloading zip", url)
    local res, body = http.request("GET", url)
    assert(res.code == 200, body)
    local filename
    for i = 1, #res do
      local key, value = unpack(res[i])
      if key:lower() == "content-disposition" then
        filename = value:match("filename=\"?([^;\"]+)")
      end
    end

    local path = filename or (target or "app") .. ".zip"
    gfs.writeFile(path, body)
    core.make(path, target, luvi_source)
    gfs.unlink(path)
  end

  local function makeLit(target, luvi_source, author, name, version)
    local tag = author .. '/' .. name
    local match, hash = db.match(author, name, version)
    if not match then
      if version then tag = tag .. "@" .. version end
      error("No such lit package: " ..  tag)
    end
    tag = tag .. "@" .. match

    if db.fetch then
      db.fetch({hash})
    end
    local meta = pkg.queryDb(db, hash)
    if not meta then
      error("Not a valid package: " .. tag)
    end

    target = target or defaultTarget(meta)

    -- Use snapshot if there is one
    if meta.snapshot then
      log("using snapshot", meta.snapshot, "highlight")
      -- but fall back to resolving deps if we can't get the snapshot
      local ok, err = pcall(makeZip, meta.snapshot, target, luvi_source)
      if not ok then
        log('failed to get snapshot', err, "failure")
      else
        return
      end
    end

    local deps = {}
    calculateDeps(core.db, deps, meta.dependencies)
    local tagObj = db.loadAs("tag", hash)
    if tagObj.type ~= "tree" then
      error("Only tags pointing to trees are currently supported for make")
    end
    hash = installDeps(core.db, tagObj.object, deps, true)
    return makeZip(hash, target, luvi_source)
  end

  local aliases = {
    "^github://([^/]+)/([^/@]+)/?@(.+)$", "https://github.com/%1/%2/archive/%3.zip",
    "^github://([^/]+)/([^/]+)/?$", "https://github.com/%1/%2/archive/master.zip",
    "^gist://([^/]+)/(.+)/?$", "https://gist.github.com/%1/%2.git",
  }
  core.urlAilases = aliases

  local handlers = {
    "^(https?://[^#]+%.git)$", makeGit,
    "^(https?://[^#]+)$", makeHttp,
    "^(git://.*)$", makeGit,
    "^lit://([^/]+)/([^@]+)@v?(.+)$", makeLit,
    "^lit://([^/]+)/([^@]+)$", makeLit,
    "^([^@/]+)/([^@]+)@v?(.+)$", makeLit,
    "^([^@/]+)/([^@]+)$", makeLit,
    "^([^ :/@]+%@.*)$", makeGit,
  }
  core.urlHandlers = handlers

  function core.makeUrl(url, target, luvi_source)
    local fullUrl = url
    for i = 1, #aliases, 2 do
      fullUrl = fullUrl:gsub(aliases[i], aliases[i + 1])
    end
    for i = 1, #handlers, 2 do
      local match = {fullUrl:match(handlers[i])}
      if #match > 0 then return handlers[i + 1](target, luvi_source, unpack(match)) end
    end
    error("Not a file or valid url: " .. fullUrl)
  end

  function core.installList(path, newDeps)
    local deps = getInstalled(gfs, path)
    calculateDeps(core.db, deps, newDeps)
    installDepsFs(core.db, gfs, path, deps, true)
    return deps
  end

  function core.installDeps(path)
    -- bubble up errors in package
    local meta = assert(pkg.query(gfs, path))
    if not meta.dependencies then
      log("no dependencies", path)
      return
    end
    return core.installList(path, meta.dependencies)
  end

  function core.sync(mainAuthor, mainName)
    local hashes = {}
    local tags = {}
    local function check(author, name)
      local versions = {}
      for version in db.versions(author, name) do
        local match, hash = db.offlineMatch(author, name, version)
        versions[match] = hash
      end
      for version, hash in pairs(versions) do
        local match, newHash = db.match(author, name, version)
        if hash ~= newHash then
          hashes[#hashes + 1] = newHash
          tags[#tags + 1] = author .. "/" .. name .. "/v" .. match
        end
      end
      local match, hash = db.match(author, name)
      if match and not db.offlineMatch(author, name, match) then
        hashes[#hashes + 1] = hash
        tags[#tags + 1] = author .. "/" .. name .. "/v" .. match
      end
    end

    if mainAuthor then
      if mainName then
        log("checking for updates", mainAuthor .. '/' .. mainName)
        check(mainAuthor, mainName)
      else
        log("checking for updates", mainAuthor .. "/*")
        for name in db.names(mainAuthor) do
          check(mainAuthor, name)
        end
      end
    else
      log("checking for updates", "*/*")
      for author in db.authors() do
        for name in db.names(author) do
          check(author, name)
        end
      end
    end
    if #tags == 0 then return end
    log("syncing", table.concat(tags, ", "), "highlight")
    db.fetch(hashes)
  end

  local function makeRequest(name, req)
    local key = getKey()
    if not (key and config.name and config.email) then
      error("Please run `lit auth` to configure your username")
    end
    assert(db.upquery, "upstream required to publish")
    req.username = config.username
    local json = jsonStringify(req) .. "\n"
    local signature = sshRsa.sign(json, key)
    return db.upquery(name, signature)
  end

  function core.claim(org)
    return makeRequest("claim", { org = org })
  end

  function core.share(org, friend)
    return makeRequest("share", { org = org, friend = friend })
  end

  function core.unclaim(org)
    return makeRequest("unclaim", { org = org })
  end

  return core
end

return makeCore
