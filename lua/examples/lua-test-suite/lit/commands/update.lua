
return function ()
  local log = require('log').log
  local core = require('core')()
  local uv = require('uv')
  local pathJoin = require('luvi').path.join
  local exec = require('exec')
  local request = require('coro-http').request
  local semver = require('semver')
  local jsonParse = require('json').parse
  local fs = require('coro-fs')
  local spawn = require('coro-spawn')
  local updater = require('auto-updater')

  local function fetch(url)
    log("downloading", url)
    local head, body = request("GET", url)
    if head.code ~= 200 then
      error("Expected 200 response from " .. url .. ", but got " .. head.code)
    end
    return body
  end

  local isWindows
  if _G.jit then
    isWindows = _G.jit.os == "Windows"
  else
    isWindows = not not package.path:match("\\")
  end

  -- Returns current version, latest version and latest compat version.
  local function checkUpdate()
    -- Read the current lit version
    local meta = require('../package')
    local version = semver.normalize(meta.version)
    local body = fetch("https://lit.luvit.io/packages/luvit/lit")
    local versions = assert(jsonParse(body), "Problem parsing JSON response from lit")
    local key
    local best = semver.match(version, function ()
      key = next(versions, key)
      return key
    end)
    key = nil
    local latest = semver.match(nil, function ()
      key = next(versions, key)
      return key
    end)
    return version, latest, best
  end

  do
    local version, latest, best = checkUpdate()
    local toupdate
    if version == latest then
      log("lit is up to date", version, "highlight")
    elseif not best or version == best then
      log("update status", "major update available: " .. latest)
      toupdate = latest
    elseif best then
      log("update status", "update available: " .. best)
      toupdate = best
    elseif not semver.gte(latest, version) then
      log("update status", "newer than published: " .. latest)
    else
      log("update status", "unknown series")
    end

    if toupdate then
      -- Guess path to lit install
      local target = uv.exepath()
      local stdout = exec(target, "-v")
      if not stdout:match("^lit version:") then
        target = pathJoin(target, "..", "lit")
        if isWindows then
          target = target .. ".exe"
        end
      end

      -- Download metadata for updated lit version
      local meta = jsonParse(fetch("https://lit.luvit.io/packages/luvit/lit/v" .. toupdate))

      -- Ensure proper luvi binary
      local luviPath = core.getLuvi(meta.luvi)

      -- Copy luvi to start of new file
      local tempPath = pathJoin(uv.cwd(), "lit-temp")
      local fd = assert(fs.open(tempPath, "w", 493)) -- 755
      local fd2 = assert(fs.open(luviPath, "r"))
      local size = assert(fs.fstat(fd2)).size
      assert(uv.fs_sendfile(fd, fd2, 0, size))
      fs.close(fd2)

      -- Download and append zip
      local zip = fetch("https://lit.luvit.io/packages/luvit/lit/v" .. toupdate .. ".zip")
      assert(fs.write(fd, zip))
      fs.close(fd)

      -- replace installed lit binary
      local old = fs.stat(target)
      if old then
        fs.rename(target, target .. ".old")
      end
      fs.rename(tempPath, target)
      if old then
        fs.unlink(target .. ".old")
      end
      log("recursivly running update", target .. ' update', 'highlight')
      local child = spawn(target, {
        args = {"update"},
        stdio = {0, 1, 2}
      })
      return(child.waitExit())
    end
  end

  do
    local luvitPath = pathJoin(uv.exepath(), "..", "luvit")
    if isWindows then
      luvitPath = luvitPath .. ".exe"
    end
    if uv.fs_stat(luvitPath) then
      local bundle = require('luvi').makeBundle({luvitPath})
      updater.check(require('pkg').query({
        readFile = bundle.readfile,
        stat = bundle.stat,
      }, "."), luvitPath)
    else
      updater.check({ name = "luvit/luvit" }, luvitPath)
    end
  end

  do
    local target = pathJoin(uv.exepath(), "..", "luvi")
    if isWindows then
      target = target .. ".exe"
    end
    local new, old
    local toupdate = require('luvi').version
    if uv.fs_stat(target) then
      local stdout = exec(target, "-v")
      if isWindows then
        stdout = stdout:gsub("luvi.exe ","luvi ")
      end
      local version = stdout:match("luvi (v[^ \r\n]+)")
      if version and version == toupdate then
        log("luvi is up to date", version, "highlight")
        return
      end

      if version then
        log("found system luvi", version)
      end

      log("updating luvi", toupdate)
      new = target .. ".new"
      old = target .. ".old"
    else
      log("installing luvi binary", target, "highlight")
      old = nil
      new = target
    end

    local fd = assert(uv.fs_open(new, "w", 511)) -- 0777
    local source = core.getLuvi()
    local binSize = uv.fs_stat(source).size
    local fd2 = assert(uv.fs_open(source, "r", 384)) -- 0600
    assert(uv.fs_sendfile(fd, fd2, 0, binSize))
    uv.fs_close(fd2)
    uv.fs_close(fd)
    if old then
      log("replacing luvi binary", target, "highlight")
      uv.fs_rename(target, old)
      uv.fs_rename(new, target)
      uv.fs_unlink(old)
      log("luvi update complete", toupdate, "success")
    else
      log("luvi install complete", toupdate, "success")
    end
  end
end
