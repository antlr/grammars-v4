--[[lit-meta
  name = "luvit/coro-fs"
  version = "2.2.5"
  homepage = "https://github.com/luvit/lit/blob/master/deps/coro-fs.lua"
  description = "A coro style interface to the filesystem."
  tags = {"coro", "fs"}
  license = "MIT"
  dependencies = {
    "luvit/pathjoin@2.0.0"
  }
  author = { name = "Tim Caswell" }
  contributors = {"Tim Caswell", "Alex Iverson"}
]]

local uv = require('uv')
local fs = {}
local pathJoin = require('pathjoin').pathJoin

local function assertResume(thread, ...)
  local success, err = coroutine.resume(thread, ...)
  if not success then
    error(debug.traceback(thread, err), 0)
  end
end

local function noop() end

local function makeCallback()
  local thread = coroutine.running()
  return function (err, value, ...)
    if err then
      assertResume(thread, nil, err)
    else
      assertResume(thread, value == nil and true or value, ...)
    end
  end
end

function fs.mkdir(path, mode)
  uv.fs_mkdir(path, mode or 511, makeCallback())
  return coroutine.yield()
end
function fs.open(path, flags, mode)
  uv.fs_open(path, flags or "r", mode or 438, makeCallback())
  return coroutine.yield()
end
function fs.unlink(path)
  uv.fs_unlink(path, makeCallback())
  return coroutine.yield()
end
function fs.stat(path)
  uv.fs_stat(path, makeCallback())
  return coroutine.yield()
end
function fs.lstat(path)
  uv.fs_lstat(path, makeCallback())
  return coroutine.yield()
end
function fs.symlink(target, path, flags)
  uv.fs_symlink(target, path, flags, makeCallback())
  return coroutine.yield()
end
function fs.readlink(path)
  uv.fs_readlink(path, makeCallback())
  return coroutine.yield()
end
function fs.fstat(fd)
  uv.fs_fstat(fd, makeCallback())
  return coroutine.yield()
end
function fs.chmod(path, mode)
  uv.fs_chmod(path, mode, makeCallback())
  return coroutine.yield()
end
function fs.fchmod(fd, mode)
  uv.fs_fchmod(fd, mode, makeCallback())
  return coroutine.yield()
end
function fs.read(fd, length, offset)
  uv.fs_read(fd, length or 1024*48, offset or -1, makeCallback())
  return coroutine.yield()
end
function fs.write(fd, data, offset)
  uv.fs_write(fd, data, offset or -1, makeCallback())
  return coroutine.yield()
end
function fs.close(fd)
  uv.fs_close(fd, makeCallback())
  return coroutine.yield()
end
function fs.access(path, mode)
  uv.fs_access(path, mode or "", makeCallback())
  return coroutine.yield()
end
function fs.rename(path, newPath)
  uv.fs_rename(path, newPath, makeCallback())
  return coroutine.yield()
end
function fs.rmdir(path)
  uv.fs_rmdir(path, makeCallback())
  return coroutine.yield()
end
function fs.rmrf(path)
  local success, err
  success, err = fs.rmdir(path)
  if success then return success end
  if err:match("^ENOTDIR:") then return fs.unlink(path) end
  if not err:match("^ENOTEMPTY:") then return success, err end
  for entry in assert(fs.scandir(path)) do
    local subPath = pathJoin(path, entry.name)
    if entry.type == "directory" then
      success, err = fs.rmrf(pathJoin(path, entry.name))
    else
      success, err = fs.unlink(subPath)
    end
    if not success then return success, err end
  end
  return fs.rmdir(path)
end
function fs.scandir(path)
  uv.fs_scandir(path, makeCallback())
  local req, err = coroutine.yield()
  if not req then return nil, err end
  return function ()
    local name, typ = uv.fs_scandir_next(req)
    if not name then return name, typ end
    if type(name) == "table" then return name end
    return {
      name = name,
      type = typ
    }
  end
end

function fs.readFile(path)
  local fd, stat, data, err
  fd, err = fs.open(path)
  if err then return nil, err end
  stat, err = fs.fstat(fd)
  if stat then
    --special case files on virtual filesystem
    if stat.size == 0 and stat.birthtime.sec == 0 and stat.birthtime.nsec == 0 then
      -- handle magic files the kernel generates as requested.
      -- hopefully the heuristic works everywhere
      local buffs = {}
      local offs = 0
      local size = 1024 * 48
      repeat
        data, err = fs.read(fd, size, offs)
        table.insert(buffs, data)
        offs = offs + (data and #data or 0)
      until err or #data < size
      if not err then
        data = table.concat(buffs)
      end
    else
      -- normal case for normal files.
      data, err = fs.read(fd, stat.size)
    end
  end
  uv.fs_close(fd, noop)
  return data, err
end

function fs.writeFile(path, data, mkdir)
  local fd, success, err
  fd, err = fs.open(path, "w")
  if err then
    if mkdir and string.match(err, "^ENOENT:") then
      success, err = fs.mkdirp(pathJoin(path, ".."))
      if success then return fs.writeFile(path, data) end
    end
    return nil, err
  end
  success, err = fs.write(fd, data)
  uv.fs_close(fd, noop)
  return success, err
end

function fs.mkdirp(path, mode)
  local success, err = fs.mkdir(path, mode)
  if success or string.match(err, "^EEXIST") then
    return true
  end
  if string.match(err, "^ENOENT:") then
    success, err = fs.mkdirp(pathJoin(path, ".."), mode)
    if not success then return nil, err end
    return fs.mkdir(path, mode)
  end
  return nil, err
end

function fs.chroot(base)
  local chroot = {
    base = base,
    fstat = fs.fstat,
    fchmod = fs.fchmod,
    read = fs.read,
    write = fs.write,
    close = fs.close,
  }
  local function resolve(path)
    assert(path, "path missing")
    return pathJoin(base, pathJoin("./".. path))
  end
  function chroot.mkdir(path, mode)
    return fs.mkdir(resolve(path), mode)
  end
  function chroot.mkdirp(path, mode)
    return fs.mkdirp(resolve(path), mode)
  end
  function chroot.open(path, flags, mode)
    return fs.open(resolve(path), flags, mode)
  end
  function chroot.unlink(path)
    return fs.unlink(resolve(path))
  end
  function chroot.stat(path)
    return fs.stat(resolve(path))
  end
  function chroot.lstat(path)
    return fs.lstat(resolve(path))
  end
  function chroot.symlink(target, path, flags)
    -- TODO: should we resolve absolute target paths or treat it as opaque data?
    return fs.symlink(target, resolve(path), flags)
  end
  function chroot.readlink(path)
    return fs.readlink(resolve(path))
  end
  function chroot.chmod(path, mode)
    return fs.chmod(resolve(path), mode)
  end
  function chroot.access(path, mode)
    return fs.access(resolve(path), mode)
  end
  function chroot.rename(path, newPath)
    return fs.rename(resolve(path), resolve(newPath))
  end
  function chroot.rmdir(path)
    return fs.rmdir(resolve(path))
  end
  function chroot.rmrf(path)
    return fs.rmrf(resolve(path))
  end
  function chroot.scandir(path, iter)
    return fs.scandir(resolve(path), iter)
  end
  function chroot.readFile(path)
    return fs.readFile(resolve(path))
  end
  function chroot.writeFile(path, data, mkdir)
    return fs.writeFile(resolve(path), data, mkdir)
  end
  return chroot
end

return fs
