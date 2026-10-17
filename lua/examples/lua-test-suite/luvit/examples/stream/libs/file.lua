-- This is a partial port of the built-in fs module as continuable format

local native = require('uv')
local Emitter = require('core').Emitter

local iStream = Emitter:extend()

local fs = {}

fs.umask = "0644"

local function noop() end

function fs.open(path, flag, mode)
  if type(path) ~= "string" then
    error("open(path, flag, [mode]): path must be a string")
  end
  if type(flag) ~= "string" then
    error("open(path, flag, [mode]): flag must be a string")
  end
  if not mode then mode = fs.umask end
  if type(mode) ~= "string" then
    error("open(path, flag, [mode]): mode must be a string")
  end
  return function (callback)
    return native.fs_open(path, flag, mode, callback or noop)
  end
end

function fs.read(fd, offset, length)
  if type(fd) ~= "number" then
    error("read(fd, offset, length): fd must be a number")
  end
  if type(offset) ~= "number" then
    error("read(fd, offset, length): offset must be a number")
  end
  if type(length) ~= "number" then
    error("read(fd, offset, length): length must be a number")
  end
  return function (callback)
    return native.fs_read(fd, offset, length, callback or noop)
  end
end

function fs.write(fd, offset, chunk)
  if type(fd) ~= "number" then
    error("write(fd, offset, chunk): fd must be a number")
  end
  if type(offset) ~= "number" then
    error("write(fd, offset, chunk): offset must be a number")
  end
  if type(chunk) ~= "string" then
    error("write(fd, offset, chunk): chunk must be a string")
  end
  return function (callback)
    return native.fs_write(fd, offset, chunk, callback or noop)
  end
end

function fs.close(fd)
  if type(fd) ~= "number" then
    error("close(fd): fd must be a number")
  end
  return function (callback)
    return native.fs_close(fd, callback or noop)
  end
end

local ReadStream = iStream:extend()
fs.ReadStream = ReadStream

function ReadStream:initialize(fd)
  self.fd = fd
  self.offset = 0
  self.chunkSize = 10
end

function ReadStream:read()
  return function (callback)
    fs.read(self.fd, self.offset, self.chunkSize)(function (err, chunk)
      if err then
        return callback(err)
      end
      local bytesRead = #chunk
      if bytesRead == 0 then
        return callback()
      end
      self.offset = self.offset + bytesRead
      callback(nil, chunk)
    end)
  end
end

local WriteStream = iStream:extend()
fs.WriteStream = WriteStream

function WriteStream:initialize(fd)
  self.fd = fd
  self.offset = 0
end

function WriteStream:write(chunk)
  return function (callback)
    if not chunk then
      return callback()
    end
    fs.write(self.fd, self.offset, chunk)(function (err, bytesWritten)
      if err then
        return callback(err)
      end
      self.offset = self.offset + bytesWritten
      callback()
    end)
  end
end

return fs
