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

--[[lit-meta
  name = "luvit/net"
  version = "2.0.4"
  dependencies = {
    "luvit/timer@2.0.0",
    "luvit/utils@2.0.0",
    "luvit/core@2.0.0",
    "luvit/stream@2.0.0",
  }
  license = "Apache 2"
  homepage = "https://github.com/luvit/luvit/blob/master/deps/net.lua"
  description = "Node-style net client and server module for luvit"
  tags = {"luvit", "tcp", "pipe", "stream"}
]]

local uv = require('uv')
local timer = require('timer')
local utils = require('utils')
local Emitter = require('core').Emitter
local Duplex = require('stream').Duplex

--[[ Socket ]]--

local Socket = Duplex:extend()
function Socket:initialize(options)
  Duplex.initialize(self)
  if type(options) == 'number' then
    options  = { fd = options }
  elseif options == nil then
    options = {}
  end

  if options.handle then
    self._handle = options.handle
  elseif options.fd then
    local typ = uv.guess_handle(options.fd);
    if typ == 'TCP' then
      self._handle = uv.new_tcp()
    elseif typ == 'PIPE' then
      self._handle = uv.new_pipe()
    end
  end

  self._connecting = false
  self._reading = false
  self._destroyed = false

  self:on('finish', utils.bind(self._onSocketFinish, self))
  self:on('_socketEnd', utils.bind(self._onSocketEnd, self))
end

function Socket:_onSocketFinish()
  if self._connecting then
    return self:once('connect', utils.bind(self._onSocketFinish, self))
  end
  if not self.readable then
    return self:destroy()
  end
end

function Socket:_onSocketEnd()
  self:once('end', function()
    self:destroy()
  end)
end

function Socket:bind(ip, port)
  uv.tcp_bind(self._handle, ip, tonumber(port))
end

function Socket:address()
  return uv.tcp_getpeername(self._handle)
end

function Socket:setTimeout(msecs, callback)
  if msecs > 0 then
    timer.enroll(self, msecs)
    timer.active(self)
    if callback then self:once('timeout', callback) end
  elseif msecs == 0 then
    timer.unenroll(self)
  end
end

function Socket:_write(data, callback)
  if not self._handle then return end
  uv.write(self._handle, data, function(err)
    if err then
      self:destroy(err)
      return callback(err)
    end
    callback()
  end)
end

function Socket:_read(n)
  local onRead

  function onRead(err, data)
    timer.active(self)
    if err then
      return self:destroy(err)
    elseif data then
      self:push(data)
    else
      self:push(nil)
      self:emit('_socketEnd')
    end
  end

  if self._connecting then
    self:once('connect', utils.bind(self._read, self, n))
  elseif not self._reading then
    self._reading = true
    uv.read_start(self._handle, onRead)
  end
end

function Socket:shutdown(callback)
  if self.destroyed == true and callback then
    return callback()
  end

  if uv.is_closing(self._handle) and callback then
    return callback()
  end

  uv.shutdown(self._handle, callback)
end

function Socket:nodelay(enable)
  uv.tcp_nodelay(self._handle, enable)
end

function Socket:keepalive(enable, delay)
  uv.tcp_keepalive(self._handle, enable, delay)
end

function Socket:getSendBufferSize()
  return uv.send_buffer_size(self._handle)
end

function Socket:getRecvBufferSize()
  return uv.recv_buffer_size(self._handle)
end

function Socket:setSendBufferSize(size)
  assert(type(size) == "number" and size > 0, "Size must be a number greater than 0")
  return uv.send_buffer_size(self._handle, size)
end

function Socket:setRecvBufferSize(size)
  assert(type(size) == "number" and size > 0, "Size must be a number greater than 0")
  return uv.recv_buffer_size(self._handle, size)
end

function Socket:pause()
  Duplex.pause(self)
  if not self._handle then return end
  self._reading = false
  uv.read_stop(self._handle)
end

function Socket:resume()
  Duplex.resume(self)
  self:_read(0)
end

function Socket:connect(...)
  local args = {...}
  local options = {}
  local callback

  if type(args[1]) == 'table' then
    -- connect(options, [cb])
    options = args[1]
    callback = args[2]
  else
    -- connect(port, [host], [cb])
    options.port = args[1]
    if type(args[2]) == 'string' then
      options.host = args[2];
      callback = args[3]
    else
      callback = args[2]
    end
  end

  callback = callback or function() end

  if not options.host then
    options.host = '0.0.0.0'
  end

  timer.active(self)
  self._connecting = true

  if not self._handle then
    self._handle = uv.new_tcp()
  end

  local _, derr = uv.getaddrinfo(options.host, options.port, { socktype = "stream" }, function(err, res)
    timer.active(self)
    if err then
      return self:destroy(err)
    end
    if self.destroyed then return end
    local _, terr = uv.tcp_connect(self._handle, res[1].addr, res[1].port, function(err)
      if err then
        return self:destroy(err)
      end
      timer.active(self)
      self._connecting = false
      self:emit('connect')
      if callback then callback() end
    end)
    if terr ~= nil then
      self:destroy(terr)
    end
  end)
  if derr ~= nil then
    return self:destroy(derr)
  end

  return self
end

function Socket:destroy(exception, callback)
  callback = callback or function() end
  if self.destroyed == true or self._handle == nil then
    return callback()
  end

  timer.unenroll(self)
  self.destroyed = true
  self.readable = false
  self.writable = false

  if uv.is_closing(self._handle) then
    timer.setImmediate(callback)
  else
    uv.close(self._handle, function()
      self:emit('close')
      callback()
    end)
  end

  if exception then
    process.nextTick(function()
      self:emit('error', exception)
    end)
  end
end

function Socket:listen(queueSize)
  local onListen
  queueSize = queueSize or 128
  function onListen()
    local client = uv.new_tcp()
    uv.tcp_keepalive(client, true, 60)
    uv.accept(self._handle, client)
    self:emit('connection', Socket:new({ handle = client }))
  end
  uv.listen(self._handle, queueSize, onListen)
end

function Socket:getsockname()
  return uv.tcp_getsockname(self._handle)
end

--[[ Server ]]--

local Server = Emitter:extend()
function Server:init(options, connectionListener)
  if type(options) == 'function' then
    connectionListener = options
    options = {}
  end

  self._connectionListener = connectionListener
  self:on('connection', self._connectionListener)

  if options.handle then
    self._handle = options.handle
  end
end

function Server:destroy(err, callback)
  self._handle:destroy(err, callback)
end

function Server:listen(port, ... --[[ ip, callback --]] )
  local args = {...}
  local ip, callback

  if not self._handle then
    self._handle = Socket:new({ handle = uv.new_tcp() })
  end

  -- Future proof
  if type(args[1]) == 'function' then
    callback = args[1]
  else
    ip = args[1]
    callback = args[2]
  end

  ip = ip or '0.0.0.0'

  self._handle:bind(ip, port)
  self._handle:listen()
  self._handle:on('connection', function(client)
    self:emit('connection', client)
  end)

  if callback then
    timer.setImmediate(callback)
  end

  return self
end

function Server:address()
  if self._handle then
    return self._handle:getsockname()
  end
  return
end

function Server:close(callback)
  self:destroy(nil, callback)
end

-- Exports

local function createConnection(port, ... --[[ host, cb --]])
  local args = {...}
  local host
  local options
  local callback
  local sock

  -- future proof
  if type(port) == 'table' then
    options = port
    port = options.port
    host = options.host
    callback = args[1]
  else
    host = args[1]
    callback = args[2]
  end

  sock = Socket:new()
  sock:connect(port, host, callback)
  return sock
end

local function createServer(options, connectionListener)
  local server = Server:new()
  server:init(options, connectionListener)
  return server
end

return {
  Server = Server,
  Socket = Socket,
  createConnection = createConnection,
  connect = createConnection,
  createServer = createServer,
}
