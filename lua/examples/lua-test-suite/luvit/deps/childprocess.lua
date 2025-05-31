--[[

Copyright 2014 The Luvit Authors. All Rights Reserved.

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
  name = "luvit/childprocess"
  version = "2.1.3"
  dependencies = {
    "luvit/core@2.0.0",
    "luvit/utils@2.0.0",
    "luvit/net@2.0.0",
    "luvit/timer@2.0.0",
  }
  license = "Apache 2"
  homepage = "https://github.com/luvit/luvit/blob/master/deps/childprocess.lua"
  description = "A port of node.js's childprocess module for luvit."
  tags = {"luvit", "spawn", "process"}
]]

local core = require('core')
local net = require('net')
local timer = require('timer')
local uv = require('uv')
local los = require('los')
local adapt = require('utils').adapt

local Error = core.Error

local Process = core.Emitter:extend()
function Process:initialize(stdin, stdout, stderr)
  self.stdout = stdout
  self.stdin = stdin
  self.stderr = stderr
end

function Process:setHandle(handle)
  self.handle = handle
end

function Process:setPid(pid)
  self.pid = pid
end

function Process:kill(signal)
  if self.handle and not uv.is_closing(self.handle) then uv.process_kill(self.handle, signal or 'sigterm') end
end

function Process:close(err)
  if self.handle and not uv.is_closing(self.handle) then uv.close(self.handle) end
  self:destroy(err)
end

function Process:destroy(err)
  if err then
    timer.setImmediate(function() self:emit('error', err) end)
  end
end

local function spawn(command, args, options)
  local envPairs = {}
  local em, onExit, handle, pid
  local stdout, stdin, stderr, stdio, closesGot

  args = args or {}
  options = options or {}
  options.detached = options.detached or false

  if options.env then
    for k, v in pairs(options.env) do
      table.insert(envPairs, k .. '=' .. v)
    end
  end

  local function maybeClose()
    closesGot = closesGot - 1
    if closesGot == 0 then
      em:emit('close', em.exitCode, em.signal)
    end
  end

  local function countStdio(stdio)
    local count = 0
    if stdio[1] then count = count + 1 end
    if stdio[2] then count = count + 1 end
    if stdio[3] then count = count + 1 end
    return count + 1 -- for exit call
  end

  if options.stdio then
    stdio = {}
    stdin = options.stdio[1]
    stdout = options.stdio[2]
    stderr = options.stdio[3]
    stdio[1] = options.stdio[1] and options.stdio[1]._handle
    stdio[2] = options.stdio[2] and options.stdio[2]._handle
    stdio[3] = options.stdio[3] and options.stdio[3]._handle
    if stdio[1] then options.stdio[1]:once('close', maybeClose) end
    if stdio[2] then options.stdio[2]:once('close', maybeClose) end
    if stdio[3] then options.stdio[3]:once('close', maybeClose) end
    closesGot = countStdio(stdio)
  else
    stdin = net.Socket:new({ handle = uv.new_pipe(false) })
    stdout = net.Socket:new({ handle = uv.new_pipe(false) })
    stderr = net.Socket:new({ handle = uv.new_pipe(false) })
    stdio = { stdin._handle, stdout._handle, stderr._handle}
    stdin:once('close', maybeClose)
    stdout:once('close', maybeClose)
    stderr:once('close', maybeClose)
    closesGot = countStdio(stdio)
  end

  function onExit(code, signal)
    em.exitCode = code
    em.signal = signal
    em:emit('exit', code, signal)
    if stdin then stdin:destroy(maybeClose) end
    maybeClose()
    em:close()
  end

  handle, pid = uv.spawn(command, {
    cwd = options.cwd or nil,
    stdio = stdio,
    args = args,
    env = envPairs,
    detached = options.detached,
    uid = options.uid,
    gid = options.gid,
    verbatim = options.verbatim,
  }, onExit)

  em = Process:new(stdin, stdout, stderr)
  em:setHandle(handle)
  em:setPid(pid)

  if not em.handle then
    timer.setImmediate(function()
      em.exitCode = -127
      em:emit('exit', em.exitCode)
      em:emit('error', Error:new(pid))
      if em.stdout then em.stdout:emit('error', Error:new(pid)) end
      if em.stderr then em.stderr:emit('error', Error:new(pid)) end
      if em.stdin then em.stdin:emit('error', Error:new(pid)) end
      if em.stdin then em.stdin:destroy() end
      maybeClose()
    end)
  end

  if stdout then stdout:resume() end
  if stderr then stderr:resume() end
  if stdin then stdin:resume() end

  return em
end

---- Exec and execfile

local function normalizeExecArgs(command, options, callback)
  if type(options) == 'function' or type(options) == 'thread' then
    callback = options
    options = {}
  end

  local isWindows
  if not pcall(function() isWindows = require('luvipath').isWindows end) then
    isWindows = los.type() == 'win32'
  end

  local file, args
  if isWindows then
    file = 'cmd.exe'
    args = {'/s', '/c', '"'..command..'"'}
    -- verbatim is necessary to avoid quotation marks getting escaped by Libuv
    options.verbatim = true
  else
    file = '/bin/sh'
    args = {'-c', command}
  end

  if options and options.shell then file = options.shell end

  return file, args, options, callback
end

local function _exec(file, args, options, callback)
  local opts = {
    timeout = 0,
    maxBuffer = 4 * 1024,
    signal = 'SIGTERM'
  }
  for k, v in pairs(opts) do
    if not options[k] then options[k] = v end
  end

  local child = spawn(file, args, options)

  local stdout, stderr = {}, {}
  local exited = false
  local stdoutLen, stderrLen = 0, 0
  local timeoutId
  local err = {}
  local called = 2

  local function exitHandler(code, signal)
    if timeoutId then
      timer.clearTimeout(timeoutId)
      timeoutId = nil
    end
    if exited then return end
    called = called - 1
    if called == 0 then
      if signal then err.signal = signal end
      if not code then
        err.message = 'Command failed: '..file
      elseif code == 0 then
        err = nil
      else
        err.code = code
        err.message = 'Command killed'
      end
      exited = true
      if not callback then return end
      callback(err, table.concat(stdout, ""), table.concat(stderr, ""))
    end
  end
  local function onClose(_exitCode)
    exitHandler(_exitCode, nil)
  end
  local function kill()
    child.stdout:emit('close', 1, options.signal)
    child.stderr:emit('close', 1, options.signal)
    child:emit('close', 1, options.signal)
    exitHandler(1, options.signal)
  end

  if options.timeout > 0 then
    timeoutId = timer.setTimeout(options.timeout, function()
      kill()
      timeoutId = nil
    end)
  end

  child.stdout:on('data', function(chunk)
    stdoutLen = stdoutLen + #chunk
    if stdoutLen > options.maxBuffer then
      kill()
    else
      table.insert(stdout, chunk)
    end
  end):once('end', exitHandler)

  child.stderr:on('data', function(chunk)
    stderrLen = stderrLen + #chunk
    if stderrLen > options.maxBuffer then
      kill()
    else
      table.insert(stderr, chunk)
    end
  end)

  child:once('close', onClose)

  return child
end

local function execFile(file, args, options, callback)
  -- Make callback, args and options optional
  -- no option or args
  if type(args) == 'function' or type(args) == 'thread' then
    callback = args
    args, options = {}, {}
    -- no options
  elseif type(options) == 'function' or type(options) == 'thread' then
    callback = options
    options = {}
    -- no options args or callback
  elseif not args and not options and not callback then
    callback = function() end
    options = {}
    args = {}
  elseif not options then
    options = {}
  elseif not args then
    args = {}
  end

  return adapt(callback, _exec, file, args, options)
end

local function exec(command, options, callback)
  if not callback and (type(options) == "thread" or type(options) == "function") then
    callback = options
    options = {}
  end

  -- options is optional
  return execFile(normalizeExecArgs(command, options, callback))
end

return {
  exec = exec,
  execFile = execFile,
  spawn = spawn,
}
