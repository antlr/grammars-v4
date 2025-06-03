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
  name = "luvit/utils"
  version = "2.1.0"
  dependencies = {
    "luvit/pretty-print@2.0.0",
  }
  license = "Apache 2"
  homepage = "https://github.com/luvit/luvit/blob/master/deps/utils.lua"
  description = "Wrapper around pretty-print with extra tools for luvit"
  tags = {"luvit", "bind", "adapter"}
]]

local unpack = unpack or table.unpack ---@diagnostic disable-line: deprecated
local Error = require('core').Error
local utils = {}
local pp = require('pretty-print')
for name, value in pairs(pp) do
  utils[name] = value
end

local function assertResume(thread, ...)
  local success, err = coroutine.resume(thread, ...)
  if not success then
    error(debug.traceback(thread, err), 0)
  end
end

local function bind(fn, self, ...)
  assert(fn, "fn is nil")
  local bindArgsLength = select("#", ...)

  -- Simple binding, just inserts self (or one arg or any kind)
  if bindArgsLength == 0 then
    return function (...)
      return fn(self, ...)
    end
  end

  -- More complex binding inserts arbitrary number of args into call.
  local bindArgs = {...}
  return function (...)
    local argsLength = select("#", ...)
    local args = {...}
    local arguments = {}
    for i = 1, bindArgsLength do
      arguments[i] = bindArgs[i]
    end
    for i = 1, argsLength do
      arguments[i + bindArgsLength] = args[i]
    end
    return fn(self, unpack(arguments, 1, bindArgsLength + argsLength))
  end
end

local function noop(err)
  if err then print("Unhandled callback error", err) end
end

local function adapt(c, fn, ...)
  local nargs = select('#', ...)
  local args = {...}
  -- No continuation defaults to noop callback
  if not c then c = noop end
  local t = type(c)
  if t == 'function' then
    args[nargs + 1] = c
    return fn(unpack(args))
  elseif t ~= 'thread' then
    error("Illegal continuation type " .. t)
  end
  local err, data, waiting
  args[nargs + 1] = function (e, ...)
    if waiting then
      if e then
        assertResume(c, nil, e)
      else
        assertResume(c, ...)
      end
    else
      err, data = e and Error:new(e), {...}
      c = nil
    end
  end
  fn(unpack(args))
  if c then
    waiting = true
    return coroutine.yield()
  elseif err then
    return nil, err
  else
    return unpack(data)
  end
end

utils.bind = bind
utils.noop = noop
utils.adapt = adapt
utils.assertResume = assertResume

return utils
