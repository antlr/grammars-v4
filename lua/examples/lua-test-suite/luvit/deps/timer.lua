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
  name = "luvit/timer"
  version = "2.0.2"
  dependencies = {
    "luvit/core@2.0.0",
    "luvit/utils@2.1.0",
  }
  license = "Apache 2"
  homepage = "https://github.com/luvit/luvit/blob/master/deps/timer.lua"
  description = "Javascript style setTimeout and setInterval for luvit"
  tags = {"luvit", "timer"}
]]

local uv = require('uv')
local Object = require('core').Object
local bind = require('utils').bind
local assertResume = require('utils').assertResume
local unpack = unpack or table.unpack ---@diagnostic disable-line: deprecated

-------------------------------------------------------------------------------

local Timer = Object:extend()

function Timer:initialize()
  self._handle = uv.new_timer()
  self._active = false
end

function Timer:_update()
  self._active = uv.is_active(self._handle)
end

-- Timer:start(timeout, interval, callback)
function Timer:start(timeout, interval, callback)
  uv.timer_start(self._handle, timeout, interval, callback)
  self:_update()
end

-- Timer:stop()
function Timer:stop()
  uv.timer_stop(self._handle)
  self:_update()
end

-- Timer:again()
function Timer:again()
  uv.timer_again(self._handle)
  self:_update()
end

-- Timer:close()
function Timer:close()
  uv.close(self._handle)
  self:_update()
end

-- Timer:setRepeat(interval)
Timer.setRepeat = uv.timer_set_repeat

-- Timer:getRepeat()
Timer.getRepeat = uv.timer_get_repeat

-- Timer.now
Timer.now = uv.now

------------------------------------------------------------------------------

local function sleep(delay, thread)
  thread = thread or coroutine.running()
  local timer = uv.new_timer()
  uv.timer_start(timer, delay, 0, function ()
    uv.timer_stop(timer)
    uv.close(timer)
    return assertResume(thread)
  end)
  return coroutine.yield()
end

local function setTimeout(delay, callback, ...)
  local timer = uv.new_timer()
  local args = {...}
  uv.timer_start(timer, delay, 0, function ()
    uv.timer_stop(timer)
    uv.close(timer)
    callback(unpack(args))
  end)
  return timer
end

local function setInterval(interval, callback, ...)
  local timer = uv.new_timer()
  uv.timer_start(timer, interval, interval, bind(callback, ...))
  return timer
end

local function clearInterval(timer)
  if uv.is_closing(timer) then return end
  uv.timer_stop(timer)
  uv.close(timer)
end


local checker = uv.new_check()
local idler = uv.new_idle()
local immediateQueue = {}

local function onCheck()
  local queue = immediateQueue
  immediateQueue = {}
  for i = 1, #queue do
    queue[i]()
  end
  -- If the queue is still empty, we processed them all
  -- Turn the check hooks back off.
  if #immediateQueue == 0 then
    uv.check_stop(checker)
    uv.idle_stop(idler)
  end
end

local function setImmediate(callback, ...)

  -- If the queue was empty, the check hooks were disabled.
  -- Turn them back on.
  if #immediateQueue == 0 then
    uv.check_start(checker, onCheck)
    uv.idle_start(idler, onCheck)
  end

  immediateQueue[#immediateQueue + 1] = bind(callback, ...)
end

------------------------------------------------------------------------------

local lists = {}

local function init(list)
  list._idleNext = list
  list._idlePrev = list
end

local function peek(list)
  if list._idlePrev == list then
    return nil
  end
  return list._idlePrev
end

local function remove(item)
  if item._idleNext then
    item._idleNext._idlePrev = item._idlePrev
  end

  if item._idlePrev then
    item._idlePrev._idleNext = item._idleNext
  end

  item._idleNext = nil
  item._idlePrev = nil
end

local function append(list, item)
  remove(item)
  item._idleNext = list._idleNext
  list._idleNext._idlePrev = item
  item._idlePrev = list
  list._idleNext = item
end

local function isEmpty(list)
  return list._idleNext == list
end

local expiration
expiration = function(timer, msecs)
  return function()
    local now = Timer.now()
    while peek(timer) do
      local elem = peek(timer)
      local diff = now - elem._idleStart;
      if ((diff + 1) < msecs) == true then
        timer:start(msecs - diff, 0, expiration(timer, msecs))
        return
      else
        remove(elem)
        if elem.emit then
          elem:emit('timeout')
        end
      end
    end

    -- Remove the timer if it wasn't already
    -- removed by unenroll
    local list = lists[msecs]
    if list and isEmpty(list) then
      list:stop()
      list:close()
      lists[msecs] = nil
    end
  end
end


local function _insert(item, msecs)
  item._idleStart = Timer.now()
  item._idleTimeout = msecs

  if msecs < 0 then return end

  local list

  if lists[msecs] then
    list = lists[msecs]
  else
    list = Timer:new()
    init(list)
    list:start(msecs, 0, expiration(list, msecs))
    lists[msecs] = list
  end

  append(list, item)
end

local function unenroll(item)
  remove(item)
  local list = lists[item._idleTimeout]
  if list and isEmpty(list) then
    -- empty list
    list:stop()
    list:close()
    lists[item._idleTimeout] = nil
  end
  item._idleTimeout = -1
end

-- does not start the timer, just initializes the item
local function enroll(item, msecs)
  if item._idleNext then
    unenroll(item)
  end
  item._idleTimeout = msecs
  init(item)
end

-- call this whenever the item is active (not idle)
local function active(item)
  local msecs = item._idleTimeout
  if msecs and msecs >= 0 then
    local list = lists[msecs]
    if not list or isEmpty(list) then
      _insert(item, msecs)
    else
      item._idleStart = Timer.now()
      append(lists[msecs], item)
    end
  end
end

return {
  sleep = sleep,
  setTimeout = setTimeout,
  setInterval = setInterval,
  clearInterval = clearInterval,
  clearTimeout = clearInterval,
  clearTimer = clearInterval, -- Luvit 1.x compatibility
  setImmediate = setImmediate,
  unenroll = unenroll,
  enroll = enroll,
  active = active,
}
