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
  name = "luvit/codec"
  version = "2.0.1"
  dependencies = {
    "luvit/utils@2.1.0",
  }
  license = "Apache 2"
  homepage = "https://github.com/luvit/luvit/blob/master/deps/codec.lua"
  description = "Utilities for working with luvit streams and codecs."
  tags = {"luvit", "codec", "stream"}
]]

local uv = require('uv')
local assertResume = require('utils').assertResume
local unpack = unpack or table.unpack ---@diagnostic disable-line: deprecated

local function wrapEmitter(emitter)
  local read, write
  local queue = {}

  -- Pipe data from chain to emitter
  do
    local paused = false
    local waiting

    function emitter:pause()
      paused = true
    end

    function emitter:resume()
      if not paused then return end
      paused = false
      if not waiting then return end
      local thread = waiting
      waiting = nil
      assertResume(thread)
    end

    function write(data)
      if paused then
        waiting = coroutine.running()
        coroutine.yield()
      end
      if data then
        emitter:emit("data", data)
      else
        emitter:emit("end")
      end
    end
  end

  -- Pipe data from emitter to chain
  do
    local waiting
    local ended = false

    function emitter:write(data)
      if waiting then
        local thread = waiting
        waiting = nil
        assertResume(thread, data)
        return true
      end
      queue[#queue + 1] = data
      return false
    end

    function emitter:shutdown()
      ended = true
      if waiting then
        local thread = waiting
        waiting = nil
        assertResume(thread)
      end
    end

    function read()
      if ended then return end
      if #queue > 0 then
        local data = table.remove(queue)
        if #queue == 0 then
          emitter:emit("drain")
        end
        return data
      end
      waiting = coroutine.running()
      return coroutine.yield()
    end
  end

  return read, write
end


-- Given a raw uv_stream_t userdara, return coro-friendly read/write functions.
local function wrapStream(socket)
  local paused = true
  local queue = {}
  local waiting
  local reading = true
  local writing = true

  local onRead

  local function read()
    if #queue > 0 then
      return unpack(table.remove(queue, 1))
    end
    if paused then
      paused = false
      uv.read_start(socket, onRead)
    end
    waiting = coroutine.running()
    return coroutine.yield()
  end

  function onRead(err, chunk)
    local data = err and {nil, err} or {chunk}
    if waiting then
      local thread = waiting
      waiting = nil
      assertResume(thread, unpack(data))
    else
      queue[#queue + 1] = data
      if not paused then
        paused = true
        uv.read_stop(socket)
      end
    end
    if not chunk then
      reading = false
      -- Close the whole socket if the writing side is also closed already.
      if not writing and not uv.is_closing(socket) then
        uv.close(socket)
      end
    end
  end

  local function write(chunk)
    if chunk == nil then
      -- Shutdown our side of the socket
      writing = false
      if not uv.is_closing(socket) then
        uv.shutdown(socket)
        -- Close if we're done reading too
        if not reading and not uv.is_closing(socket) then
          uv.close(socket)
        end
      end
    else
      -- TODO: add backpressure by pausing and resuming coroutine
      -- when write buffer is full.
      uv.write(socket, chunk)
    end
  end

  return read, write
end
local function chain(...)
  local args = {...}
  local nargs = select("#", ...)
  return function (read, write)
    local threads = {} -- coroutine thread for each item
    local waiting = {} -- flag when waiting to pull from upstream
    local boxes = {}   -- storage when waiting to write to downstream
    for i = 1, nargs do
      threads[i] = coroutine.create(args[i])
      waiting[i] = false
      local r, w
      if i == 1 then
        r = read
      else
        function r()
          local j = i - 1
          if boxes[j] then
            local data = boxes[j]
            boxes[j] = nil
            assertResume(threads[j])
            return unpack(data)
          else
            waiting[i] = true
            return coroutine.yield()
          end
        end
      end
      if i == nargs then
        w = write
      else
        function w(...)
          local j = i + 1
          if waiting[j] then
            waiting[j] = false
            assertResume(threads[j], ...)
          else
            boxes[i] = {...}
            coroutine.yield()
          end
        end
      end
      assertResume(threads[i], r, w)
    end
  end
end

return {
  wrapEmitter = wrapEmitter,
  wrapStream = wrapStream,
  chain = chain,
}
