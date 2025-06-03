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

local core = require('core')
local Stream = require('./stream_core').Stream
local Error = core.Error

local onwrite, writeAfterEnd, validChunk, writeOrBuffer, clearBuffer,
  decodeChunk, doWrite, onwriteError, onwriteStateUpdate, needFinish,
  afterWrite, finishMaybe, onwriteDrain, endWritable, prefinish

local WriteReq = core.Object:extend()

function WriteReq:initialize(chunk, cb)
  self.chunk = chunk
  self.callback = cb
end


local WritableState = core.Object:extend()

function WritableState:initialize(options, stream)
  options = options or {}

  --[[
  // object stream flag to indicate whether or not this stream
  // contains buffers or objects.
  --]]
  self.objectMode = not not options.objectMode

  if core.instanceof(stream, require('./stream_duplex').Duplex) then
    self.objectMode = self.objectMode or not not options.writableObjectMode
  end

  --[[
  // the point at which write() starts returning false
  // Note: 0 is a valid value, means that we always return false if
  // the entire buffer is not flushed immediately on write()
  --]]
  local hwm = options.highWaterMark
  local defaultHwm
  if self.objectMode then
    defaultHwm = 16
  else
    defaultHwm = 16 * 1024
  end
  self.highWaterMark = hwm or defaultHwm

  --[[
  // cast to ints.
  this.highWaterMark = ~~this.highWaterMark
  --]]

  self.needDrain = false
  --[[
  // at the start of calling end()
  --]]
  self.ending = false
  --[[
  // when end() has been called, and returned
  --]]
  self.ended = false
  --[[
  // when 'finish' is emitted
  --]]
  self.finished = false

  --[[
  // should we decode strings into buffers before passing to _write?
  // this is here so that some node-core streams can optimize string
  // handling at a lower level.
  local noDecode = options.decodeStrings == false
  self.decodeStrings = not noDecode
  --]]

  --[[
  // not an actual buffer we keep track of, but a measurement
  // of how much we're waiting to get pushed to some underlying
  // socket or file.
  --]]
  self.length = 0

  --[[
  // a flag to see when we're in the middle of a write.
  --]]
  self.writing = false

  --[[
  // when true all writes will be buffered until .uncork() call
  --]]
  self.corked = 0

  --[[
  // a flag to be able to tell if the onwrite cb is called immediately,
  // or on a later tick.  We set this to true at first, because any
  // actions that shouldn't happen until "later" should generally also
  // not happen before the first write call.
  --]]
  self.sync = true

  --[[
  // a flag to know if we're processing previously buffered items, which
  // may call the _write() callback in the same tick, so that we don't
  // end up in an overlapped onwrite situation.
  --]]
  self.bufferProcessing = false

  --[[
  // the callback that's passed to _write(chunk,cb)
  --]]
  self.onwrite = function(er)
    onwrite(stream, er)
  end

  --[[
  // the callback that the user supplies to write(chunk,encoding,cb)
  --]]
  self.writecb = nil

  --[[
  // the amount that is being written when _write is called.
  --]]
  self.writelen = 0

  self.buffer = {}

  --[[
  // number of pending user-supplied write callbacks
  // this must be 0 before 'finish' can be emitted
  --]]
  self.pendingcb = 0

  --[[
  // emit prefinish if the only thing we're waiting for is _write cbs
  // This is relevant for synchronous Transform streams
  --]]
  self.prefinished = false

  --[[
  // True if the error was already emitted and should not be thrown again
  --]]
  self.errorEmitted = false
end


local Writable = Stream:extend()

function Writable:initialize(options)
  --[[
  // Writable ctor is applied to Duplexes, though they're not
  // instanceof Writable, they're instanceof Readable.
  if (!(this instanceof Writable) && !(this instanceof Stream.Duplex))
    return new Writable(options)
  --]]

  self._writableState = WritableState:new(options, self)

  if type(Stream.initialize) == 'function' then
    Stream.initialize(self)
  end
end

--[[
// Otherwise people can pipe Writable streams, which is just wrong.
--]]
function Writable:pipe()
  self:emit('error', Error:new('Cannot pipe. Not readable.'))
end


function writeAfterEnd(stream, state, cb)
  local er = Error:new('write after end')
  --[[
  // TODO: defer error events consistently everywhere, not just the cb
  --]]
  stream:emit('error', er)
  process.nextTick(function()
    cb(er)
  end)
end

--[[
// If we get something that is not a buffer, string, null, or undefined,
// and we're not in objectMode, then that's an error.
// Otherwise stream chunks are all considered to be of length=1, and the
// watermarks determine how many objects to keep in the buffer, rather than
// how many bytes or characters.
--]]
function validChunk(stream, state, chunk, cb)
  local valid = true
  if chunk ~= nil and type(chunk) ~= 'string' and not state.objectMode then
    local er = Error:new('Invalid non-string/buffer chunk')
    stream:emit('error', er)
    process.nextTick(function()
      cb(er)
    end)
    valid = false
  end
  return valid
end

function Writable:write(chunk, cb)
  local state = self._writableState
  local ret = false

  if type(cb) ~= 'function' then
    cb = function() end
  end

  if state.ended then
    writeAfterEnd(self, state, cb)
  elseif validChunk(self, state, chunk, cb) then
    state.pendingcb = state.pendingcb + 1
    ret = writeOrBuffer(self, state, chunk, cb)
  end

  return ret
end

function Writable:cork()
  local state = self._writableState

  state.corked = state.corked + 1
end

function Writable:uncork()
  local state = self._writableState

  if state.corked ~= 0 then
    state.corked = state.corked - 1

    if not state.writing and
        state.corked == 0 and
        not state.finished and
        not state.bufferProcessing and
        #state.buffer ~= 0 then
      clearBuffer(self, state)
    end
  end
end

function decodeChunk(state, chunk)

--[[
  if (!state.objectMode &&
      state.decodeStrings !== false &&
      util.isString(chunk)) {
    chunk = new Buffer(chunk)
  }
--]]
  return chunk
end

--[[
// if we're already writing something, then just put this
// in the queue, and wait our turn.  Otherwise, call _write
// If we return false, then we need a drain event, so set that flag.
--]]
function writeOrBuffer(stream, state, chunk, cb)
  chunk = decodeChunk(state, chunk)
  --[[
  if (util.isBuffer(chunk))
    encoding = 'buffer'
  --]]
  local len
  if state.objectMode then
    len = 1
  else
    len = string.len(chunk)
  end

  state.length = state.length + len

  local ret = state.length < state.highWaterMark
  --[[
  // we must ensure that previous needDrain will not be reset to false.
  --]]
  if not ret then
    state.needDrain = true
  end

  if state.writing or state.corked ~= 0 then
    table.insert(state.buffer, WriteReq:new(chunk, cb))
  else
    doWrite(stream, state, false, len, chunk, cb)
  end

  return ret
end

function doWrite(stream, state, writev, len, chunk, cb)
  state.writelen = len
  state.writecb = cb
  state.writing = true
  state.sync = true
  if writev then
    stream:_writev(chunk, state.onwrite)
  else
    stream:_write(chunk, state.onwrite)
  end
  state.sync = false
end

function onwriteError(stream, state, sync, er, cb)
  if sync then
    process.nextTick(function()
      state.pendingcb = state.pendingcb - 1
      cb(er)
    end)
  else
    state.pendingcb = state.pendingcb - 1
    cb(er)
  end

  stream._writableState.errorEmitted = true
  stream:emit('error', er)
end

function onwriteStateUpdate(state)
  state.writing = false
  state.writecb = nil
  state.length = state.length - state.writelen
  state.writelen = 0
end

function onwrite(stream, er)
  local state = stream._writableState
  local sync = state.sync
  local cb = state.writecb

  onwriteStateUpdate(state)

  if er then
    onwriteError(stream, state, sync, er, cb)
  else
    --[[
    // Check if we're actually ready to finish, but don't emit yet
    --]]
    local finished = needFinish(stream, state)

    if not finished and
        state.corked == 0 and
        not state.bufferProcessing and
        #state.buffer ~= 0 then
      clearBuffer(stream, state)
    end

    if sync then
      process.nextTick(function()
        afterWrite(stream, state, finished, cb)
      end)
    else
      afterWrite(stream, state, finished, cb)
    end
  end
end

function afterWrite(stream, state, finished, cb)
  if not finished then
    onwriteDrain(stream, state)
  end
  state.pendingcb = state.pendingcb - 1
  cb()
  finishMaybe(stream, state)
end

--[[
// Must force callback to be called on nextTick, so that we don't
// emit 'drain' before the write() consumer gets the 'false' return
// value, and has a chance to attach a 'drain' listener.
--]]
function onwriteDrain(stream, state)
  if state.length == 0 and state.needDrain then
    state.needDrain = false
    stream:emit('drain')
  end
end


--[[
// if there's something in the buffer waiting, then process it
--]]
function clearBuffer(stream, state)
  state.bufferProcessing = true

  if stream._writev and #state.buffer > 1 then
    --[[
    // Fast case, write everything using _writev()
    --]]
    local cbs = {}
    for c = 1, #state.buffer do
      table.insert(cbs, state.buffer[c].callback)
    end

    --[[
    // count the one we are adding, as well.
    // TODO(isaacs) clean this up
    --]]
    state.pendingcb = state.pendingcb + 1
    doWrite(stream, state, true, state.length, state.buffer, function(err)
      for i = 1, #cbs do
        state.pendingcb = state.pendingcb - 1
        cbs[i](err)
      end
    end)

    --[[
    // Clear buffer
    --]]
    state.buffer = {}
  else
    --[[
    // Slow case, write chunks one-by-one
    --]]
    local c = 1
    while c <= #state.buffer do
      local entry = state.buffer[c]
      local chunk = entry.chunk
      local cb = entry.callback
      local len
      if state.objectMode then
        len = 1
      else
        len =string.len(chunk)
      end

      doWrite(stream, state, false, len, chunk, cb)

      --[[
      // if we didn't call the onwrite immediately, then
      // it means that we need to wait until it does.
      // also, that means that the chunk and cb are currently
      // being processed, so move the buffer counter past them.
      --]]
      if state.writing then
        c = c + 1
        break
      end
      c = c + 1
    end

    if c <= #state.buffer then
      -- node.js: state.buffer = state.buffer.slice(c)
      for i=1,c-1 do
        table.remove(state.buffer, 1)
      end
    else
      state.buffer = {}
    end
  end

  state.bufferProcessing = false
end

function Writable:_write(chunk, cb)
  cb(Error:new('not implemented'))
end

Writable._writev = nil

function Writable:_end(chunk, cb)
  local state = self._writableState

  if type(chunk) == 'function' then
    cb = chunk
    chunk = nil
  end

  if chunk ~= nil then
    self:write(chunk)
  end

  --[[
  // .end() fully uncorks
  --]]
  if state.corked ~= 0 then
    state.corked = 1
    self:uncork()
  end

  --[[
  // ignore unnecessary end() calls.
  --]]
  if not state.ending and not state.finished then
    endWritable(self, state, cb)
  end
end


function needFinish(stream, state)
  return state.ending and state.length == 0 and #state.buffer == 0 and
  not state.finished and not state.writing
end

function prefinish(stream, state)
  if not state.prefinished then
    state.prefinished = true
    stream:emit('prefinish')
  end
end

function finishMaybe(stream, state)
  local need = needFinish(stream, state)
  if need then
    if state.pendingcb == 0 then
      prefinish(stream, state)
      state.finished = true
      stream:emit('finish')
    else
      prefinish(stream, state)
    end
  end
  return need
end

function endWritable(stream, state, cb)
  state.ending = true
  finishMaybe(stream, state)
  if cb then
    if state.finished then
      process.nextTick(cb)
    else
      stream:once('finish', cb)
    end
  end
  state.ended = true
  stream:emit('end')
end

return {
  WriteReq = WriteReq,
  WritableState = WritableState,
  Writable = Writable,
}
