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
local utils = require('utils')
local Stream = require('./stream_core').Stream
local Error = core.Error

local ReadableState = core.Object:extend()

function ReadableState:initialize(options, stream)
  options = options or {}

  --[[
  // the point at which it stops calling _read() to fill the buffer
  // Note: 0 is a valid value, means "don't call _read preemptively ever"
  --]]
  local hwm = options.highWaterMark
  local defaultHwm = 16
  if not options.objectMode then
    defaultHwm = 16 * 1024
  end

  self.highWaterMark = hwm or defaultHwm

  self.buffer = {}
  self.length = 0
  self.pipes = nil
  self.pipesCount = 0
  self.flowing = nil
  self.ended = false
  self.endEmitted = false
  self.reading = false

  --[[
  // a flag to be able to tell if the onwrite cb is called immediately,
  // or on a later tick.  We set this to true at first, because any
  // actions that shouldn't happen until "later" should generally also
  // not happen before the first write call.
  --]]
  self.sync = true

  --[[
  // whenever we return null, then we set a flag to say
  // that we're awaiting a 'readable' event emission.
  --]]
  self.needReadable = false
  self.emittedReadable = false
  self.readableListening = false

  --[[
  // object stream flag. Used to make read(n) ignore n and to
  // make all the buffer merging and length checks go away
  --]]
  self.objectMode = not not options.objectMode

  if core.instanceof(stream, Stream.Duplex) then
    self.objectMode = self.objectMode or (not not options.readableObjectMode)
  end


  --[[
  // Crypto is kind of old and crusty.  Historically, its default string
  // encoding is 'binary' so we have to make this configurable.
  // Everything else in the universe uses 'utf8', though.
  --]]
  -- self.defaultEncoding = options.defaultEncoding or 'utf8';

  --[[
  // when piping, we only care about 'readable' events that happen
  // after read()ing all the bytes and not getting any pushback.
  --]]
  self.ranOut = false

  --[[
  // the number of writers that are awaiting a drain event in .pipe()s
  --]]
  self.awaitDrain = 0

  --[[
  // if true, a maybeReadMore has been scheduled
  --]]
  self.readingMore = false

  -- Here node.js stream handles encodings. But StringDecoder is not
  -- implemented in luvit
end



local Readable = Stream:extend()
local len, readableAddChunk, chunkInvalid, onEofChunk, emitReadable,
  maybeReadMore, needMoreData, roundUpToNextPowerOf2, howMuchToRead,
  endReadable, fromList, emitReadable_, flow, maybeReadMore_, resume,
  resume_, pipeOnDrain

function len(buf)
  if type(buf) == 'string' then
    return string.len(buf)
  elseif type(buf) == 'table' then
    return #buf
  else
    return -1
  end
end

function Readable:initialize(options)
  self._readableState = ReadableState:new(options, self)
  if type(Stream.initialize) == 'function' then
    Stream.initialize(self)
  end
end

--[[
// Manually shove something into the read() buffer.
// This returns true if the highWaterMark has not been hit yet,
// similar to how Writable.write() returns true if you should
// write() some more.
--]]
function Readable:push(chunk)
  local state = self._readableState

  -- encodings are not implemented

  return readableAddChunk(self, state, chunk, false)
end

--[[
// Unshift should *always* be something directly out of read()
--]]
function Readable:unshift(chunk)
  return readableAddChunk(self, self._readableState, chunk, '', true)
end

function readableAddChunk(stream, state, chunk, addToFront)
  local er = chunkInvalid(state, chunk)
  if er then
    stream:emit('error', er)
  elseif chunk == nil then
    state.reading = false
    if not state.ended then
      onEofChunk(stream, state)
    end
  elseif state.objectMode or chunk and len(chunk) > 0 then
    if state.ended and not addToFront then
      local e = Error:new('stream.push() after EOF')
      stream:emit('error', e)
    else
      if not addToFront then
        state.reading = false
      end

      --[[
      // if we want the data now, just emit it.
      --]]
      if state.flowing and state.length == 0 and not state.sync then
        stream:emit('data', chunk)
        stream:read(0)
      else
        --[[
        // update the buffer info.
        --]]
        if state.objectMode then
          state.length = state.length + 1
        else
          state.length = state.length + len(chunk)
        end
        if addToFront then
          table.insert(state.buffer, 1, chunk)
        else
          table.insert(state.buffer,chunk)
        end

        if state.needReadable then
          emitReadable(stream)
        end
      end
      maybeReadMore(stream, state)
    end
  elseif not addToFront then
    state.reading = false
  end
  return needMoreData(state)
end

--[[
// if it's past the high water mark, we can push in some more.
// Also, if we have no data yet, we can stand some
// more bytes.  This is to work around cases where hwm=0,
// such as the repl.  Also, if the push() triggered a
// readable event, and the user called read(largeNumber) such that
// needReadable was set, then we ought to push more, so that another
// 'readable' event will be triggered.
--]]
function needMoreData(state)
  return not state.ended and
  (state.needReadable or state.length < state.highWaterMark or state.length == 0)
end

--[[
// Dont't raise the hwm > 128MB
--]]
local MAX_HWM = 0x800000
function roundUpToNextPowerOf2(n)
  if n >= MAX_HWM then
    n = MAX_HWM
  else
    n = n - 1
    local p = 1
    while p < 32 do
      n = bit.bor(n, bit.rshift(n, p))
      p = bit.lshift(p, 1)
    end
    n = n + 1
  end
  return n
end

function howMuchToRead(n, state)
  if state.length == 0 and state.ended then
    return 0
  end

  if state.objectMode then
    if n == 0 then
      return 0
    else
      return 1
    end
  end

  -- n ~= n <==> isnan(n)
  if n ~= n or not n then
    if state.flowing and len(state.buffer) > 0 then
      return len(state.buffer[1])
    else
      return state.length
    end
  end

  if n <= 0 then
    return 0
  end

  --[[
  // If we're asking for more than the target buffer level,
  // then raise the water mark.  Bump up to the next highest
  // power of 2, to prevent increasing it excessively in tiny
  // amounts.
  --]]
  if n > state.highWaterMark then
    state.highWaterMark = roundUpToNextPowerOf2(n)
  end

  --[[
  // don't have that much. return null, unless we've ended.
  --]]
  if n > state.length then
    if not state.ended then
      state.needReadable = true
      return 0
    else
      return state.length
    end
  end

  return n
end

function Readable:read(n)
  local state = self._readableState
  local nOrig = n

  if type(n) ~= 'number' or n > 0 then
    state.emittedReadable = false
  end

  --[[
  // if we're doing read(0) to trigger a readable event, but we
  // already have a bunch of data in the buffer, then just trigger
  // the 'readable' event and move on.
  --]]
  if n ==0 and state.needReadable and (state.length >= state.highWaterMark or state.ended) then
    if state.length == 0 and state.ended then
      endReadable(self)
    else
      emitReadable(self)
    end
    return nil
  end

  n = howMuchToRead(n, state)

  --[[
  // if we've ended, and we'ar now clear, then finish it up
  --]]
  if n == 0 and state.ended then
    if state.length == 0 then
      endReadable(self)
    end
    return nil
  end

  --[[
  // All the actual chunk generation logic needs to be
  // *below* the call to _read.  The reason is that in certain
  // synthetic stream cases, such as passthrough streams, _read
  // may be a completely synchronous operation which may change
  // the state of the read buffer, providing enough data when
  // before there was *not* enough.
  //
  // So, the steps are:
  // 1. Figure out what the state of things will be after we do
  // a read from the buffer.
  //
  // 2. If that resulting state will trigger a _read, then call _read.
  // Note that this may be asynchronous, or synchronous.  Yes, it is
  // deeply ugly to write APIs this way, but that still doesn't mean
  // that the Readable class should behave improperly, as streams are
  // designed to be sync/async agnostic.
  // Take note if the _read call is sync or async (ie, if the read call
  // has returned yet), so that we know whether or not it's safe to emit
  // 'readable' etc.
  //
  // 3. Actually pull the requested chunks out of the buffer and return.
  --]]

  --[[
  // if we need a readable event, then we need to do some reading.
  --]]
  local doRead = state.needReadable

  --[[
  //if we currently have less than the highWaterMark, then also read some
  --]]
  if state.length == 0 or state.length - n < state.highWaterMark then
    doRead = true
  end

  --[[
  // however, if we've ended, then there's no point, and if we're already
  // reading, then it's unnecessary.
  --]]
  if state.ended or state.reading then
    doRead = false
  end

  if doRead then
    state.reading = true
    state.sync = true
    --[[
    // if the length is currently zero, then we *need* a readable event.
    --]]
    if state.length == 0 then
      state.needReadable = true
    end
    --[[
    // call internal read method
    --]]
    self:_read(state.highWaterMark)
    state.sync = false
  end
  --[[
  // If _read pushed data synchronously, then `reading` will be false,
  // and we need to re-evaluate how much data we can return to the user.
  --]]
  if doRead and not state.reading then
    n = howMuchToRead(nOrig, state)
  end

  local ret
  if n > 0 then
    ret = fromList(n, state)
  else
    ret = nil
  end

  if ret == nil then
    state.needReadable = true
    n = 0
  end

  state.length = state.length - n

  --[[
  // If we have nothing in the buffer, then we want to know
  // as soon as we *do* get something into the buffer.
  --]]
  if state.length == 0 and not state.ended then
    state.needReadable = true
  end

  --[[
  // If we tried to read() past the EOF, then emit end on the next tick.
  --]]
  if nOrig ~= n and state.ended and state.length == 0 then
    endReadable(self)
  end

  if ret ~= nil then
    self:emit('data', ret)
  end

  return ret
end

function chunkInvalid(state, chunk)
  local er
  if type(chunk) ~= 'string' and
    chunk and
    not state.objectMode then
    er = Error:new('Invalid non-string/buffer chunk')
  end
  return er
end

function onEofChunk(stream, state)
  state.ended = true

  --[[
  // emit 'readable' now to make sure it gets picked up.
  --]]
  emitReadable(stream)
end

--[[
// Don't emit readable right away in sync mode, because this can trigger
// another read() call => stack overflow.  This way, it might trigger
// a nextTick recursion warning, but that's not so bad.
--]]
function emitReadable(stream)
  local state = stream._readableState
  state.needReadable = false
  if not state.emittedReadable then
    state.emittedReadable = true
    if state.sync then
      process.nextTick(function()
        emitReadable_(stream)
      end)
    else
      emitReadable_(stream)
    end
  end
end

function emitReadable_(stream)
  stream:emit('readable')
  flow(stream)
end


--[[
// at this point, the user has presumably seen the 'readable' event,
// and called read() to consume some data.  that may have triggered
// in turn another _read(n) call, in which case reading = true if
// it's in progress.
// However, if we're not ended, or reading, and the length < hwm,
// then go ahead and try to read some more preemptively.
--]]
function maybeReadMore(stream, state)
  if not state.readingMore then
    state.readingMore = true
    process.nextTick(function()
      maybeReadMore_(stream, state)
    end)
  end
end

function maybeReadMore_(stream, state)
  local len = state.length
  while not state.reading and not state.flowing and not state.ended and
    state.length < state.highWaterMark do
    stream:read(0)
    if len == state.length then
      --[[
      // didn't get any data, stop spinning.
      --]]
      break
    else
      len = state.length
    end
  end
  state.readingMore = false
end

--[[
// abstract method.  to be overridden in specific implementation classes.
// call cb(er, data) where data is <= n in length.
// for virtual (non-string, non-buffer) streams, "length" is somewhat
// arbitrary, and perhaps not very meaningful.
--]]
function Readable:_read(n)
  self:emit('error', Error:new('not implemented'))
end

function Readable:pipe(dest, pipeOpts)
  local src = self
  local state = self._readableState
  local _endFn, ondrain

  -- local functions
  local onunpipe, onend, cleanup, ondata, onerror, onclose, onfinish, unpipe

  onunpipe = function(readable)
    if readable == src then
      cleanup()
    end
  end

  onend = function ()
    dest:_end()
  end

  cleanup = function()
    --[[
    // cleanup event handlers once the pipe is broken
    --]]
    dest:removeListener('close', onclose)
    dest:removeListener('finish', onfinish)
    dest:removeListener('drain', ondrain)
    dest:removeListener('error', onerror)
    dest:removeListener('unpipe', onunpipe)
    src:removeListener('end', onend)
    src:removeListener('end', cleanup)
    src:removeListener('data', ondata)

    --[[
    // if the reader is waiting for a drain event from this
    // specific writer, then it would cause it to never start
    // flowing again.
    // So, if this is awaiting a drain, then we just call it now.
    // If we don't know, then assume that we are waiting for one.
    --]]
    if state.awaitDrain and
      (not dest._writableState or dest._writableState.needDrain) then
      ondrain()
    end
  end

  ondata = function(chunk)
    local ret = dest:write(chunk)
    if false == ret then
      src._readableState.awaitDrain = src._readableState.awaitDrain + 1
      src:pause()
    end
  end

  --[[
  // if the dest has an error, then stop piping into it.
  // however, don't suppress the throwing behavior for this.
  --]]
  onerror = function(er)
    unpipe()
    dest:removeListener('error', onerror)
    if core.Emitter.listenerCount(dest, 'error') == 0 then
      dest:emit('error', er)
    end
  end

  --[[
  // Both close and finish should trigger unpipe, but only once.
  --]]
  onclose = function()
    dest:removeListener('finish', onfinish)
    unpipe()
  end

  onfinish = function()
    dest:removeListener('close', onclose)
    unpipe()
  end

  unpipe = function()
    src:unpipe(dest)
  end



  if state.pipesCount == 0 then
    state.pipes = dest
  elseif state.pipesCount == 1 then
    state.pipes = {state.pipes, dest}
  else
    table.insert(state.pipes, dest)
  end
  state.pipesCount = state.pipesCount + 1

  local doEnd = (not pipeOpts or pipeOpts._end ~= false) and dest ~=
  process.stdout and dest ~= process.stderr

  if doEnd then
    _endFn = onend
  else
    _endFn = cleanup
  end

  if state.endEmitted then
    process.nextTick(_endFn)
  else
    src:once('end', _endFn)
  end

  dest:on('unpipe', onunpipe)

  --[[
  // when the dest drains, it reduces the awaitDrain counter
  // on the source.  This would be more elegant with a .once()
  // handler in flow(), but adding and removing repeatedly is
  // too slow.
  --]]
  ondrain = pipeOnDrain(src)
  dest:on('drain', ondrain)

  src:on('data', ondata)

  --[[
  // This is a brutally ugly hack to make sure that our error handler
  // is attached before any userland ones.  NEVER DO THIS.
  if (!dest._events || !dest._events.error)
  dest.on('error', onerror);
  else if (Array.isArray(dest._events.error))
  dest._events.error.unshift(onerror);
  else
  dest._events.error = [onerror, dest._events.error];
  --]]

  dest:once('close', onclose)
  dest:once('finish', onfinish)

  --[[
  // tell the dest that it's being piped to
  --]]
  dest:emit('pipe', src)

  --[[
  // start the flow if it hasn't been started already.
  --]]
  if not state.flowing then
    src:resume()
  end

  return dest
end

function pipeOnDrain(src)
  return function()
    local state = src._readableState
    if state.awaitDrain ~= 0 then
      state.awaitDrain = state.awaitDrain - 1
    end
    if state.awaitDrain == 0 and core.Emitter.listenerCount(src, 'data') ~= 0 then
      state.flowing = true
      flow(src)
    end
  end
end


function Readable:unpipe(dest)
  local state = self._readableState

  --[[
  // if we're not piping anywhere, then do nothing.
  --]]
  if state.pipesCount == 0 then
    return self
  end

  --[[
  // just one destination.  most common case.
  --]]
  if state.pipesCount == 1 then
    --[[
    // passed in one, but it's not the right one.
    --]]
    if dest and dest ~= state.pipes then
      return self
    end

    if not dest then
      dest = state.pipes
    end

    --[[
    // got a match.
    --]]
    state.pipes = nil
    state.pipesCount = 0
    state.flowing = false
    if dest then
      dest:emit('unpipe', self)
    end
    return self
  end

  --[[
  // slow case. multiple pipe destinations.
  --]]

  if not dest then
    --[[
    // remove all.
    --]]
    local dests = state.pipes
    local len = state.pipesCount
    state.pipes = nil
    state.pipesCount = 0
    state.flowing = false

    for i = 1,len,1 do
      dests[i]:emit('unpipe', self)
    end
    return self
  end

  --[[
  // try to find the right one.
  --]]
  local i
  for j, pipe in ipairs(state.pipes) do
    if pipe == dest then
      i = j
    end
  end
  if i == nil then
    return self
  end

  table.remove(state.pipes, i)
  state.pipesCount = state.pipesCount - 1
  if state.pipesCount == 1 then
    state.pipes = state.pipes[1]
  end

  dest:emit('unpipe', self)

  return self
end

--[[
// set up data events if they are asked for
// Ensure readable listeners eventually get something
--]]
function Readable:on(ev, fn)
  local res = Stream.on(self, ev, fn)

  --[[
  // If listening to data, and it has not explicitly been paused,
  // then call resume to start the flow of data on the next tick.
  --]]
  if ev == 'data' and false ~= self._readableState.flowing then
    self:resume()
  end

  if ev == 'readable' and self.readable then
    local state = self._readableState
    if not state.readableListening then
      state.readableListening = true
      state.emittedReadable = false
      state.needReadable = true
      if not state.reading then
        local _self = self
        process.nextTick(function()
          _self:read(0)
        end)
      elseif state.length then
        emitReadable(self, state)
      end
    end
  end

  return res
end
Readable.addListener = Readable.on

--[[
// pause() and resume() are remnants of the legacy readable stream API
// If the user uses them, then switch into old mode.
--]]
function Readable:resume()
  local state = self._readableState
  if not state.flowing then
    state.flowing = true
    if not state.reading then
      self:read(0)
    end
    resume(self, state)
  end
  return self
end

function resume(stream, state)
  if not state.resumeScheduled then
    state.resumeScheduled = true
    process.nextTick(function()
      resume_(stream, state)
    end)
  end
end

function resume_(stream, state)
  state.resumeScheduled = false
  stream:emit('resume')
  flow(stream)
  if state.flowing and not state.reading then
    stream:read(0)
  end
end

function Readable:pause()
  if false ~= self._readableState.flowing then
    self._readableState.flowing = false
    self:emit('pause')
  end
  return self
end

function flow(stream)
  local state = stream._readableState
  if state.flowing then
    local chunk = stream:read()
    while nil ~= chunk and state.flowing do
      chunk = stream:read()
    end
  end
end

--[[
// wrap an old-style stream as the async data source.
// This is *not* part of the readable stream interface.
// It is an ugly unfortunate mess of history.
--]]
function Readable:wrap(stream)
  local state = self._readableState
  local paused = false

  stream:on('end', function()
    self:emit('end')
    self:push(nil)
  end)

  stream:on('data', function(chunk)
    if chunk == nil or not state.objectMode and len(chunk) == 0 then
      return
    end

    local ret = self:push(chunk)
    if not ret then
      paused = true
      stream:pause()
    end
  end)

  --[[
  // proxy all the other methods.
  // important when wrapping filters and duplexes.
  --]]
  for i in pairs(stream) do
    if ('function' == type(stream[i]) and self[i] == nil) then
      self[i] = stream[i]
    end
  end

  --[[
  // proxy certain important events.
  --]]
  local events = {'error', 'close', 'destroy', 'pause', 'resume'}
  for k,v in pairs(events) do
    stream:on(v, utils.bind(self.emit, self, v))
  end

  --[[
  // when we try to consume some more bytes, simply unpause the
  // underlying stream.
  --]]
  self._read = function(n)
    if paused then
      paused = false
      stream:resume()
    end
  end

  return self
end



--[[
// Pluck off n bytes from an array of buffers.
// Length is the combined lengths of all the buffers in the list.
--]]
function fromList(n, state)
  local list = state.buffer
  local length = state.length
  local objectMode = not not state.objectMode
  local ret

  --[[
  // nothing in the list, definitely empty.
  --]]
  if len(list) == 0 then
    return nil
  end

  if length == 0 then
    ret = nil
  elseif objectMode then
    ret = table.remove(list, 1)
  elseif not n or n >= length then
    ret = table.concat(list, '')
    state.buffer = {}
  else
    --[[
    // read just some of it.
    --]]
    if n < len(list[1]) then
      --[[
      // just take a part of the first list item.
      // slice is the same for buffers and strings.
      --]]
      local buf = list[1]

      ret = string.sub(buf,1,n)
      list[1] = string.sub(buf,n+1, -1)
    elseif n == len(list[1]) then
      --[[
      // first list is a perfect match
      --]]
      ret = table.remove(list, 1)
    else
      --[[
      // complex case.
      // we have enough to cover it, but it spans past the first buffer.
      --]]

      -- for better GC efficiency. (http://www.lua.org/pil/11.6.html)
      local tmp = {}
      local c = 0
      for i=1,len(list),1 do
        if n - c >= len(list[1]) then
          -- grab the entire list[1]
          c = c+ list[1]
          table.insert(tmp, table.remove(list, 1))
        else
          c = n
          table.insert(tmp, string.sub(list[1], 1, n-c))
          list[1] = string.sub(list[1], n+1, -1)
          break
        end
      end
      ret = table.concat(tmp)
    end
  end
  return ret
end

--[[
// exposed for testing purposes only.
--]]
Readable._fromList = fromList

function endReadable(stream)
  local state = stream._readableState

  --[[
  // If we get here before consuming all the bytes, then that is a
  // bug in node.  Should never happen.
  --]]
  if state.length > 0 then
    error('endReadable called on non-empty stream')
  end

  if not state.endEmitted then
    state.ended = true
    process.nextTick(function()
      --[[
      // Check that we didn't get one last unshift.
      --]]
      if not state.endEmitted and state.length == 0 then
        state.endEmitted = true
        stream.readable = false
        stream:emit('end')
      end
    end)
  end
end

return {
  Readable = Readable,
  ReadableState = ReadableState,
}
