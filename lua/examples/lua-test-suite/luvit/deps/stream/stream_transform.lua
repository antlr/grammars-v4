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

--[[
// a transform stream is a readable/writable stream where you do
// something with the data.  Sometimes it's called a "filter",
// but that's not a great name for it, since that implies a thing where
// some bits pass through, and others are simply ignored.  (That would
// be a valid example of a transform, of course.)
//
// While the output is causally related to the input, it's not a
// necessarily symmetric or synchronous transformation.  For example,
// a zlib stream might take multiple plain-text writes(), and then
// emit a single compressed chunk some time in the future.
//
// Here's how this works:
//
// The Transform stream has all the aspects of the readable and writable
// stream classes.  When you write(chunk), that calls _write(chunk,cb)
// internally, and returns false if there's a lot of pending writes
// buffered up.  When you call read(), that calls _read(n) until
// there's enough pending readable data buffered up.
//
// In a transform stream, the written data is placed in a buffer.  When
// _read(n) is called, it transforms the queued up data, calling the
// buffered _write cb's as it consumes chunks.  If consuming a single
// written chunk would result in multiple output chunks, then the first
// outputted bit calls the readcb, and subsequent chunks just go into
// the read buffer, and will cause it to emit 'readable' if necessary.
//
// This way, back-pressure is actually determined by the reading side,
// since _read has to be called to start processing a new chunk.  However,
// a pathological inflate type of transform can cause excessive buffering
// here.  For example, imagine a stream where every byte of input is
// interpreted as an integer from 0-255, and then results in that many
// bytes of output.  Writing the 4 bytes {ff,ff,ff,ff} would result in
// 1kb of data being output.  In this case, you could write a very small
// amount of input, and end up with a very large amount of output.  In
// such a pathological inflating mechanism, there'd be no way to tell
// the system to stop doing the transform.  A single 4MB write could
// cause the system to run out of memory.
//
// However, even in such a pathological case, only a single written chunk
// would be consumed, and then the rest would wait (un-transformed) until
// the results of the previous transformed chunk were consumed.
--]]

local Duplex = require('./stream_duplex').Duplex
local core = require('core')
local Error = core.Error

local Transform = Duplex:extend()


local TransformState = core.Object:extend()

local afterTransform, done

function TransformState:initialize(options, stream)
  self.afterTransform = function(er, data)
    return afterTransform(stream, er, data)
  end

  self.needTransform = false
  self.transforming = false
  self.writecb = nil
  self.writechunk = nil
end

function afterTransform(stream, er, data)
  local ts = stream._transformState
  ts.transforming = false

  local cb = ts.writecb

  if not cb then
    return stream:emit('error', Error:new('no writecb in Transform class'))
  end

  ts.writechunk = nil
  ts.writecb = nil

  if data then
    stream:push(data)
  end

  if cb then
    cb(er)
  end

  local rs = stream._readableState
  rs.reading = false
  if rs.needReadable or rs.length < rs.highWaterMark then
    stream:_read(rs.highWaterMark)
  end
end


function Transform:initialize(options)
  --[[
  if (!(this instanceof Transform))
    return new Transform(options)
  --]]

  Duplex.initialize(self, options)

  self._transformState = TransformState:new(options, self)

  --[[
  // when the writable side finishes, then flush out anything remaining.
  --]]
  local stream = self

  --[[
  // start out asking for a readable event once data is transformed.
  --]]
  self._readableState.needReadable = true

  --[[
  // we have implemented the _read method, and done the other things
  // that Readable wants before the first _read call, so unset the
  // sync guard flag.
  --]]
  self._readableState.sync = false

  self:once('prefinish', function()
    if type(self._flush) == 'function' then
      self._flush(function(er)
        done(stream, er)
      end)
    else
      done(stream)
    end
  end)
end

function Transform:push(chunk)
  self._transformState.needTransform = false
  return Duplex.push(self, chunk)
end

--[[
// This is the part where you do stuff!
// override this function in implementation classes.
// 'chunk' is an input chunk.
//
// Call `push(newChunk)` to pass along transformed output
// to the readable side.  You may call 'push' zero or more times.
//
// Call `cb(err)` when you are done with this chunk.  If you pass
// an error, then that'll put the hurt on the whole operation.  If you
// never call cb(), then you'll never get another chunk.
--]]
function Transform:_transform(chunk, cb)
  error('not implemented')
end

function Transform:_write(chunk, cb)
  local ts = self._transformState
  ts.writecb = cb
  ts.writechunk = chunk
  if not ts.transforming then
    local rs = self._readableState
    if ts.needTransform or
        rs.needReadable or
        rs.length < rs.highWaterMark then
      self:_read(rs.highWaterMark)
    end
  end
end

--[[
// Doesn't matter what the args are here.
// _transform does all the work.
// That we got here means that the readable side wants more data.
--]]
function Transform:_read(n)
  local ts = self._transformState

  if ts.writechunk ~= nil and ts.writecb and not ts.transforming then
    ts.transforming = true
    self:_transform(ts.writechunk, ts.afterTransform)
  else
    --[[
    // mark that we need a transform, so that any data that comes in
    // will get processed, now that we've asked for it.
    --]]
    ts.needTransform = true
  end
end


function done(stream, er)
  if er then
    return stream:emit('error', er)
  end

  --[[
  // if there's nothing in the write buffer, then that means
  // that nothing more will ever be provided
  --]]
  local ws = stream._writableState
  local ts = stream._transformState

  if ws.length ~= 0 then
    error('calling transform done when ws.length != 0')
  end

  if ts.transforming then
    error('calling transform done when still transforming')
  end

  return stream:push(nil)
end

return { Transform = Transform }
