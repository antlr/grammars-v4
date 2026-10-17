local uv = require 'uv'
local channel = require 'coro-channel'

local wrapWrite = channel.wrapWrite


local writeCallbacks = {
  successful_write = function(self, chunk, callback)
    self.writeBuf = chunk
    callback()
    return true
  end,
  delayed_successful_write = function(self, chunk, callback)
    local timer = uv.new_timer()
    timer:start(50, 0, function ()
      timer:stop()
      timer:close()
      self.writeBuf = chunk
      callback()
    end)
    return true
  end,

  failed_call_write = function()
    return nil, 'call failed'
  end,
  failed_callback_write = function(_, _, callback)
    callback('callback failed')
    return true
  end,
  delayed_failed_callback_write = function(_, _, callback)
    local timer = uv.new_timer()
    timer:start(50, 0, function ()
      timer:stop()
      timer:close()
      callback('callback failed')
    end)
    return true
  end,
}

-- create a dummy socket for a write/read.
-- override methods as needed to control the behavior.
-- TODO: add read_start/read_stop
local function dummySocket()
  return {
    -- used throughout the tests to track the socket state
    is_shutdown = false,
    is_closed = false,
    writeBuf = nil,

    write = writeCallbacks.successful_write,
    is_closing = function()
      return false
    end,
    close = function(self)
      self.is_closed = true
    end,
    shutdown = function(self, callback)
      self.is_shutdown = true
      callback()
      return true
    end,
  }
end

-- Attempt a write and check the written chunk & success/failure state.
--  - if a write is expected to fail, set `expectFailure` to the expected error return.
--  - `socket.writeBuf` should be set by the write method so it can verify written data
--    and compare it to the passed `chunk` argument.
--  - if `socket.wrappedWrite` exists, it will be used instead of calling `wrapWrite` again,
--    this is useful when testing multiple write/read states.
local function expectWrite(socket, chunk, expectFailure)
  local write = socket.wrappedWrite or wrapWrite(socket)
  local success, err = write(chunk)

  if not expectFailure then
    assert(success, 'write failed, expected a successful write')
    assert(not err, 'write was successful but an error was returned') -- happens if write returned `true, "error"`
    assert(socket.writeBuf == chunk, 'write was successful but written chunk mismatches')
  else
    assert(not success, 'write succeeded, expected a failed write')
    assert(err == expectFailure, 'write returned error does not match expected error')
  end

  return success, err
end

-- TODO: add reader/closer tests
require('tap')(function (test)

  test('writer: finish writing instantly', function ()
    local ssocket = dummySocket()

    -- successful writes
    ssocket.write = writeCallbacks.successful_write
    expectWrite(ssocket, 'placeholder: write succeeds')
    expectWrite(ssocket, false)
    expectWrite(ssocket, {})

    -- failed write: the call returned failure
    ssocket.write = writeCallbacks.failed_call_write
    expectWrite(ssocket, 'placeholder: write fails', 'call failed')
    assert(ssocket.is_closed, 'expected socket to close after error')

    -- failed write: the call is successful but callback returned failure
    ssocket.write = writeCallbacks.failed_callback_write
    expectWrite(ssocket, 'placeholder: write fails', 'callback failed')
  end)

  test('writer: delayed write', function ()
    local ssocket = dummySocket()

    -- successful writes
    ssocket.write = writeCallbacks.delayed_successful_write
    expectWrite(ssocket, false)
    expectWrite(ssocket, {})

    -- failed write
    ssocket.write = writeCallbacks.delayed_failed_callback_write
    expectWrite(ssocket, 'placeholder: write fails', 'callback failed')
  end)

  test('writer: mixed instantaneous/delayed writes', function ()
    local ssocket = dummySocket()
    -- share states between writes
    -- this test tries to mix up and corrupt the states
    ssocket.wrappedWrite = wrapWrite(ssocket)

    -- successful writes
    ssocket.write = writeCallbacks.successful_write
    expectWrite(ssocket, {})
    ssocket.write = writeCallbacks.delayed_successful_write
    expectWrite(ssocket, {})
    -- failed writes
    ssocket.write = writeCallbacks.delayed_failed_callback_write
    expectWrite(ssocket, 'placeholder: write fails', 'callback failed')
    ssocket.write = writeCallbacks.failed_call_write
    expectWrite(ssocket, 'placeholder: write fails', 'call failed')
    assert(ssocket.is_closed, 'expected socket to close after error')

    -- reset states, redo tests in mixed order in case order matters
    ssocket.wrappedWrite = wrapWrite(ssocket)

    ssocket.write = writeCallbacks.delayed_failed_callback_write
    expectWrite(ssocket, 'placeholder: write fails', 'callback failed')
    ssocket.write = writeCallbacks.delayed_successful_write
    expectWrite(ssocket, {})
    ssocket.write = writeCallbacks.failed_call_write
    expectWrite(ssocket, 'placeholder: write fails', 'call failed')
    assert(ssocket.is_closed, 'expected socket to close after error')
    ssocket.write = writeCallbacks.successful_write
    expectWrite(ssocket, {})
  end)

  test('writer: an empty chunk triggers close', function ()
    local ssocket = dummySocket()

    ssocket.wrappedWrite = wrapWrite(ssocket)
    expectWrite(ssocket, nil)
    assert(ssocket.is_shutdown, 'expected socket to shutdown')
    assert(ssocket.is_closed, 'expected socket to close')
    expectWrite(ssocket, 'placeholder: write fails', 'already shutdown')

    -- do we catch socket:shutdown() failing properly?
    ssocket.wrappedWrite = wrapWrite(ssocket)
    ssocket.shutdown = function(_, callback)
      callback('failed to shutdown')
      return true
    end
    expectWrite(ssocket, nil, 'failed to shutdown')

    ssocket.wrappedWrite = wrapWrite(ssocket)
    ssocket.shutdown = function(_, callback)
      return nil, 'failed to call shutdown'
    end
    expectWrite(ssocket, nil, 'failed to call shutdown')
  end)
end)
