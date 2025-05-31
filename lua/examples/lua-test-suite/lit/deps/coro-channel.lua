--[[lit-meta
  name = "luvit/coro-channel"
  version = "3.0.5"
  homepage = "https://github.com/luvit/lit/blob/master/deps/coro-channel.lua"
  description = "An adapter for wrapping uv streams as coro-streams."
  tags = {"coro", "adapter"}
  license = "MIT"
  author = { name = "Tim Caswell" }
]]

local unpack = unpack or table.unpack

local function assertResume(thread, ...)
  local success, err = coroutine.resume(thread, ...)
  if not success then
    error(debug.traceback(thread, err), 0)
  end
end

local function makeCloser(socket)
  local closer = {
    read = false,
    written = false,
    errored = false,
  }

  local closed = false

  local function close()
    if closed then return end
    closed = true
    if not closer.readClosed then
      closer.readClosed = true
      if closer.onClose then
        closer.onClose()
      end
    end
    if not socket:is_closing() then
      socket:close()
    end
  end

  closer.close = close

  function closer.check()
    if closer.errored or (closer.read and closer.written) then
      return close()
    end
  end

  return closer
end

local function makeRead(socket, closer)
  local paused = true

  local queue = {}
  local tindex = 0
  local dindex = 0

  local function dispatch(data)

    -- p("<-", data[1])

    if tindex > dindex then
      local thread = queue[dindex]
      queue[dindex] = nil
      dindex = dindex + 1
      assertResume(thread, unpack(data))
    else
      queue[dindex] = data
      dindex = dindex + 1
      if not paused then
        paused = true
        assert(socket:read_stop())
      end
    end
  end

  closer.onClose = function ()
    if not closer.read then
      closer.read = true
      return dispatch {nil, closer.errored}
    end
  end

  local function onRead(err, chunk)
    if err then
      closer.errored = err
      return closer.check()
    end
    if not chunk then
      if closer.read then return end
      closer.read = true
      dispatch {}
      return closer.check()
    end
    return dispatch {chunk}
  end

  local function read()
    if dindex > tindex then
      local data = queue[tindex]
      queue[tindex] = nil
      tindex = tindex + 1
      return unpack(data)
    end
    if paused then
      paused = false
      assert(socket:read_start(onRead))
    end
    queue[tindex] = coroutine.running()
    tindex = tindex + 1
    return coroutine.yield()
  end

  -- Auto use wrapper library for backwards compat
  return read
end

local function makeWrite(socket, closer)

  local hasYielded, hasReturned
  local success, err, cbErr
  local function wait()
    local thread = coroutine.running()
    hasYielded, hasReturned = nil, nil
    return function (err)
      if hasYielded then
        hasYielded = false
        assertResume(thread, err)
      else
        cbErr = err
        hasReturned = true
      end
    end
  end

  local function write(chunk)
    if closer.written then
      return nil, "already shutdown"
    end

    -- p("->", chunk)

    if chunk == nil then
      closer.written = true
      closer.check()
      success, err = socket:shutdown(wait())
      if not success then
        return nil, err
      end
      if not hasReturned then
        hasYielded = true
        cbErr = coroutine.yield()
      end
      return not cbErr, cbErr
    end

    success, err = socket:write(chunk, wait())
    if not success then
      closer.errored = err
      closer.check()
      return nil, err
    end
    if not hasReturned then
      hasYielded = true
      cbErr = coroutine.yield()
    end
    return not cbErr, cbErr
  end

  return write
end

local function wrapRead(socket)
  local closer = makeCloser(socket)
  closer.written = true
  return makeRead(socket, closer), closer.close
end

local function wrapWrite(socket)
  local closer = makeCloser(socket)
  closer.read = true
  return makeWrite(socket, closer), closer.close
end

local function wrapStream(socket)
  assert(socket
    and socket.write
    and socket.shutdown
    and socket.read_start
    and socket.read_stop
    and socket.is_closing
    and socket.close, "socket does not appear to be a socket/uv_stream_t")

  local closer = makeCloser(socket)
  return makeRead(socket, closer), makeWrite(socket, closer), closer.close
end

return {
  wrapRead = wrapRead,
  wrapWrite = wrapWrite,
  wrapStream = wrapStream,
}
