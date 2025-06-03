--[[lit-meta
  name = "luvit/coro-websocket"
  version = "3.1.1"
  dependencies = {
    "luvit/http-codec@3.0.0",
    "luvit/websocket-codec@3.0.0",
    "luvit/coro-net@3.3.0",
  }
  homepage = "https://github.com/luvit/lit/blob/master/deps/coro-websocket.lua"
  description = "Websocket helpers assuming coro style I/O."
  tags = {"coro", "websocket"}
  license = "MIT"
  author = { name = "Tim Caswell" }
]]

local uv = require('uv')
local httpCodec = require('http-codec')
local websocketCodec = require('websocket-codec')
local net = require('coro-net')

local function parseUrl(url)
  local protocol, host, port, pathname = string.match(url, "^(wss?)://([^:/]+):?(%d*)(/?[^#?]*)")
  local tls
  if protocol == "ws" then
    port = tonumber(port) or 80
    tls = false
  elseif protocol == "wss" then
    port = tonumber(port) or 443
    tls = true
  else
    return nil, "Sorry, only ws:// or wss:// protocols supported"
  end
  return {
    host = host,
    port = port,
    tls = tls,
    pathname = pathname
  }
end

local function wrapIo(rawRead, rawWrite, options)

  local closeSent = false

  local timer

  local function cleanup()
    if timer then
      if not timer:is_closing() then
        timer:close()
      end
      timer = nil
    end
  end

  local function write(message)
    if message then
      message.mask = options.mask
      if message.opcode == 8 then
        closeSent = true
        rawWrite(message)
        cleanup()
        return rawWrite()
      end
    else
      if not closeSent then
        return write({
          opcode = 8,
          payload = ""
        })
      end
    end
    return rawWrite(message)
  end


  local function read()
    while true do
      local message = rawRead()
      if not message then
        return cleanup()
      end
      if message.opcode < 8 then
        return message
      end
      if not closeSent then
        if message.opcode == 8 then
          write {
            opcode = 8,
            payload = message.payload
          }
        elseif message.opcode == 9 then
          write {
            opcode = 10,
            payload = message.payload
          }
        end
        return message
      end
    end
  end

  if options.heartbeat then
    local interval = options.heartbeat
    timer = uv.new_timer()
    timer:unref()
    timer:start(interval, interval, function ()
      coroutine.wrap(function ()
        local success, err = write {
          opcode = 10,
          payload = ""
        }
        if not success then
          timer:close()
          print(err)
        end
      end)()
    end)
  end

  return read, write
end

-- options table to configure connection
--   options.path
--   options.host
--   options.port
--   options.tls
--   options.pathname
--   options.subprotocol
--   options.headers (as list of header/value pairs)
--   options.timeout
--   options.heartbeat
-- returns res, read, write (res.socket has socket)
local function connect(options)
  options = options or {}
  local config = {
    path = options.path,
    host = options.host,
    port = options.port,
    tls = options.tls,
    encoder = httpCodec.encoder,
    decoder = httpCodec.decoder,
  }
  local read, write, socket, updateDecoder, updateEncoder
    = net.connect(config, options.timeout or 10000)
  if not read then
    return nil, write
  end

  local res

  local success, err = websocketCodec.handshake({
    host = options.host,
    path = options.pathname,
    protocol = options.subprotocol
  }, function (req)
    local headers = options.headers
    if headers then
      for i = 1, #headers do
        req[#req + 1] = headers[i]
      end
    end
    write(req)
    res = read()
    if not res then error("Missing server response") end
    if res.code == 400 then
      -- p { req = req, res = res }
      local reason = read() or res.reason
      error("Invalid request: " .. reason)
    end
    return res
  end)
  if not success then
    return nil, err
  end

  -- Upgrade the protocol to websocket
  updateDecoder(websocketCodec.decode)
  updateEncoder(websocketCodec.encode)

  read, write = wrapIo(read, write, {
    mask = true,
    heartbeat = options.heartbeat
  })

  res.socket = socket
  return res, read, write

end

return {
  parseUrl = parseUrl,
  wrapIo = wrapIo,
  connect = connect,
}
