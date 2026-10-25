--[[lit-meta
  name = "creationix/weblit-websocket"
  version = "3.0.0"
  dependencies = {
    "luvit/websocket-codec@3.0.0",
    "luvit/coro-websocket@3.0.0",
  }
  description = "The websocket middleware for Weblit enables handling websocket clients."
  tags = {"weblit", "middleware", "websocket"}
  license = "MIT"
  author = { name = "Tim Caswell" }
  homepage = "https://github.com/creationix/weblit/blob/master/libs/weblit-websocket.lua"
]]

local websocketCodec = require('websocket-codec')
local wrapIo = require('coro-websocket').wrapIo

local function websocketHandler(options, handler)
  return function (req, res, go)
    -- Websocket connections must be GET requests
    -- with 'Upgrade: websocket'
    -- and 'Connection: Upgrade' headers
    local headers = req.headers
    local connection = headers.connection
    local upgrade = headers.upgrade
    if not (
      req.method == "GET" and
      upgrade and upgrade:lower():find("websocket", 1, true) and
      connection and connection:lower():find("upgrade", 1, true)
    ) then
      return go()
    end

    if options.filter and not options.filter(req) then
      return go()
    end

    -- If there is a sub-protocol specified, filter on it.
    local protocol = options.protocol
    if protocol then
      local list = headers["sec-websocket-protocol"]
      local foundProtocol
      if list then
        for item in list:gmatch("[^, ]+") do
          if item == protocol then
            foundProtocol = true
            break
          end
        end
      end
      if not foundProtocol then
        return go()
      end
    end

    -- Make sure it's a new client speaking v13 of the protocol
    assert(tonumber(headers["sec-websocket-version"]) >= 13, "only websocket protocol v13 supported")

    -- Get the security key
    local key = assert(headers["sec-websocket-key"], "websocket security required")

    res.code = 101
    headers = res.headers
    headers.Upgrade = "websocket"
    headers.Connection = "Upgrade"
    headers["Sec-WebSocket-Accept"] = websocketCodec.acceptKey(key)
    if protocol then
      headers["Sec-WebSocket-Protocol"] = protocol
    end
    function res.upgrade(read, write, updateDecoder, updateEncoder)
      updateDecoder(websocketCodec.decode)
      updateEncoder(websocketCodec.encode)
      read, write = wrapIo(read, write, {
        mask = false,
        heartbeat = options.heartbeat
      })
      local success, err = pcall(handler, req, read, write)
      if not success then
        print(err)
        write({
          opcode = 1,
          payload = err,
        })
        return write()
      end
    end
  end
end

local server = require('weblit-app')
function server.websocket(options, handler)
  server.route({
    method = "GET",
    path = options.path,
    host = options.host,
  }, websocketHandler(options, handler))
  return server
end

return websocketHandler
