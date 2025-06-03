local args = {...}
return function ()

  local uv = require 'uv'
  local log = require('log').log
  local makeRemote = require('codec').makeRemote
  local core = require('core')()
  local cachedDb = require('db-cached')

  local handlers = require('handlers')(core)
  -- use cached db
  local db = cachedDb(core.db)
  local handleRequest = require('api')(db, args[2])

  local app = require('weblit-app')
  require 'weblit-websocket'

  -- Listen on port 4822
  app.bind({
    host = "0.0.0.0",
    port = 4822,
  })

  -- Log requests
  app.use(function (req, res, go)
    -- Record start time
    local now = uv.now()

    -- Process the request
    go()

    -- And then log after everything is done, inserting a header for the delay
    local delay = (uv.now() - now) .. "ms"
    res.headers["X-Request-Time"] = delay
    local useragent = req.headers["user-agent"]
    if useragent then
      log("http", string.format("%s %s %s %s %s", req.method, req.path, useragent,
        res.code,
        delay
      ))
    end
  end)

  app.use(require('weblit-auto-headers'))

  .route({ method = "GET", path = "/snapshots"}, require('snapshots'))
  .route({ method = "GET", path = "/stats"}, require('stats'))

  -- Handle websocket clients
  app.websocket({
    protocol = "lit"
  }, function (req, read, write)
    -- Log the client connection
    local peerName = req.socket:getpeername()
    peerName = peerName.ip .. ':' .. peerName.port
    log("client connected", peerName)

    -- Process the client using server handles
    local remote = makeRemote(read, write)
    local success, err = xpcall(function ()
      for command, data in remote.read do
        log("client command", peerName .. " - " .. command)
        local handler = handlers[command]
        if handler then
          handler(remote, data)
        else
          remote.writeAs("error", "no such command " .. command)
        end
      end
      remote.close()
    end, debug.traceback)
    if not success then
      log("client error", err, "err")
      remote.writeAs("error", string.match(err, ":%d+: *([^\n]*)"))
      remote.close()
    end
    log("client disconnected", peerName)
  end)

  app.use(function (req, res, go)
    if req.method == "OPTIONS" then
      -- Wide open CORS headers
      return {
        code = 204,
        {"Access-Control-Allow-Origin", "*"},
        {'Access-Control-Allow-Credentials', 'true'},
        {'Access-Control-Allow-Methods', 'GET, OPTIONS'},
        -- Custom headers and headers various browsers *should* be OK with but aren't
        {'Access-Control-Allow-Headers', 'DNT,Keep-Alive,User-Agent,X-Requested-With,If-Modified-Since,Cache-Control'},
        -- Tell client that this pre-flight info is valid for 20 days
        {'Access-Control-Max-Age', 1728000},
        {'Content-Type', 'text/plain charset=UTF-8'},
        {'Content-Length', 0},
      }
    end

    go()
    -- Add CORS headers
    res.headers['Access-Control-Allow-Origin'] = '*'
    res.headers['Access-Control-Allow-Methods'] = 'GET, OPTIONS'
    res.headers['Access-Control-Allow-Headers'] = 'DNT,Keep-Alive,User-Agent,X-Requested-With,If-Modified-Since,Cache-Control'

  end)

  -- Handle HTTP clients
  .use(handleRequest)

  .start()
  -- Never return so that the command keeps running.
  coroutine.yield()
end
