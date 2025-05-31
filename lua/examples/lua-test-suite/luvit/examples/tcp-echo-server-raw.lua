-- This example just for learning the raw uv ways of making a echo server in luvit.
-- 'tcp-echo-server-simple.lua' is a much simpler version.

local uv = require('uv')

-- Create listener socket
local server = uv.new_tcp()
server:bind('127.0.0.1', 1234)

server:listen(128, function(err)
  -- Create socket handle for client
  local client = uv.new_tcp()

  -- Accept incoming connection
  server:accept(client)
  print("Client connected")

  -- Relay data back to client
  client:read_start(function(err, data)
    -- If error, print and close connection
    if err then
      print("Client read error: " .. err)
      client:close()
    end

    -- If data is set the client has sent data, if unset the client has disconnected
    if data then
      client:write(data)
    else
      print("Client disconnected")
      client:close()
    end

  end)
end)
