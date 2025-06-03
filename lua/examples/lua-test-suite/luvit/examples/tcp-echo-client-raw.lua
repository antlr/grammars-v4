-- This example just for learning the raw uv ways of making a echo client in luvit.
-- 'tcp-echo-client-simple.lua' is a much simpler version.

local uv = require('uv')

-- Create client socket
local client = uv.new_tcp()

client:connect('127.0.0.1', 1234, function(err)
  if err then error(err) end

  -- Relay data back to client
  client:read_start(function(err,data)
    -- If error, print and close connection
    if err then
      print("Client read error: " .. err)
      client:close()
    end

    -- If data is set the server has relaid data, if unset the client has disconnected
    if data then
      process.stdout.handle:write(data)
    else
      client:close()
    end

  end)

  process.stdin.handle:read_start(function(err,data)
    if err then error(err) end
    -- Send stdin to the server
    if data then
      client:write(data)
    end
  end)


end)
