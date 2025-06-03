local net = require('net')

local server = net.createServer(function(client)
  print("Client connected")

  -- Add some listenners for incoming connection
  client:on("error",function(err)
    print("Client read error: " .. err)
    client:close()
  end)

  client:on("data",function(data)
    client:write(data)
  end)

  client:on("end",function()
    print("Client disconnected")
  end)
end)

-- Add error listenner for server
server:on('error',function(err)
  if err then error(err) end
end)

server:listen(1234, '127.0.0.1') -- or "server:listen(1234)"
