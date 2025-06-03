local uv = require("uv")

-- Create listening socket and bind to 127.0.0.1:8080
local server = uv.new_tcp()
server:bind("127.0.0.1", 8080)

-- Setup listener
server:listen(128, function(error)
  -- This function is executed for each new client
  print("New connection")

  -- Create handles for client and upstream
  local client = uv.new_tcp()
  local upstream = uv.new_tcp()

  -- Accept the client connection
  server:accept(client)

  -- Connect to upstream server
  upstream:connect("127.0.0.1", 80, function(error)
    if error then
      print('Failed to connect to upstream: ' .. error)
      -- If can't connect to upstream, cleanup both handles
      upstream:close()
      client:close()
    else
      -- Setup handler to send data from upstream to client
      upstream:read_start(function(err, data)
        if err then print("Upstream error:" .. err) end
        if data then
          print("Upstream response: " .. data)
          client:write(data)
        else
          -- Upstream disconnected, cleanup handles
          upstream:close()
          client:close()
          print("Upstream disconnected")
        end
      end)

      -- Setup handler to send data from client to upstream
      client:read_start(function(err, data)
        if err then print("Client error:" .. err) end
        if data then
          print("Client request: " .. data)
          upstream:write(data)
        else
          -- Client disconnected, cleanup handles
          upstream:close()
          client:close()
          print("Client disconnected")
        end
      end)
    end
  end)
end)

-- Notify that the proxy is ready
print("Listening on 127.0.0.1:8080, proxying to 127.0.0.1:80")
