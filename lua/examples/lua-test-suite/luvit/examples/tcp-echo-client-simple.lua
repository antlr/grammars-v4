local net = require('net')

local client
client = net.createConnection(1234, '127.0.0.1', function (err)
  if err then error(err) end

  print("Connected...")

  -- Send stdin to the server
  process.stdin:on("data",function(data) -- or 'process.stdin:pipe(client)'
    client:write(data)
  end)

  -- Send the server's response to stdout
  client:on("data",function(data) -- or 'client:pipe(process.stdout)'
    process.stdout:write(data)
  end)
  
end)
