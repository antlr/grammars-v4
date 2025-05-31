local http = require('http')

local options = {
  host = '10.10.10.10',        -- proxy ip
  port = '80',                 -- proxy port
  path = 'www.google.com:80',  -- what to proxy to
  method = 'CONNECT',
  headers = {
    {'host', 'proxy'}          -- proxy vhost on host
  }
}

local proxy = http.request(options)
proxy:on('connect', function(response, socket, headers)
  http.get({host='www.google.com', socket=socket, connect_emitter='alreadyConnected'}, function(response)
    response:on('data', function(chunk)
      p('http data', chunk)
    end)
  end)
  socket:emit('alreadyConnected', socket)
end)
proxy:done()
