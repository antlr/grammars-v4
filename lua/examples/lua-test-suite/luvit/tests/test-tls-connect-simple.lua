
require('tap')(function (test)
  local fixture = require('./fixture-tls')
  local tls = require('tls')

  local options = {
    cert = fixture.certPem,
    key = fixture.keyPem
  }

  local serverConnected = 0
  local clientConnected = 0

  local server
  local client1

  test("tls connect simple test", function()
    server = tls.createServer(options, function(conn)
      serverConnected = serverConnected + 1
      p('server accepted',serverConnected)
      if (serverConnected == 1) then
        server:close()
        p('server closed')
        conn:destroy()
      end
    end)
    server:listen(fixture.commonPort, function()
      p('server listening')
      client1 = tls.connect({port = fixture.commonPort, host = '127.0.0.1'})
      client1:on('connect', function()
        p('client connected')
        clientConnected = clientConnected + 1
      end)
      client1:on('error', function(err)
        p(err)
        client1:destroy()
      end)
      client1:on('end', function()
        p('client end')
      end)
    end)
  end)
end)
