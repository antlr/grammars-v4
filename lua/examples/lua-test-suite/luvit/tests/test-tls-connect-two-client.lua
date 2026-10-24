
require('tap')(function (test)
  local fixture = require('./fixture-tls')
  local tls = require('tls')

  local options = {
    cert = fixture.certPem,
    key = fixture.keyPem
  }

  local serverConnected = 0
  local clientConnected = 0

  test("tls connect simple two client", function()

    local server
    server = tls.createServer(options, function(conn)
      serverConnected = serverConnected + 1
      p('accepted',serverConnected)
      conn:destroy()
      if (serverConnected == 2) then
        server:close()
        p('server closed')
      end
    end)
    server:listen(fixture.commonPort, function()
      p('listening')
      local client1, client2
      client1 = tls.connect({port = fixture.commonPort, host = '127.0.0.1'})
      client1:on('connect', function()
        clientConnected = clientConnected + 1
        --client1:destroy()
      end)
      client1:on('error',function(err) p(err) end)

      client2 = tls.connect({port = fixture.commonPort, host = '127.0.0.1'})
      client2:on('connect', function()
        clientConnected = clientConnected + 1
        --client2:destroy()
      end)
      client2:on('error',function(err) p(err) end)
    end)

  end)
end)
