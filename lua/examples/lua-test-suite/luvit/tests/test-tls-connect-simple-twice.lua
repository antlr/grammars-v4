require('tap')(function (test)
  local fixture = require('./fixture-tls')
  local tls = require('tls')

  local options = {
    cert = fixture.certPem,
    key = fixture.keyPem
  }

  local serverConnected = 0
  local clientConnected = 0

  test("tls connect simple twice", function()
    local server
    server = tls.createServer(options, function(conn,err)
      p(conn)
      p(err)
      serverConnected = serverConnected + 1
      if (serverConnected == 2) then
        server:close()
      end
      conn:destroy()
    end)

    server:listen(fixture.commonPort, function()
      p('listen')
      local client1, client2
      client1 = tls.connect({port = fixture.commonPort, host = '127.0.0.1'})
      client1:on('connect', function()
        p('connect 1')
        clientConnected = clientConnected + 1
        client1:on('end',function() client1:destroy() end)

        client2 = tls.connect({port = fixture.commonPort, host = '127.0.0.1'})
        client2:on('connect', function()
          p('connect 2')
          clientConnected = clientConnected + 1
          client2:on('end',function() client2:destroy() end)
        end)
      end)
    end)

  end)
end)
