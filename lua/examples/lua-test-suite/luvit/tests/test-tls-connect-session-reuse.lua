
require('tap')(function (test)
  local fixture = require('./fixture-tls')
  local tls = require('tls')
  local timer = require('timer')
  local openssl = require('openssl')
  local options = {
    cert = fixture.certPem,
    key = fixture.keyPem
  }

  local serverConnected = 0
  local clientConnected = 0

  local server
  local client1, client2
  local session0,session1,session2,session3
  local conns={}
  test("tls connect session reuse test", function()
    server = tls.createServer(options, function(conn)
      serverConnected = serverConnected + 1
      p('server accepted',serverConnected)
      conns[serverConnected] = conn
      conn:write('done\n')
      if (serverConnected == 2) then
        timer.setTimeout(1000,function()
          server:close()
          p('server closed')
          if conn:version()~='TLSv1.3' then
            -- TLSv1.3 change the session id
            assert(session0:id()==session1:id())
            assert(session1:id()==session2:id())
            assert(session2:id()==session3:id())
          else
            assert(session1:id()==session2:id())
          end
          conns[1]:destroy()
          conns[2]:destroy()
          client1:destroy()
          client2:destroy()
        end)
      end
    end)

    server:listen(fixture.commonPort, function()
      p('server listening at:',fixture.commonPort)
      local options = {
        port = fixture.commonPort,
        host = '127.0.0.1',
        rejectUnauthorized=false
      }

      options.secureContext = tls.createCredentials(options)
      local calltwo
      client1 = tls.connect(options)
      client1:on('secureConnection', function()
        p('client1 connected')
        clientConnected = clientConnected + 1
        session0 = client1.ssl:session()
        print('SESSIONID0:', openssl.hex(session0:id()))
      end)

      client1:on('error', function(err)
        p(err)
        client1:destroy()
      end)
      client1:on('data', function(...)
        p(...)
        session1 = client1.ssl:session()
        options.secureContext.session = session1
        print('SESSIONID1:', openssl.hex(session1:id()))
        assert(client1.ssl:session_reused()==false)
        timer.setTimeout(100, calltwo)
      end)
      client1:on('end', function()
        p('client end')
      end)

      calltwo = function()
        p(options)
        client2 = tls.connect(options)
        client2:on('secureConnection', function()
          p('client2 connected')
          clientConnected = clientConnected + 1
          session2 = client2.ssl:session()
          print('SESSIONID2:', openssl.hex(session2:id()))
          assert(client2.ssl:session_reused()==true)
        end)
        client2:on('error', function(err)
          p(err)
          client2:destroy()
        end)
        client2:on('end', function()
          p('client end')
        end)
        client2:on('data', function(...)
          p(...)
          session3 = client2.ssl:session()
          print('SESSIONID3:', openssl.hex(session3:id()))
        end)
      end
    end)
  end)
end)
