require('tap')(function (test)
  local fixture = require('./fixture-tls')
  local childprocess = require('childprocess')
  local os = require('os')
  local los = require('los')
  local tls = require('tls')
  local timer = require('timer')

  local args = {
    's_server',
    '-accept', fixture.commonPort,
    '-key', 'tests/fixtures/keys/agent1-key.pem',
    '-cert', 'tests/fixtures/keys/agent1-cert.pem',
  }

  test("tls client econnreset", function()
    if los.type() == 'win32' then return end
    local child = childprocess.spawn('openssl', args)
    child:on('exit', function(exit_status)
      print('server exited')
    end)
    child:on('error', function(err)
      p('server error:', err)
      child:destroy()
    end)
    child.stdout:on('data', function(data)
      print('server stdout: ' .. data)
    end)
    child.stderr:on('data', function(data)
      print('server stderr: ' .. data)
      if data:find('error') then
        child:emit('error','internal error')
      end
    end)

    local interval = timer.setInterval(100, function()
      local success, err = pcall(child.stdin.write, child.stdin, "Hello world")
    end)

    timer.setTimeout(200,function ()
      local c
      c = tls.connect({
        port = fixture.commonPort,
        host = '127.0.0.1',
        secureProtocol = tls.DEFAULT_SECUREPROTOCOL
      })
      c:on('error', function(err)
        print("got connection error")
        p(err)
      end)
      c:on('close', function()
        print('got close signal')
        timer.setTimeout(100, function()
          interval:close()
        end)
      end)
      c:on('data', function(data)
        print('client got: ' .. data)
      end)
      c:on('end', function()
        c:destroy()
      end)
    end)

    timer.setTimeout(1000, function()
      child:kill()
    end)
  end)
end)
