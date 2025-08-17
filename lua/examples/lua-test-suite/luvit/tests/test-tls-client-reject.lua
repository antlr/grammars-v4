--[[

Copyright 2015 The Luvit Authors. All Rights Reserved.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS-IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

--]]

local fs = require('fs')
local tls = require('tls')
local path = require('luvi').path

if require('los').type() == 'win32' then
  return
end

require('tap')(function(test)
  test('tls client reject', function(expect)
    local options, key, cert, port, server
    local onClient

    port = 32333
    options = {}
    options.port = port
    options.key = fs.readFileSync(path.join(module.dir, 'fixtures', 'keys', 'agent1-key.pem'))
    options.cert = fs.readFileSync(path.join(module.dir, 'fixtures', 'keys', 'agent1-cert.pem'))

    function onClient(socket)
      local onData
      function onData(data)
        print(data)
        assert(data == 'ok')
      end
      p('client connected')
      socket:on('data', onData)
    end

    server = tls.createServer(options, onClient)

    local authorized = function()
      local socket, options
      local onConnect, onError

      options = {
        port = port,
        host = '127.0.0.1',
        rejectUnauthorized = true,
        ca = fs.readFileSync(path.join(module.dir, 'fixtures', 'keys', 'ca1-cert.pem'))
      }

      function onConnect()
        print("authorized() OK")
        assert(socket.authorized == true)
        socket:destroy()
        server:close()
      end

      function onError(err)
        print("authorized() error!\n")
        print(err)
        assert(false)
      end

      socket = tls.connect(options, onConnect)
      socket:on('error', onError)
      socket:write('ok')
    end

    local rejectUnauthorized = function()
      local socket, onConnect, onError

      function onConnect()
        assert(false)
      end

      function onError(err)
        assert(err.message == 'unable to get local issuer certificate')
        print('rejectUnauthorized() finished, now authorized()')
        authorized()
      end

      socket = tls.connect({
        port = port,
        host = '127.0.0.1',
        rejectUnauthorized = true
      }, onConnect)

      socket:once('error', onError)
      socket:write('ng')
    end

    local unauthorized = function()
      local socket, onConnect, onError

      function onConnect()
        assert(socket.authorized == false)
        socket:destroy()
        print('unauthorized() finished, now rejectUnauthorized()')
        rejectUnauthorized()
      end

      function onError(err)
        p(err)
        assert(false)
      end

      socket = tls.connect({rejectUnauthorized = false, port = port,
          host = '127.0.0.1'}, onConnect)
      socket:on('error', onError)
      socket:write('ok')
    end

    server:listen(port, function() unauthorized() end)
  end)
end)

