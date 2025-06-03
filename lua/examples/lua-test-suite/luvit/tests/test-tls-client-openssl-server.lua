--[[

Copyright 2014 The Luvit Authors. All Rights Reserved.

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

local childprocess = require('childprocess')
local los = require('los')
local tls = require('tls')
local timer = require('timer')
local path = require('luvi').path

require('tap')(function(test)
  test('tls client econnreset', function(expect)
    local port, args, child, interval, timerCallback, key, cert
    local onInterval, onStartClient, client, data
    local count, maxCount

    if los.type() == 'win32' then return end

    count = 0
    maxCount = 5
    data = ''
    port = 32312
    key = path.join(module.dir, 'fixtures', 'keys', 'agent1-key.pem')
    cert = path.join(module.dir, 'fixtures', 'keys', 'agent1-cert.pem')
    args = { 's_server', '-accept', port, '-key', key, '-cert', cert }

    child = childprocess.spawn('openssl', args)
    child.stdout:once('data', function(data)
      p(data)
      timer.setTimeout(100, begin)
    end)
    child:on('error', function(err)
      p('server error:', err)
      child:destroy()
    end)
    child.stderr:on('data', function(data)
      print('server stderr: ' .. data)
      if data:find('error') then
        child:emit('error','internal error')
      end
    end)

    function begin()
      p('begin')
      interval = timer.setInterval(200, onInterval)
      timer.setTimeout(100, startClient)
    end

    function onInterval()
      p('onInterval')
      child.stdin:write('hello world\n')
      count = count + 1
      if count == maxCount then
        timer.clearInterval(interval)
        interval = nil
      end
    end

    function startClient()
      local onData, options, count

      p('onStartClient')

      options = {
        port = port,
        host = '127.0.0.1',
        rejectUnauthorized = false,
        secureProtocol = tls.DEFAULT_SECUREPROTOCOL
      }

      count = 0

      function onData(_data)
        p('client data ' .. _data)
        data = data .. _data
        count = count + 1
        if count == 5 then
          client:destroy()
          child:kill()
        end
      end

      client = tls.connect(options)
      client:on('data', onData)
      client:on('close', function()
        print('got close signal')
        timer.setTimeout(100, function()
          if interval then interval:close() end
          client:destroy()
          child:kill()
        end)
      end)
    end
  end)
end)

