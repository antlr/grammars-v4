--[[

Copyright 2012-2015 The Luvit Authors. All Rights Reserved.

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

require('tap')(function (test)

  local net = require('net')
  local http = require('http')
  local PORT = process.env.PORT or 10084
  local HOST = '127.0.0.1'

  local running = false

  local caughtErrors = 0
  local gotParseError = false

  test("tls http parse error", function()
    local server = net.createServer(function(client)
      client:write('test\n\n',function(...)
        client:setTimeout(100,function()
          client:destroy()
        end)
      end)
      client:on("end", function()
        client:destroy()
        server:close()
      end)
      client:on('error',function(err)
        print('ERR',err)
      end)
    end)

    server:listen(PORT, HOST, function()
      running = true

      local req = http.request({
        host = HOST,
        port = PORT,
        path = '/'
      }, function (res)
        p(res)
      end)

      req:on("error", function(err)
        msg = tostring(err)

        caughtErrors = caughtErrors + 1

        if msg:find('expected HTTP data') then
          gotParseError = true
          running = false
          req:destroy()
          server:close()
        end
      end)

      req:done()
    end)

  end)
end)
