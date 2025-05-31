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

local net = require('net')

local PORT = process.env.PORT or 10087
local HOST = '127.0.0.1'

require('tap')(function(test)
  test('net-buffer-write-before-connect', function(expected)
    local server = net.createServer(function(client)
      p('accepted')
      client:on("data", function (chunk)
        p('server get data',chunk)
        client:write(chunk, function(err)
          assert(err == nil)
          client:destroy()
          p('server close client')
        end)
      end)
    end)

    local client
    local receivedMessage = false

    server:listen(PORT, HOST, function()
      p('server listening')
      local msg= 'hello world'
      client = net.Socket:new()
      client:connect(PORT, HOST,function()
        p('client connected')
        client:on('data', function(data)
          receivedMessage = true
          server:close()
          assert(data == msg)
        end)
        client:on('end', function()
          assert(receivedMessage == true)
          client:destroy()
          p('client end')
        end)
        client:on('error', function(err)
          assert(err)
        end)
        client:write(msg,function()
          p('client write')
        end)
      end)
    end)


    server:on("error", function(err)
      assert(err)
    end)
  end)
end)
