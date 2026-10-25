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

local timer = require('timer')
local net = require('net')
local uv = require('uv')

local function createTestServer(port, host, listenCallback)
  local server = net.createServer(function(client)
    client:pipe(client)
  end)
  server:listen(port, host, listenCallback)
  server:on("error", function(err) assert(err) end)
  return server
end

require('tap')(function (test)
  test("simple server", function(expect)
    local port = 10081
    local host = '127.0.0.1'
    local server
    server = createTestServer(port, host, expect(function()
      local client
      client = net.createConnection(port, host, expect(function()
        client:on('data', expect(function(data)
          assert(#data == 5)
          assert(data == 'hello')
          client:destroy()
          server:close()
        end))
        client:write('hello')
      end))
    end))
  end)

  test("keepalive server", function(expect)
    local port = 10082
    local host = '127.0.0.1'
    local server
    server = createTestServer(port, host, expect(function()
      local client
      client = net.createConnection(port, host, expect(function(err)
        if err then
          assert(err)
        end
        client:keepalive(true, 10)
        assert(type(client:getsockname()) == 'table')
        client:on('data', expect(function(data)
          client:keepalive(true, 10)
          assert(#data == 5)
          assert(data == 'hello')
          client:destroy()
          server:close()
        end))
        client:write('hello')
      end))
    end))
  end)

  test("nodelay server", function(expect)
    local port = 10083
    local host = '127.0.0.1'
    local server
    server = createTestServer(port, host, expect(function()
      local client
      client = net.createConnection(port, host, expect(function()
        client:nodelay(true)
        assert(type(client:getsockname()) == 'table')
        client:on('data', expect(function(data)
          assert(#data == 5)
          assert(data == 'hello')
          client:destroy()
          server:close()
        end))
        client:write('hello')
      end))
    end))
  end)

  test("timeout client", function(expect)
    local port = 10083
    local host = '127.0.0.1'
    local timeout = 1
    local onClient, onListen, server

    function onListen()
      local client, onConnect, onTimeout

      function onConnect() end

      function onTimeout()
        client:destroy()
        server:close()
      end

      client = net.createConnection(port, host, onConnect)
      client:setTimeout(timeout, expect(onTimeout))
    end

    function onClient(client)
      client:pipe(client)
    end

    server = net.createServer(onClient)
    server:listen(port, host, expect(onListen))
  end)

  test("socket buffer_size", function(expect)
    local port = 10084
    local host = '127.0.0.1'
    local timeout = 1
    local onClient, onListen, server

    function onListen()
      local client, onConnect, onTimeout

      function onConnect()
        local _1M = 1024*1024
        assert(client:getRecvBufferSize() > 0)
        assert(client:setRecvBufferSize(_1M))
        assert(client:getRecvBufferSize()==_1M)

        assert(client:getSendBufferSize() > 0)
        assert(client:setSendBufferSize(_1M))
        assert(client:getSendBufferSize()==_1M)
      end

      function onTimeout()
        client:destroy()
        server:close()
      end

      client = net.createConnection(port, host, onConnect)
      client:setTimeout(timeout, expect(onTimeout))
    end

    function onClient(client)
      client:pipe(client)
    end

    server = net.createServer(onClient)
    server:listen(port, host, expect(onListen))
  end)
end)
