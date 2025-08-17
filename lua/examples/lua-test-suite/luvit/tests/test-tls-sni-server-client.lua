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
]]--
require('tap')(function (test)
  local fixture = require('./fixture-tls')
  local table = require('table')
  local tls = require('tls')

  local options = {
    key = fixture.loadPEM('agent2-key'),
    cert = fixture.loadPEM('agent2-cert')
  }

  local SNIContexts = {}
  SNIContexts['a.example.com'] = {
    key = fixture.loadPEM('agent1-key'),
    cert = fixture.loadPEM('agent1-cert')
  }
  SNIContexts['asterisk.test.com'] = {
    key = fixture.loadPEM('agent3-key'),
    cert = fixture.loadPEM('agent3-cert')
  }

  local clientsOptions = {
    {
      port = fixture.commonPort,
      key = fixture.loadPEM('agent1-key'),
      cert = fixture.loadPEM('agent1-cert'),
      ca = fixture.loadPEM('ca1-cert'),
      hostname = 'a.example.com'
    },{
      port = fixture.commonPort,
      key = fixture.loadPEM('agent2-key'),
      cert = fixture.loadPEM('agent2-cert'),
      ca = fixture.loadPEM('ca2-cert'),
      hostname = 'b.test.com'
    },{
      port = fixture.commonPort,
      key = fixture.loadPEM('agent3-key'),
      cert = fixture.loadPEM('agent3-cert'),
      ca = fixture.loadPEM('ca1-cert'),
      hostname = 'c.wrong.com'
    }
  }

  local serverResults = {}
  local clientResults = {}
  test("tls sni server client", function()
    local server
    server = tls.createServer(options, function(conn)
      p(conn.ssl:get('hostname'))
      table.insert(serverResults, conn.serverName)
      conn:destroy()
    end)

    local function connectClient(options, callback)
      local client
      options.host = '127.0.0.1'
      client = tls.connect(options)
      clientResults[options.hostname] = false
      client:on('secureConnection', function()
        clientResults[options.hostname] = client.authorized
      end)
      client:on("_socketEnd", function ()
        client:destroy()
        callback()
      end)
      client:on('error', function(...)
        assert(nil, ...)
      end)
    end


    server:listen(fixture.commonPort, '127.0.0.1', function()
      server:sni({
        ['a.example.com']=SNIContexts['a.example.com'],
        ['b.test.com']=SNIContexts['asterisk.test.com']
      });

      connectClient(clientsOptions[1], function()
        connectClient(clientsOptions[2], function()
          connectClient(clientsOptions[3], function()
            p(clientResults)
            assert(clientResults['a.example.com'])
            assert(clientResults['b.test.com'])
            assert(not clientResults['c.wrong.com'])
            server:close()
          end)
        end)
      end)
    end)

  end)
end)
