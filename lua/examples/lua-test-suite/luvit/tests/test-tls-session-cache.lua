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
  local fs = require('fs')
  local childprocess = require('childprocess')
  local table = require('table')
  local tls = require('tls')
  local los = require('los')
  local key = fixture.loadPEM('agent1-key')
  local cert = fixture.loadPEM('agent1-cert')

  local options = {
    key = key,
    cert = cert,
    ca = cert,
    requestCert = true
  }

  local requestCount = 0
  test("tls connect simple twice", function()
    local server
    if los.type() == 'win32' then return end

    server = tls.createServer(options, function(cleartext)
      requestCount = requestCount + 1
      cleartext:destroy()
    end)

    function clientConnect(callback)
      local client = childprocess.spawn('openssl', {
        's_client',
        '-tls1',
        '-connect', 'localhost:' .. fixture.commonPort,
        '-key', fixture.filenamePEM('agent1-key'),
        '-cert', fixture.filenamePEM('agent1-cert'),
        '-reconnect'
      })
      client:on('exit', function()
        callback()
      end)
    end

    server:listen(fixture.commonPort, function()
      clientConnect(function()
        clientConnect(function()
          server:close()
        end)
      end)
    end)
  end)
end)
