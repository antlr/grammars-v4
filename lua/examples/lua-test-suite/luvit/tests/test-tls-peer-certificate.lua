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
  local tls = require('tls')

  local options = {
    key = fixture.loadFile('agent.key'),
    cert = fixture.loadFile('alice.crt'),
    rejectUnauthorized=false
  }

  local verified = false

  test("tls peer certificate", function(expect)
    local server
    server = tls.createServer(options, function(cleartext,err)
      if err~='EPIPE' and err ~= 'ECANCELED' and err ~= 'ECONNRESET' then assert(not err,err) end
      cleartext:on('data', function(chunk, err)
        p(chunk, err)
        assert(chunk=='hello')
        cleartext:write('world')
        cleartext:destroy()
      end)
    end)

    server:listen(fixture.commonPort, function()
      local socket

      socket = tls.connect({host = '127.0.0.1', port = fixture.commonPort})
      socket:on('connect',function()
        socket:write('Hello')
        socket:on('data',function(chunk,err)
          assert(chunk=='world')
          p(chunk,err)
          local peercert = assert(socket:getPeerCertificate())
          peercert = peercert:parse()
          assert(peercert.subject:tostring() == '/CN=alice/subjectAltName=uniformResourceIdentifier:http://localhost:8000/alice.foaf#me')
          verified = true
          socket:destroy()
          server:close()
        end)
        socket:on('close',expect(function()
          assert(verified)
        end))
        socket:on('end',function() print('END') end)
        socket:on('error',function(err)
          assert(err.message == 'self signed certificate')
          local peercert = assert(socket:getPeerCertificate())
          peercert = peercert:parse()
          assert(peercert.subject:tostring() == '/CN=alice/subjectAltName=uniformResourceIdentifier:http://localhost:8000/alice.foaf#me')
          verified = true
          socket:destroy()
          server:close()
        end)
      end)

    end)
  end)
end)
