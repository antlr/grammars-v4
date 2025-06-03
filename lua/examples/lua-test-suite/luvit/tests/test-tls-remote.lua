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
  local openssl = require('openssl')
  local tls = require('tls')

  local options = {
    key = fixture.loadPEM('agent1-key'),
    cert = fixture.loadPEM('agent1-cert')
  }
  p(options)
  local server
  local c
  print(openssl.error(true))
  test("tls remote-address", function()
    server = tls.createServer(options, function(s,err)
      assert(err==nil)
      s:destroy()
      server:close()
    end)
    server:on('connection', function(s, err)
      assert(err==nil)
      local addr = assert(s:address())
      p('remote ip:', addr.ip)
      p('remote port:',addr.port)
    end)

    server:listen(fixture.commonPort, '127.0.0.1', function()
      assert(server:address().ip == '127.0.0.1')
      assert(server:address().port == fixture.commonPort)

      c = tls.connect({port = fixture.commonPort, host = '127.0.0.1'})
      c:on('connect', function()
        assert(c:address().ip == '127.0.0.1')
        assert(c:address().port == fixture.commonPort)
      end)
      c:on('error', function(err)
        p(err)
        c:destroy()
      end)
      c:on('end', function()
        p('client end')
        server:close()
      end)
    end)
  end)
end)
