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
    local net = require('net')

    local options = {
      key = fixture.loadPEM('agent2-key'),
      cert = fixture.loadPEM('agent2-cert')
    }
  test("tls server side ssl error when accepting", function()
    local server
    server = tls.createServer(options, function(s,err)
      assert(err=='ssl')
      s:destroy()
      server:close()
    end)

    server:listen(fixture.commonPort, function()
      local c
      c = net.createConnection({port = fixture.commonPort, host = '127.0.0.1'})
      c:on('connect', function()
        c:write('blah\nblah\nblah\n')
        c:destroy()
      end)
    end)
  end)
end)
