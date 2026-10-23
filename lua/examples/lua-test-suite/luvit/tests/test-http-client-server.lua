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

local http = require('http')

local HOST = "127.0.0.1"
local PORT = process.env.PORT or 10082

local body = "Hello world\n"

require('tap')(function(test)
  test("http-client-server", function(expect)
    local server = nil
    local client = nil

    server = http.createServer(expect(function(request, response)
      p('server:onConnection req', request)
      assert(request.method == "GET")
      assert(request.url == "/foo")
      -- Fixed because header parsing is not busted anymore
      assert(request.headers.bar == "cats")
      p('server:onConnection bare resp', response)
      response:setHeader("Content-Type", "text/plain")
      response:setHeader("Content-Length", #body)
      response:finish(body)
    end))

    server:listen(PORT, HOST, function()
      local req = http.request({
        host = HOST,
        port = PORT,
        path = "/foo",
        headers = {{"bar", "cats"}}
        }, expect(function(response)
          p('client:onResponse', response)
          assert(response.statusCode == 200)
          assert(response.httpVersion == '1.1')
          server:close()
      end))
      req:on('error',function(...)print(...)end)
      req:done()
    end)
  end)
end)
