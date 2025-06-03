--[[

Copyright 2012 The Luvit Authors. All Rights Reserved.

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

require('tap')(function (test)

  test("Chunked example with writeHead", function (expect)
    local body = "Hello world\n"

    local server = http.createServer(expect(function (req, res)
      res:writeHead(200, {["Content-Type"] = "text/plain"})
      res:finish(body)
    end))
    server:listen(1337, '127.0.0.1')
    print('Server running at http://127.0.0.1:1337/')

    http.get('http://127.0.0.1:1337', expect(function (res)
      p(res.headers)
      assert(res.headers["content-type"] == "text/plain")
      assert(res.headers["transfer-encoding"] == "chunked")
      local result = ""
      res:on("data", function (chunk)
        result = result .. chunk
      end)
      res:on("end", expect(function ()
        p(result)
        assert(result == body)
        server:close()
      end))
    end))

  end)

  test("Chunked example with setHeader", function (expect)
    local body = "Hello world\n"

    local server = http.createServer(expect(function (req, res)
      res:setHeader("Content-Type", "text/plain")
      res:finish(body)
    end))
    server:listen(1337, '127.0.0.1')
    print('Server running at http://127.0.0.1:1337/')

    http.get('http://127.0.0.1:1337', expect(function (res)
      p(res.headers)
      assert(res.headers["content-type"] == "text/plain")
      assert(res.headers["transfer-encoding"] == "chunked")
      local result = ""
      res:on("data", function (chunk)
        result = result .. chunk
      end)
      res:on("end", expect(function ()
        p(result)
        assert(result == body)
        server:close()
      end))
    end))

  end)
end)
