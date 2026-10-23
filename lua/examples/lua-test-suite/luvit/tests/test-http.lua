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

local decoder = require('http-codec').decoder
local encoder = require('http-codec').encoder
local uv = require('uv')

require('tap')(function (test)

  test("Real HTTP request", function (expect)
    uv.getaddrinfo("github.com", "http", {
      socktype = "stream",
      family = "inet",
    }, expect(function (err, res)
      assert(not err, err)
      local client = uv.new_tcp()
      client:connect(res[1].addr, res[1].port, expect(function (err)
        assert(not err, err)
        p {
          client = client,
          sock = client:getsockname(),
          peer = client:getpeername(),
        }
        local encode, decode = encoder(), decoder()
        local req = {
            method = "GET", path = "/",
            {"Host", "github.com"},
            {"User-Agent", "luvit"},
            {"Accept", "*/*"},
        }
        p(req)
        client:write(encode(req))
        local parts = {}
        local data = ""
        local finish
        client:read_start(expect(function (err, chunk)
          assert(not err, err)
          if not chunk then
            return finish()
          end
          data = data .. chunk
          repeat
            local event, extra = decode(data)
            if event then
              parts[#parts + 1] = event
              if event == "" then return finish() end
              data = extra
            end
          until not event
        end))

        finish = expect(function ()
          client:read_stop()
          client:close()
          local res = table.remove(parts, 1)
          p(res)
          -- luvit.io should redirect to https version
          assert(res.code == 301)

          local contentLength
          for i = 1, #res do
            if string.lower(res[i][1]) == "content-length" then
              contentLength = tonumber(res[i][2])
              break
            end
          end
          for i = 1, #parts do
            local item = parts[i]
            contentLength = contentLength - #item
            p(item)
          end
          assert(contentLength == 0)
        end)
      end))
    end))
  end)
end)
