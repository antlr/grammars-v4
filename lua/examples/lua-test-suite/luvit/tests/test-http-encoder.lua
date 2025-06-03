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

local encoder = require('http-codec').encoder
local deepEqual = require('deep-equal')

local function testEncoder(encoder, inputs)
  local outputs = {}
  local encode = encoder()
  for i = 1, #inputs + 1 do
    local chunk = encode(inputs[i])
    if chunk and #chunk > 0 then
      outputs[#outputs + 1] = chunk
    end
  end
  return outputs
end

require('tap')(function (test)

  test("server encoder", function ()
    local output = testEncoder(encoder, {
      { code = 200 }
    })
    p(output)
    assert(deepEqual({
      "HTTP/1.1 200 OK\r\n\r\n"
    }, output))
  end)

  test("server encoder - Keepalive", function ()
    local output = testEncoder(encoder, {
      { code = 200,
        {"Content-Length", 12}
      },
      "Hello World\n",
      "",
      { code = 304 },
    })
    p(output)
    assert(deepEqual({
      "HTTP/1.1 200 OK\r\nContent-Length: 12\r\n\r\n",
      "Hello World\n",
      "HTTP/1.1 304 Not Modified\r\n\r\n",
    }, output))
  end)

  test("server encoder - Chunked Encoding, explicit end", function ()
    local output = testEncoder(encoder, {
      { code = 200,
        {"Transfer-Encoding", "chunked"}
      },
      "Hello World\n",
      "Another Chunk",
      "",
      { code = 304 },
    })
    p(output)
    assert(deepEqual({
      "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n",
      "c\r\nHello World\n\r\n",
      "d\r\nAnother Chunk\r\n",
      "0\r\n\r\n",
      "HTTP/1.1 304 Not Modified\r\n\r\n",
    }, output))
  end)

  test("server encoder - Chunked Encoding, auto end", function ()
    local output = testEncoder(encoder, {
      { code = 200,
        {"Transfer-Encoding", "chunked"}
      },
      "Hello World\n",
      "Another Chunk",
    })
    p(output)
    assert(deepEqual({
      "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n",
      "c\r\nHello World\n\r\n",
      "d\r\nAnother Chunk\r\n",
      "0\r\n\r\n",
    }, output))
  end)

  test("server encoder - Chunked Encoding, auto keepalive end", function ()
    local output = testEncoder(encoder, {
      { code = 200,
        {"Transfer-Encoding", "chunked"}
      },
      "Hello World\n",
      "Another Chunk",
      { code = 304 },
    })
    p(output)
    assert(deepEqual({
      "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n",
      "c\r\nHello World\n\r\n",
      "d\r\nAnother Chunk\r\n",
      "0\r\n\r\nHTTP/1.1 304 Not Modified\r\n\r\n",
    }, output))
  end)

  test("client encoder", function ()
    local output = testEncoder(encoder, {
      { method = "GET", path = "/my-resource",
        {"Accept", "*/*"}
      },
      "",
      { method = "GET", path = "/favicon.ico",
        {"Accept", "*/*"}
      },
      { method = "GET", path = "/orgs/luvit",
        {"User-Agent", "Luvit Unit Tests"},
        {"Host", "api.github.com"},
        {"Accept", "*/*"},
        {"Authorization", "token 6d2fc6ae08215d69d693f5ca76ea87c7780a4275"},
      }
    })
    p(output)
    assert(deepEqual({
      "GET /my-resource HTTP/1.1\r\nAccept: */*\r\n\r\n",
      "GET /favicon.ico HTTP/1.1\r\nAccept: */*\r\n\r\n",
      "GET /orgs/luvit HTTP/1.1\r\nUser-Agent: Luvit Unit Tests\r\nHost: api.github.com\r\nAccept: */*\r\nAuthorization: token 6d2fc6ae08215d69d693f5ca76ea87c7780a4275\r\n\r\n"
    }, output))
  end)

end)
