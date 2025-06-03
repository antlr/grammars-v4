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

pcall(require, "helper")

local http = require('http')
local delay = require('timer').setTimeout

local HOST = "127.0.0.1"
local PORT = process.env.PORT or 10080
local server = nil
local client = nil

debug = function () end
local N = 10

server = http.createServer(function(request, response)
  debug('server connection handler entered')
  debug('connection flags', request.headers.connection, response.should_keep_alive)
  N = N + 1
  if N == 3 then
    request.should_keep_alive = false
    response.should_keep_alive = false
  end
debug('REQRES1', tostring(request), tostring(response), response.headers, response.headers_sent)
  local income = 'Hello world\n'
  request:on('data', function (data)
    debug('server request data', data)
    income = income .. data
  end)
  request:on('end', function ()
    debug('server request end')
    debug('server response begin')
debug('REQRES2', tostring(request), tostring(response), response.headers, response.headers_sent)
    --response:setHeader('Content-Length', #income)
    response:write(income)
    response:finish()
    --response = nil
  end)
  request:on('error', function (err)
    debug('server request error', err)
  end)
  response:on('data', function (data)
    debug('server response data', data)
  end)
  response:on('close', function ()
    debug('server response socket closed')
  end)
  response:on('end', function ()
    debug('server response end')
    --[[delay(500, function ()
      process.exit()
    end)]]--
  end)
  response:on('error', function (err)
    debug('server response error', err)
  end)
  debug('server connection handler ended')
end):listen(PORT, HOST)

--[[ TEST WITH

#!/bin/sh

cat | nc 127.0.0.1 10080 <<_EOF
GET /
Connection: keep

GET /123
Connection: keep

GET /456
Connection: close

_EOF

]]--
