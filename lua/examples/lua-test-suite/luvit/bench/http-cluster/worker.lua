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

local uv = require('uv')
local encoder = require('http-codec').encoder
local decoder = require('http-codec').decoder

local app = require('./app')

local server = uv.new_tcp()

local function httpDecoder(emit)
  local decode = decoder()
  local input
  return function (err, chunk)
    if err then return emit(err) end
    input = (input and chunk) and (input .. chunk) or chunk
    repeat
      local event, extra = decode(input)
      if event then
        input = extra
        emit(nil, event)
      end
    until not event
  end
end

local function process(socket, app)
  local req, res, body
  local encode = encoder()
  socket:read_start(httpDecoder(function (err, event)
    if err then return end
    assert(not err, err)
    local typ = type(event)
    if typ == "table" then
      req = event
      res, body = app(req)
      socket:write(encode(res) .. encode(body))
      if not req.keepAlive then
        socket:close()
      end
    elseif typ == "string" and req.onbody then
      req.onbody(event)
    elseif not event then
      socket:close()
    end
  end))
end

local function onconnection(err)
  if err then return end
  local client = uv.new_tcp()
  server:accept(client)
  process(client, app)
end

-- Get listening socket from master process
local pipe = uv.new_pipe(true)
uv.pipe_open(pipe, 3)
uv.read_start(pipe, function (err)
  assert(not err, err)
  if uv.pipe_pending_count(pipe) > 0 then
    local pending_type = uv.pipe_pending_type(pipe)
    assert(pending_type == "tcp")
    assert(uv.accept(pipe, server))
    assert(uv.listen(server, 256, onconnection))
    uv.close(pipe)
    print("Worker received server handle")
  end
end)
