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

--[[lit-meta
  name = "luvit/https"
  version = "2.0.0"
  dependencies = {
    "luvit/tls@2.0.0",
    "luvit/http@2.0.0",
  }
  license = "Apache 2"
  homepage = "https://github.com/luvit/luvit/blob/master/deps/https.lua"
  description = "Node-style https client and server module for luvit"
  tags = {"luvit", "https", "stream"}
]]

local tls = require('tls')
local http = require('http')

local function createServer(options, onRequest)
  return tls.createServer(options, function (socket)
    return http.handleConnection(socket, onRequest)
  end)
end

local function createConnection(...)
  local args = {...}
  local options = {}
  local callback
  if type(args[1]) == 'table' then
    options = args[1]
  elseif type(args[2]) == 'table' then
    options = args[2]
    options.port = args[1]
  elseif type(args[3]) == 'table' then
    options = args[3]
    options.port = args[2]
    options.host = args[1]
  else
    if type(args[1]) == 'number' then
      options.port = args[1]
    end
    if type(args[2]) == 'string' then
      options.host = args[2]
    end
  end

  if type(args[#args]) == 'function' then
    callback = args[#args]
  end

  return tls.connect(options, callback)
end

local function request(options, callback)
  options = http.parseUrl(options)
  if options.protocol and options.protocol ~= 'https' then
    error(string.format('Protocol %s not supported', options.protocol))
  end
  options.port = options.port or 443
  options.connect_emitter = 'secureConnection'
  options.socket = options.socket or createConnection(options)
  return http.request(options, callback)
end

local function get(options, onResponse)
  options = http.parseUrl(options)
  options.method = 'GET'
  local req = request(options, onResponse)
  req:done()
  return req
end

return {
  createServer = createServer,
  request = request,
  get = get,
}
