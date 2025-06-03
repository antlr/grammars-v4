--[[

Copyright 2014-2015 The Luvit Authors. All Rights Reserved.

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

local loaded = pcall(require, 'openssl')
if not loaded then return end

local _common_tls = require('./common')
local net = require('net')

local DEFAULT_CIPHERS = _common_tls.DEFAULT_CIPHERS

local extend = function(...)
  local args = {...}
  local obj = args[1]
  for i=2, #args do
    for k,v in pairs(args[i]) do
      obj[k] = v
    end
  end
  return obj
end

local Server = net.Server:extend()
function Server:init(options, connectionListener)
  options = options or {}
  options.server = true

  options.secureContext = options.secureContext
                          or _common_tls.createCredentials(options)
  net.Server.init(self, options, function(raw_socket)
    local socket
    socket = _common_tls.TLSSocket:new(raw_socket, options)
    socket:on('secureConnection', function()
      connectionListener(socket)
    end)
    socket:on('error',function(err)
      connectionListener(socket,err)
    end)
    self.socket = socket
    if self.sni_hosts then
      socket:sni(self.sni_hosts)
    end
  end)
end

function Server:sni(hosts)
  self.sni_hosts = hosts
end

local DEFAULT_OPTIONS = {
  ciphers = DEFAULT_CIPHERS,
  rejectUnauthorized = true,
  -- TODO checkServerIdentity
}

local function connect(options, callback)
  local hostname, port, sock, colon

  callback = callback or function() end
  options = extend({}, DEFAULT_OPTIONS, options or {})
  options.server = false
  port = options.port
  hostname = options.host or options.hostname
  colon = hostname:find(':')
  if colon then
    hostname = hostname:sub(1, colon-1 )
  end

  sock = _common_tls.TLSSocket:new(nil, options)
  sock:connect(port, hostname, callback)
  return sock
end

local function createServer(options, secureCallback)
  local server = Server:new()
  server:init(options, secureCallback)
  return server
end

return {
  DEFAULT_SECUREPROTOCOL = _common_tls.DEFAULT_SECUREPROTOCOL,
  isLibreSSL = _common_tls.isLibreSSL,
  isTLSv1_3 = _common_tls.isTLSv1_3,

  TLSSocket = _common_tls.TLSSocket,
  createCredentials = _common_tls.createCredentials,
  connect = connect,
  createServer = createServer,
}
