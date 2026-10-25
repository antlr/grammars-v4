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

local Object = require('core').Object
local Error = require('core').Error
local net = require('net')
local openssl = require('openssl')
local timer = require('timer')
local resource = require('resource')
local uv = require('uv')
local utils = require('utils')
local unpack = unpack or table.unpack ---@diagnostic disable-line: deprecated

local createCredentials
local DEFAULT_CIPHERS
local DEFAULT_SECUREPROTOCOL

local isLibreSSL = function()
  local _, _, V = openssl.version()
  return V:find('^LibreSSL')
end

local isTLSv1_3 = function()
  if isLibreSSL() then
    return false
  end

  local _, _, V = openssl.version(true)
  return V >= 0x10101000
end

DEFAULT_CIPHERS = 'TLS_AES_128_GCM_SHA256:TLS_AES_128_CCM_SHA256:' .. --TLS 1.3
                  'ECDHE-RSA-AES128-SHA256:AES128-GCM-SHA256:' ..     --TLS 1.2
                  'RC4:HIGH:!MD5:!aNULL:!EDH'                         --TLS 1.0

if isTLSv1_3() then
--[[
TLS_method(), TLS_server_method(), TLS_client_method() These are the
general-purpose version-flexible SSL/TLS methods. The actual protocol version
used will be negotiated to the highest version mutually supported by the client
and the server. The supported protocols are SSLv3, TLSv1, TLSv1.1 and TLSv1.2.
Applications should use these methods, and avoid the version- specific methods
described below.
...
TLSv1_2_method(), ...
...

Note that OpenSSL-1.1 is the version of OpenSSL; Fedora 25 and RHEL 7.3 and
other distributions (still) have OpenSSL-1.0.

TLS versions are orthogonal to the OpenSSL version.  TLS_method() is the new
in OpenSSL-1.1 version flexible function intended to replace the
TLSv1_2_method() function in OpenSSL-1.0 and the older (?), insecure
TLSv23_method(). (OpenSSL-1.0 does not have TLS_method())
--]]
  DEFAULT_SECUREPROTOCOL = 'TLS'
else
--[[
A TLS/SSL connection established with these methods may understand the SSLv3,
TLSv1, TLSv1.1 and TLSv1.2 protocols.

A client will send out TLSv1 client hello messages including extensions and
will indicate that it also understands TLSv1.1, TLSv1.2 and permits a fallback
to SSLv3. A server will support SSLv3, TLSv1, TLSv1.1 and TLSv1.2 protocols.
This is the best choice when compatibility is a concern.
--]]
  DEFAULT_SECUREPROTOCOL = 'SSLv23'
end
-------------------------------------------------------------------------------

local getSecureOptions = function(protocol, flags)
  return bit.bor(openssl.ssl.no_sslv2,
                 openssl.ssl.no_sslv3,
                 openssl.ssl.no_compression,
                 flags or 0)
end

-------------------------------------------------------------------------------

local DEFAULT_CA_STORE
do
  local data = assert(resource.load("root_ca.dat"))
  DEFAULT_CA_STORE = openssl.x509.store:new()
  local index = 1
  local len = #data
  while index < len do
    local len = bit.bor(bit.lshift(data:byte(index), 8), data:byte(index + 1))
    index = index + 2
    local cert = assert(openssl.x509.read(data:sub(index, index + len)))
    index = index + len
    assert(DEFAULT_CA_STORE:add(cert))
  end
end

-------------------------------------------------------------------------------

local Credential = Object:extend()
function Credential:initialize(secureProtocol,defaultCiphers, flags,
                               rejectUnauthorized, context, isServer)
  self.rejectUnauthorized = rejectUnauthorized
  if context then
    self.context = context
  else
    self.context = openssl.ssl.ctx_new(secureProtocol or DEFAULT_SECUREPROTOCOL,
      defaultCiphers or DEFAULT_CIPHERS)
    self.context:mode(true, 'release_buffers')
    self.context:options(getSecureOptions(secureProtocol, flags))
  end
end

function Credential:addRootCerts()
  self.context:cert_store(DEFAULT_CA_STORE)
end

function Credential:setCA(certs)
  if not self.store then
    self.store = openssl.x509.store:new()
    self.context:cert_store(self.store)
  end
  if type(certs) == 'table' then
    for _, v in pairs(certs) do
      local cert = assert(openssl.x509.read(v))
      assert(self.store:add(cert))
    end
  else
    local cert = assert(openssl.x509.read(certs))
    assert(self.store:add(cert))
  end
end

function Credential:setKeyCert(key, cert)
  key = assert(openssl.pkey.read(key, true))
  cert = assert(openssl.x509.read(cert))
  self.context:use(key, cert)
end


-------------------------------------------------------------------------------

local TLSSocket = net.Socket:extend()
function TLSSocket:initialize(socket, options)

  if socket then
    net.Socket.initialize(self, { handle = socket._handle })
  else
    net.Socket.initialize(self)
  end

  self.options = options
  self.ctx = options.secureContext
  self.server = options.server
  self.requestCert = options.requestCert
  self.rejectUnauthorized = options.rejectUnauthorized

  if self._handle == nil then
    self:once('connect', utils.bind(self._init, self))
  else
    self:_init()
  end

  self._connected = false
  self.encrypted = true
  self.readable = true
  self.writable = true

  if self.server then
    self._connecting = false
    self:once('secure', utils.bind(self._verifyServer, self))
  else
    self._connecting = true
    self:once('secure', utils.bind(self._verifyClient, self))
  end

  if socket then
    self._connecting = socket._connecting
  end

  self:once('end', function()
    self:destroy()
  end)

  self:read(0)
end

function TLSSocket:_init()
  self.ctx = self.options.secureContext or
             self.options.credentials or
             createCredentials(self.options)
  self.inp = openssl.bio.mem(8192)
  self.out = openssl.bio.mem(8192)
  self.ssl = self.ctx.context:ssl(self.inp, self.out, self.server)

  if (not self.server) then
    if self.options.hostname then
      self.ssl:set('hostname',self.options.hostname)
    end
    if self.ctx.session then
      self.ssl:session(self.ctx.session)
    end
  end
end

function TLSSocket:version()
  return self.ssl:get('version')
end

function TLSSocket:getPeerCertificate()
  return self.ssl:peer()
end

function TLSSocket:_verifyClient()
  if self.ssl:session_reused() then
    self.sessionReused = true
    self:emit('secureConnection', self)
  else
    local verifyError, verifyResults
    verifyError, verifyResults = self.ssl:getpeerverification()
    if verifyError then
      self.authorized = true
      self:emit('secureConnection', self)
    else
      self.authorized = false
      self.authorizationError = verifyResults[1].error_string
      if self.rejectUnauthorized then
        local err = Error:new(self.authorizationError)
        self:destroy(err)
      else
        self:emit('secureConnection', self)
      end
    end
  end
end

function TLSSocket:_verifyServer()
  if self.requestCert then
    local peer, verify, err
    peer = self.ssl:peer()
    if peer then
      verify, err = self.ssl:getpeerverification()
      self.authorizationError = err
      if verify then
        self.authorized = true
      elseif self.rejectUnauthorized then
        self:destroy(err)
      end
    elseif self.rejectUnauthorized then
      self:destroy(Error:new('reject unauthorized'))
    end
  end
  if not self.destroyed then
    self.ctx.session = self.ssl:session()
    self:emit('secureConnection', self)
  end
end

function TLSSocket:destroy(err)

  local hasShutdown = false
  local function reallyShutdown()
    if hasShutdown then return end
    hasShutdown = true
    net.Socket.destroy(self, err)
  end

  local function shutdown()
    timer.active(self)
    if self._shutdown then
      local _, shutdown_err = self.ssl:shutdown()
      if (shutdown_err == "want_read" or shutdown_err == "want_write"
          or shutdown_err == "syscall")
      then
        local r = self.out:pending()
        if r > 0 then
          timer.active(self._shutdownTimer)
          net.Socket._write(self, self.out:read(), function(err)
            timer.active(self._shutdownTimer)
            if err then
              self._shutdown = false
              return reallyShutdown()
            end
            shutdown()
          end)
        end
      else
        self._shutdown = false
        return reallyShutdown()
      end
    end
  end

  local function onShutdown(read_err, data)
    timer.active(self)
    if read_err or not data then
      return reallyShutdown()
    end
    timer.active(self._shutdownTimer)
    self.inp:write(data)
    shutdown()
  end

  if self.destroyed or self._shutdown then return end
  if self.ssl and self.authorized then
    if not self._shutdownTimer then
      self._shutdownTimer = timer.setTimeout(5000, reallyShutdown)
    end
    self._shutdown = true
    uv.read_stop(self._handle)
    uv.read_start(self._handle, onShutdown)
    self:emit('shutdown')
    shutdown()
  else
    reallyShutdown()
  end
end

function TLSSocket:connect(...)
  local args = {...}
  local secureCallback

  if type(args[#args]) == 'function' then
    secureCallback = args[#args]
    args[#args] = nil
  end

  self:on('secureConnection', secureCallback)
  net.Socket.connect(self, unpack(args))
end

function TLSSocket:sni(hosts)
  if self.server then
    local maps = {}
    for k,v in pairs(hosts) do
      local ctx = createCredentials(v)
      maps[k] = ctx.context
    end
    self.ctx.context:set_servername_callback(maps)
  end
end

function TLSSocket:flush(callback)
  local chunks = {}
  local i = 0
  while self.out:pending() > 0 do
    i = i + 1
    chunks[i] = self.out:read()
  end
  if i>0 then
    chunks = table.concat(chunks)
    net.Socket._write(self, chunks, callback)
  end
end

function TLSSocket:_write(data, callback)
  local ret, err
  if (not self.ssl or self.destroyed or self._shutdown or not self._connected)
  then
    return
  end
  if data then
    ret, err = self.ssl:write(data)
    if ret == nil then
      return self:destroy(err)
    end
  end
  self:flush(callback)
end

function TLSSocket:_read(n)
  local onData, handshake, incoming

  function incoming()
    repeat
      local plainText, op = self.ssl:read()
      if not plainText then
        if op == 0 then
          return net.Socket.destroy(self)
        else
          return
        end
      else
        self:push(plainText)
      end
    until not plainText
  end

  function onData(err, cipherText)
    timer.active(self)
    if err then
      return self:destroy(err)
    elseif cipherText then
      if self.inp:write(cipherText) then
        if self._connected then
          -- already finish handshake
          incoming()
        else
          -- do handshake
          handshake()
        end
      end
    else
      self.ssl = nil
      self:push(nil)
      self:emit('_socketEnd')
    end
  end

  function handshake()
    if self._connected then return end
    local ret, err = self.ssl:handshake()
    if ret == nil then
      return net.Socket.destroy(self, err)
    else
      self:flush(function(err)
        if err then return self:shutdown(err) end
        handshake()
      end)
    end

    if ret == false then return end

    self._connected = true
    if self.ssl:peek() then
      incoming()
    end
    if not uv.is_active(self._handle) then return end
    self:emit('secure')
  end

  if self._connecting then
    self:once('connect', utils.bind(self._read, self, n))
  elseif not self._reading and not self._connected then
    self._reading = true
    uv.read_start(self._handle, onData)
    handshake()
  elseif not self._reading then
    self._reading = true
    uv.read_start(self._handle, onData)
  end
end

-------------------------------------------------------------------------------
local VERIFY_PEER = openssl.ssl.peer
local VERIFY_PEER_FAIL = bit.bor(openssl.ssl.peer,openssl.ssl.fail)
local VERIFY_NONE = openssl.ssl.none

function createCredentials(options, context)
  local ctx, returnOne

  options = options or {}

  ctx = Credential:new(options.secureProtocol, options.ciphers,
    options.secureOptions, options.rejectUnauthorized, context, options.server)
  if context then
    return ctx
  end

  if options.key and options.cert then
    ctx:setKeyCert(options.key, options.cert)
  end

  if options.ca then
    ctx:setCA(options.ca)
  else
    ctx:addRootCerts()
  end

  function returnOne()
    return 1
  end

  if options.server then
    if options.requestCert then
      if options.rejectUnauthorized then
        ctx.context:verify_mode(VERIFY_PEER_FAIL, returnOne)
      else
        ctx.context:verify_mode(VERIFY_PEER, returnOne)
      end
    else
      ctx.context:verify_mode(VERIFY_NONE, returnOne)
    end
  else
    ctx.context:verify_mode(VERIFY_NONE, returnOne)
  end

  return ctx
end

return {
  DEFAULT_CIPHERS = DEFAULT_CIPHERS,
  DEFAULT_CA_STORE = DEFAULT_CA_STORE,
  DEFAULT_SECUREPROTOCOL = DEFAULT_SECUREPROTOCOL,
  isLibreSSL = isLibreSSL(),
  isTLSv1_3 = isTLSv1_3(),

  Credential = Credential,
  createCredentials = createCredentials,
  TLSSocket = TLSSocket,
}
