--[[

Copyright 2016 The Luvit Authors. All Rights Reserved.

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
local openssl = require('openssl')

local loadResource
if type(module) == "table" then
  function loadResource(path)
    return module:load(path)
  end
else
  loadResource = require('resource').load
end
local bit = require('bit')

local DEFAULT_SECUREPROTOCOL
do
  local _, _, V = openssl.version()
  local isLibreSSL = V:find('^LibreSSL')

  _, _, V = openssl.version(true)
  local isTLSv1_3 = not isLibreSSL and V >= 0x10101000

  if isTLSv1_3 then
    DEFAULT_SECUREPROTOCOL = 'TLS'
  else
    DEFAULT_SECUREPROTOCOL = 'SSLv23'
  end
end
local DEFAULT_CIPHERS = 'TLS_AES_128_GCM_SHA256:TLS_AES_128_CCM_SHA256:' .. --TLS 1.3
                        'ECDHE-RSA-AES128-SHA256:AES128-GCM-SHA256:' ..     --TLS 1.2
                        'RC4:HIGH:!MD5:!aNULL:!EDH'                         --TLS 1.0
local DEFAULT_CA_STORE
do
  local data = assert(loadResource("./root_ca.dat"))
  DEFAULT_CA_STORE = openssl.x509.store:new()
  local index = 1
  local dataLength = #data
  while index < dataLength do
    local len = bit.bor(bit.lshift(data:byte(index), 8), data:byte(index + 1))
    index = index + 2
    local cert = assert(openssl.x509.read(data:sub(index, index + len)))
    index = index + len
    assert(DEFAULT_CA_STORE:add(cert))
  end
end

local function returnOne()
  return 1
end

return function (options)
  local ctx = openssl.ssl.ctx_new(
    options.protocol or DEFAULT_SECUREPROTOCOL,
    options.ciphers or DEFAULT_CIPHERS)

  local key, cert, ca
  if options.key then
    key = assert(openssl.pkey.read(options.key, true, 'pem'))
  end
  if options.cert then
    cert = {}
    for chunk in options.cert:gmatch("%-+BEGIN[^-]+%-+[^-]+%-+END[^-]+%-+") do
      cert[#cert + 1] = assert(openssl.x509.read(chunk))
    end
  end
  if options.ca then
    if type(options.ca) == "string" then
      ca = { assert(openssl.x509.read(options.ca)) }
    elseif type(options.ca) == "table" then
      ca = {}
      for i = 1, #options.ca do
        ca[i] = assert(openssl.x509.read(options.ca[i]))
      end
    else
      error("options.ca must be string or table of strings")
    end
  end
  if key and cert then
    local first = table.remove(cert, 1)
    assert(ctx:use(key, first))
    if #cert > 0 then
      -- TODO: find out if there is a way to not need to duplicate the last cert here
      -- as a dummy fill for the root CA cert
      assert(ctx:add(cert[#cert], cert))
    end
  end
  if ca then
    local store = openssl.x509.store:new()
    for i = 1, #ca do
      assert(store:add(ca[i]))
    end
    ctx:cert_store(store)
  elseif DEFAULT_CA_STORE then
    ctx:cert_store(DEFAULT_CA_STORE)
  end
  if not (options.insecure or options.key) then
    ctx:verify_mode(openssl.ssl.peer, returnOne)
  end

  ctx:options(bit.bor(
    openssl.ssl.no_sslv2,
    openssl.ssl.no_sslv3,
    openssl.ssl.no_compression))

  return ctx
end
