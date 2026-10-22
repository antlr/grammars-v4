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


local _, openssl = pcall(require, 'openssl')

if openssl == nil then
  return
end

local fs = require('fs')
local path = require('luvi').path
local lua_openssl_version = openssl.version(true)

local message1 = 'This message '
local message2 = 'will be signed'
local message = message1 .. message2

require('tap')(function (test)

  local ca_path = path.join(module.dir, 'ca')
  local RSA_PUBLIC_KEY = fs.readFileSync(path.join(ca_path, 'server.pub'))
  local RSA_PRIV_KEY = fs.readFileSync(path.join(ca_path, 'server.key.insecure'))
  local kpriv = openssl.pkey.read(RSA_PRIV_KEY, true)
  local kpub = openssl.pkey.read(RSA_PUBLIC_KEY)
  local sha256 = openssl.digest.get("sha256")

  if lua_openssl_version < 0x00703000 then
    assert(kpub:export({pem = true}) == RSA_PUBLIC_KEY)
  else
    assert(kpub:export('pem') == RSA_PUBLIC_KEY)
  end

  test("test digests", function()
    local hash = 'da0fd2505f0fc498649d6cf9abc7513be179b3295bb1838091723b457febe96a'
    local d = openssl.digest.new(sha256)
    d:update(message1)
    d:update(message2)
    local ret = d:final()
    assert(hash == ret)

    d:reset(d)
    d:update(message1)
    ret = d:final()
    assert(hash ~= ret)
  end)

  test("test signing", function()
    local sig = kpriv:sign(message, sha256)
    assert(openssl.pkey.verify(kpub, message1 .. message2, sig, sha256))
  end)

  test("streaming verification", function()
    local sig = kpriv:sign(message, sha256)
    assert(openssl.pkey.verify(kpub, message1 .. message2, sig, sha256))
    assert(not openssl.pkey.verify(kpub, message1 .. message2 .. 'x', sig, sha256))
  end)

  test("full buffer verify", function()
    local sig = kpriv:sign(message, sha256)
    assert(openssl.pkey.verify(kpub, message, sig, sha256))
    assert(not openssl.pkey.verify(kpub, message..'x', sig, sha256))
  end)

  test("bogus rsa", function()
    assert(not openssl.pkey.read("1"))
  end)
end)
