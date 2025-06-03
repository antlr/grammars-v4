--[[lit-meta
  name = "luvit/ssh-rsa"
  version = "2.0.0"
  homepage = "https://github.com/luvit/lit/blob/master/deps/ssh-rsa.lua"
  description = "Addons to lua-openssl for working with openssh rsa keys."
  tags = {"ssh", "rsa"}
  license = "MIT"
  author = { name = "Tim Caswell" }
]]

local openssl = require('openssl')
local pkey = openssl.pkey
local bn = openssl.bn
local digest = openssl.digest.digest

local enc, dec
-- base64 code from http://lua-users.org/wiki/BaseSixtyFour
do
  -- character table string
  local chars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/'

  -- encoding
  function enc(data)
      return ((data:gsub('.', function(x)
          local r, b='',x:byte()
          for i=8,1,-1 do r=r..(b%2^i-b%2^(i-1)>0 and '1' or '0') end
          return r;
      end)..'0000'):gsub('%d%d%d?%d?%d?%d?', function(x)
          if (#x < 6) then return '' end
          local c=0
          for i=1,6 do c=c+(x:sub(i,i)=='1' and 2^(6-i) or 0) end
          return chars:sub(c+1,c+1)
      end)..({ '', '==', '=' })[#data%3+1])
  end

  -- decoding
  function dec(data)
      data = string.gsub(data, '[^' .. chars .. '=]', '')
      return (data:gsub('.', function(x)
          if (x == '=') then return '' end
          local r, f = '', (chars:find(x) - 1)
          for i=6,1,-1 do r=r..(f%2^i-f%2^(i-1)>0 and '1' or '0') end
          return r;
      end):gsub('%d%d%d?%d?%d?%d?%d?%d?', function(x)
          if (#x ~= 8) then return '' end
          local c=0
          for i=1,8 do c=c+(x:sub(i,i)=='1' and 2^(8-i) or 0) end
          return string.char(c)
      end))
  end
end

local function encodePrefix(body)
  if string.byte(body, 1) >= 128 then
    body = '\0' .. body
  end
  local len = #body
  return string.char(bit.band(bit.rshift(len, 24), 0xff))
      .. string.char(bit.band(bit.rshift(len, 16), 0xff))
      .. string.char(bit.band(bit.rshift(len, 8), 0xff))
      .. string.char(bit.band(len, 0xff))
      .. body
end

local function decodePrefix(input)
  local len = bit.bor(
    bit.lshift(string.byte(input, 1), 24),
    bit.lshift(string.byte(input, 2), 16),
    bit.lshift(string.byte(input, 3), 8),
               string.byte(input, 4))
  return string.sub(input, 5, 4 + len), string.sub(input, 5 + len)
end


-- Given two openssl.bn instances for e and n, return the ssh-rsa formatted string for public keys.
local function encode(e, n)
  return encodePrefix("ssh-rsa")
    .. encodePrefix(e:totext())
    .. encodePrefix(n:totext())
end

-- Given a raw ssh-rsa key as a binary string, parse out e and n as openssl.bn instances
local function decode(input)
  local format, e, n
  format, input = decodePrefix(input)
  assert(format == "ssh-rsa")
  e, input = decodePrefix(input)
  n, input = decodePrefix(input)
  assert(input == "")
  return bn.text(e), bn.text(n)
end

-- Calculate an ssh style fingerprint from raw public data
local function fingerprint(data)
  local parts = {}
  local hash = digest("md5", data, true)
  for i = 1, #hash do
    parts[i] = string.format("%02x", string.byte(hash, i))
  end
  return table.concat(parts, ":")
end

-- Calculate the public key data from an rsa private key file
local function loadPrivate(data)
  local key = pkey.read(data, true)
  local rsa = key:parse().rsa:parse()
  return encode(rsa.e, rsa.n)
end

-- Extract the raw data from a public key file.
local function loadPublic(data)
  data = data:match("^ssh%-rsa ([^ ]+)")
  data = data and data:gsub("%s", "")
  return data and dec(data)
end

local function writePublic(data)
  return "ssh-rsa " .. enc(data)
end

local function sign(body, privateKey)

  -- Extract e and n from the private RSA key to build the ssh public key
  local rsa = privateKey:parse().rsa:parse()
  -- Encode in ssh-rsa format
  local data = encode(rsa.e, rsa.n)
  -- And digest in ssh fingerprint format
  local fingerprint = fingerprint(data)

  -- Sign the message using a sha256 message digest
  local sig = privateKey:sign(body, "sha256")
  return body ..
    "-----BEGIN RSA SIGNATURE-----\n" ..
    "Format: sha256-ssh-rsa\n" ..
    "Fingerprint: " .. fingerprint .. "\n\n" ..
    openssl.base64(sig) ..
    "-----END RSA SIGNATURE-----\n"
end

local function toPublicKey(data)

  -- Convert to openssl format
  local e, n = decode(data)
  local key = pkey.new({
    alg = 'rsa',
    n = n,
    e = e
  })

  -- Make sure the encoding/decoding roudtrip worked
  local rsa = key:parse().rsa:parse()
  assert(rsa.e == e and rsa.n == n)

  return key
end

-- Given a raw body, a raw signature (PEM encoded with metadata), and a
-- publicKey instance, verify a signature.
local function verify(body, signature, data)
  local key = toPublicKey(data)
  return key:verify(body, dec(signature), "sha256")
end

return {
  encode = encode,
  decode = decode,
  fingerprint = fingerprint,
  loadPrivate = loadPrivate,
  loadPublic = loadPublic,
  writePublic = writePublic,
  sign = sign,
  toPublicKey = toPublicKey,
  verify = verify,
}
