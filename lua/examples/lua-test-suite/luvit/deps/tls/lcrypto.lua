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
--
local _, openssl = pcall(require, 'openssl')

local function randomBytesOpenSSL(size, callback)
  local str = openssl.random(size)
  if callback then
    callback(nil, str)
  end
  return str
end

local function randomBytesInsecure(size, callback)
  print('**** WARNING: Using insecure RNG ****')
  local str = {}
  for i=0, size do
    table.insert(str, math.random())
  end
  str = table.concat(str)
  if callback then
    callback(str)
  end
  return str
end

if type(openssl) == 'table' then
  return { randomBytes = randomBytesOpenSSL }
else
  return { randomBytes = randomBytesInsecure }
end
