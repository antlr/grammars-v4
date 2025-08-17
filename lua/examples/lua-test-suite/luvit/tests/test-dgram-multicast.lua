--[[

Copyright 2012-2015 The Luvit Authors. All Rights Reserved.

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

require('tap')(function (test)
  local dgram = require('dgram')

  local PORT = process.env.PORT or 10081
  local HOST = '0.0.0.0'

  test("tls dgram multicast", function()
    local s1 = dgram.createSocket('udp4')
    local s2 = dgram.createSocket('udp4')

    s2:on('message', function(msg, rinfo)
      assert(#msg == 5)
      assert(msg == 'HELLO')
      s2:close()
      s1:close()
    end)

    p('PORT',PORT)
    s2:bind(PORT+1,HOST)
    s1:bind(PORT,HOST)

    assert(s2:addMembership('239.255.0.1'))

    s1:send('HELLO', PORT+1, '127.0.0.1')
  end)
end)
