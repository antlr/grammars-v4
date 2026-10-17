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

local dgram = require('dgram')

require('tap')(function(test)
  test('test udp', function(expect)
    local PORT, HOST, s1, s2
    local onMessageS1, onMessageS2, onError

    HOST = '127.0.0.1'
    PORT = 53211

    s1 = dgram.createSocket()
    s2 = dgram.createSocket()

    function onMessageS1(msg, rinfo)
      assert(#msg == 4)
      assert(msg == 'PING')
      s1:send('PONG', PORT+1, HOST)
    end

    function onMessageS2(msg, rinfo)
      assert(#msg == 4)
      assert(msg == 'PONG')
      s1:close()
      s2:close()
    end

    function onError(err)
      assert(err)
    end

    s1:on('message', expect(onMessageS1))
    s1:on('error', onError)
    s1:bind(PORT,'127.0.0.1')

    s2:on('message', expect(onMessageS2))
    s2:on('error', onError)
    s2:bind(PORT+1, '127.0.0.1')
    s2:send('PING', PORT, HOST)
  end)
end)
