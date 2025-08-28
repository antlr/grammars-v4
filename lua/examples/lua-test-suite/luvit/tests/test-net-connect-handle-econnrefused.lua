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

local Tcp = require('uv').Tcp
local net = require('net')

local PORT = process.env.PORT or 10088

require('tap')(function(test)
  test('net-connect-handle-econnerefuesed', function(expected)
    local c, err = net.createConnection(PORT)
    c:on('connect', function ()
      print("error: connnected, please shutdown whatever is running on " .. PORT)
      assert(false)
    end)

    c:on('error', function (err)
      assert('ECONNREFUSED' == err or 'EADDRNOTAVAIL' == err)
      expected = {gotError = true}
      c:destroy()
    end)
  end)
end)