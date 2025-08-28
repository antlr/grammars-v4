--[[

Copyright 2012-2015 The Luvit Authors. All Rights Reserved.

Licensed under the Apache License, Version 2.0 (the "License")
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS-IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

--]]

require('tap')(function(test)

  local FS = require('fs')
  local Path = require('path')
  local Buffer = require('buffer').Buffer
  local string = require('string')
  local __filename = module.path

  local dataExpected = FS.readFileSync(__filename)

  test('fs readfile zero byte liar', function()
    -- sometimes stat returns size=0, but it's a lie.
    local _fstat,_fstatSync = FS.fstat,FS.fstatSync
    FS._fstat = FS.fstat
    FS._fstatSync = FS.fstatSync

    FS.fstat = function(fd, cb)
      FS._fstat(fd, function(er, st)
        if er then
          return cb(er)
        end
        st.size = 0
        return cb(er, st)
      end)
    end

    FS.fstatSync = function(fd)
      local st = FS._fstatSync(fd)
      st.size = 0
      return st
    end

    local d = FS.readFileSync(__filename)
    assert(d == dataExpected)

    FS.readFile(__filename, function (er, d)
      assert(d == dataExpected)
      FS.fstat,FS.fstatSync = _fstat,_fstatSync
    end)
  end)
end)
