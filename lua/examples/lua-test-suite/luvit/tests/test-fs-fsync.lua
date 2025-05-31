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

  test('fs sync operation', function()
    local file = Path.join(module.dir, 'fixtures', 'a.lua')

    p('open ' .. file)

    FS.open(file, 'a', '0777', function(err, fd)
      print(err, fd)
      p('fd ' .. fd)
      assert(not err)

      FS.fdatasyncSync(fd)
      p('fdatasync SYNC: ok')

      FS.fsyncSync(fd)
      p('fsync SYNC: ok')

      FS.fdatasync(fd, function(err)
        assert(not err)
        p('fdatasync ASYNC: ok')
        FS.fsync(fd, function(err)
          assert(not err)
          p('fsync ASYNC: ok')
        end)
      end)
    end)

  end)
end)
