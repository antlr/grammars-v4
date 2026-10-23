--[[

Copyright 2012 The Luvit Authors. All Rights Reserved.

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

local FS = require('fs')
local JSON = require('json')



require('tap')(function(test)

  test('fs.stat', function()
    FS.stat('.', function(err, stats)
      assert(not err)
      p(JSON.stringify(stats))
      assert(type(stats.mtime.sec) == 'number')
    end)
  end)


  test('fs.lstat', function()
    FS.lstat('.', function(err, stats)
      assert(not err)
      p(JSON.stringify(stats))
      assert(type(stats.mtime.sec) == 'number')
    end)
  end)

  -- fstat
  test('fs.open', function()
    FS.open('.', 'r', function(err, fd)
      assert(not err)
      assert(fd)
      FS.fstat(fd, function(err, stats)
        assert(not err)
        p(JSON.stringify(stats))
        assert(type(stats.mtime.sec) == 'number')
        FS.close(fd)
      end)
    end)
  end)

  -- fstatSync
  test('fstatSync', function()
    FS.open('.', 'r', function(err, fd)
      local ok, stats
      ok, stats = pcall(FS.fstatSync, fd)
      assert(ok)
      if stats then
        p(JSON.stringify(stats))
        assert(type(stats.mtime.sec) == 'number')
      end
      FS.close(fd)
    end)
  end)

  test('stat', function()
    p('stating: ' .. module.path)
    FS.stat(module.path, function(err, s)
      assert(not err)
      p(JSON.stringify(s))
      assert(s.type == 'file')
      assert(type(s.mtime.sec) == 'number')
    end)
  end)
end)


