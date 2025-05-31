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

  local dirName = Path.join(module.dir, 'fixtures', 'test-readfile-unlink')
  local fileName = Path.join(dirName, 'test.bin')

  local bufStr = string.rep(string.char(42), 512 * 1024)
  local buf = Buffer:new(bufStr)

  test('fs readfile unlink', function()

    local ok, err

    ok, err = pcall(FS.mkdirSync, dirName, '0777')
    if not ok then
      assert(err.code == 'EEXIST')
    end

    FS.writeFileSync(fileName, buf:toString())
    FS.readFile(fileName, function(err, data)
      assert(err == nil)
      assert(#data == buf.length)
      assert(string.byte(data, 1) == 42)

      FS.unlink(fileName, function()
        FS.rmdirSync(dirName)
      end)
    end)
  end)
end)
