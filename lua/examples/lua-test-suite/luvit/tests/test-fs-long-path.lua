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
  local math = require('math')
  local string = require('string')
  local FS = require('fs')
  local Path = require('path')
  local JSON = require('json')

  local successes = 0

  -- make a path that will be at least 260 chars long.
  local tmpDir = Path.join(module.dir, 'tmp')
  local fileNameLen = math.max(260 - #tmpDir - 1, 1)
  local fileName = Path.join(tmpDir, string.rep('x', fileNameLen))

  test('fs longpoath', function()
    p('fileName=' .. fileName)
    p('fileNameLength=' .. #fileName)

    FS.writeFile(fileName, 'ok', function(err)
      if err then
        return err
      end
      successes = successes + 1

      FS.stat(fileName, function(err, stats)
        if err then
          return err
        else
          successes = successes + 1
          assert(successes == 2)
          if successes > 0 then
            FS.unlinkSync(fileName)
          end
        end
      end)
    end)
  end)
end)