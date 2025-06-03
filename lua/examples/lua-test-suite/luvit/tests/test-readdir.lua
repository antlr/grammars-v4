--[[

Copyright 2015 The Luvit Authors. All Rights Reserved.

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

require('tap')(function(test)
  local path = require('path')
  local fs = require('fs')

  test('fs.readdir', function(expect)
    local dir, _, err, files, onReadDir
    dir = path.join(module.dir, 'tmp', 'readdir')
    _, err = fs.statSync(dir)
    if err then fs.mkdirpSync(dir, "0755") end

    fs.writeFileSync(path.join(dir, "1"), "")
    fs.writeFileSync(path.join(dir, "2"), "")
    fs.writeFileSync(path.join(dir, "3"), "")

    files = fs.readdirSync(dir)
    assert(#files == 3)

    function onReadDir(err, files)
      assert(err == nil)
      assert(#files == 3)
      fs.unlink(path.join(dir, "1"), function() end)
      fs.unlink(path.join(dir, "2"), function() end)
      fs.unlink(path.join(dir, "3"), function() end)
    end

    fs.readdir(dir, expect(onReadDir))
  end)

end)

