--[[

Copyright 201202015 The Luvit Authors. All Rights Reserved.

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
  local path = require('path')
  local fs = require('fs')
  local __dirname = module.dir
  local name = 'test-pipe'
  local tmp_file = path.join(__dirname, 'tmp', name)
  fs.writeFileSync(tmp_file, "")

  local streamEventClosed = false
  local streamEventAlreadyClosed = false

  local file_path = path.join(__dirname, name..'.lua')
  test("pipe", function()
    local fp = fs.createReadStream(file_path)
    local null = fs.createWriteStream(tmp_file)
    null:on('error', function(err)
      p(err)
      if err.message:find('write after end') then
        streamEventAlreadyClosed = true
      end
    end)
    null:on('closed', function()
      p('closed')
      streamEventClosed = true
      null:write('after closed')
    end)
    fp:pipe(null)

    assert(true)
  end)
end)