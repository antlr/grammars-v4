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
  local Writable = require('stream').Writable

  local text = [[Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do
eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim
veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo
consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse
cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non
proident, sunt in culpa qui officia deserunt mollit anim id est laborum.]]


  test('fs.readstream length', function(expect)
    local tmp_file = path.join(module.dir, 'test_readstream1.txt')
    fs.writeFileSync(tmp_file, text)

    local options = {
      flags = 'r',
      mode = '0644',
      chunk_size = 65536,
      offset = nil,
      fd = nil,
      length = 16, -- should stop at 16
    }

    local sink = Writable:new()
    sink.str = {}
    function sink:write(chunk) table.insert(self.str, chunk) end

    local function onEnd()
      local expected = string.sub(text, 1, options.length)
      local readString = table.concat(sink.str, "")
      p(expected, readString)
      assert(expected == readString)
      fs.unlinkSync(tmp_file)
    end

    local fp = fs.createReadStream(tmp_file, options)
    fp:once('end', expect(onEnd))
    fp:pipe(sink)
  end)

  test('fs.readstream offset and length', function(expect)
    local tmp_file = path.join(module.dir, 'test_readstream2.txt')
    fs.writeFileSync(tmp_file, text)

    local options = {
      flags = 'r',
      mode = '0644',
      chunk_size = 65536,
      offset = 19,
      fd = nil,
      length = 16, -- should stop at 16
    }

    local sink = Writable:new()
    sink.str = {}
    function sink:write(chunk) table.insert(self.str, chunk) end

    local function onEnd()
      local expected = string.sub(text, options.offset + 1, options.length + options.offset)
      local readString = table.concat(sink.str, "")
      p(expected, readString)
      assert(expected == readString)
      fs.unlinkSync(tmp_file)
    end

    local fp = fs.createReadStream(tmp_file, options)
    fp:once('end', expect(onEnd))
    fp:pipe(sink)
  end)

  test('fs.readstream offset only', function(expect)
    local tmp_file = path.join(module.dir, 'test_readstream3.txt')
    fs.writeFileSync(tmp_file, text)

    local options = {
      flags = 'r',
      mode = '0644',
      chunk_size = 65536,
      offset = 16,
      fd = nil,
      length = nil,
    }

    local sink = Writable:new()
    sink.str = {}
    function sink:write(chunk) table.insert(self.str, chunk) end

    local function onEnd()
      assert(string.sub(text, options.offset + 1) == table.concat(sink.str, ""))
      fs.unlinkSync(tmp_file)
    end

    local fp = fs.createReadStream(tmp_file, options)
    fp:once('end', expect(onEnd))
    fp:pipe(sink)
  end)
end)
