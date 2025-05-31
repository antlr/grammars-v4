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
  local dir = Path.join(module.dir, 'tmp')
  local filename = Path.join(dir, 'truncate-file.txt')
  local data = string.rep('x', 1024 * 16)

  test('fs truncate', function()
    local stat
    _, err = FS.statSync(dir)
    if err then FS.mkdirpSync(dir, "0755") end

    -- truncateSync
    FS.writeFileSync(filename, data)
    stat = FS.statSync(filename)
    assert(stat.size == 1024 * 16)

    FS.truncateSync(filename, 1024)
    stat = FS.statSync(filename)
    assert(stat.size == 1024)

    FS.truncateSync(filename)
    stat = FS.statSync(filename)
    assert(stat.size == 0)

    -- ftruncateSync
    FS.writeFileSync(filename, data)
    local fd = FS.openSync(filename, 'r+')

    stat = FS.statSync(filename)
    assert(stat.size == 1024 * 16)

    FS.ftruncateSync(fd, 1024)
    stat = FS.statSync(filename)
    assert(stat.size == 1024)

    FS.ftruncateSync(fd)
    stat = FS.statSync(filename)
    assert(stat.size == 0)

    FS.closeSync(fd)

    function testTruncate(cb)
      FS.writeFile(filename, data, function(er)
        if er then
          return cb(er)
        end
        FS.stat(filename, function(er, stat)
          if er then
            return cb(er)
          end
          assert(stat.size == 1024 * 16)

          FS.truncate(filename, 1024, function(er)
            if er then
              return cb(er)
            end
            FS.stat(filename, function(er, stat)
              if er then
                return cb(er)
              end
              assert(stat.size == 1024)

              FS.truncate(filename, function(er)
                if er then
                  return cb(er)
                end
                FS.stat(filename, function(er, stat)
                  if er then
                    return cb(er)
                  end
                  assert(stat.size == 0)
                  cb()
                end)
              end)
            end)
          end)
        end)
      end)
    end


    function testFtruncate(cb)
      FS.writeFile(filename, data, function(er)
        if er then
          return cb(er)
        end
        FS.stat(filename, function(er, stat)
          if er then
            return cb(er)
          end
          assert(stat.size == 1024 * 16)

          FS.open(filename, 'w', function(er, fd)
            if er then
              return cb(er)
            end
            FS.ftruncate(fd, 1024, function(er)
              if er then
                return cb(er)
              end
              FS.stat(filename, function(er, stat)
                if er then
                  return cb(er)
                end
                assert(stat.size == 1024)

                FS.ftruncate(fd, function(er)
                  if er then
                    return cb(er)
                  end
                  FS.stat(filename, function(er, stat)
                    if er then
                      return cb(er)
                    end
                    assert(stat.size == 0)
                    FS.close(fd, cb)
                    FS.unlinkSync(filename)
                  end)
                end)
              end)
            end)
          end)
        end)
      end)
    end

    -- async tests
    local success = 0
    testTruncate(function(er)
      if er then
        return er
      end
      success = success + 1
      testFtruncate(function(er)
        if er then
          return er
        end
        success = success + 1
      end)
    end)
--[[
    process:on('exit', function()
      assert(success == 2)
      p('ok')
    end)
--]]
  end)
end)
