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
  local math = require('math')
  local os = require('os')
  local string = require('string')
  local los = require('los')
  local __filename = module.path

  test('fs utime', function()
    local is_windows = los.type() == 'win32'

    local tests_ok = 0
    local tests_run = 0

    function stat_resource(resource)
      if type(resource) == 'string' then
        return FS.statSync(resource)
      else
        -- ensure mtime has been written to disk
        FS.fsyncSync(resource)
        return FS.fstatSync(resource)
      end
    end

    function check_mtime(resource, mtime)
      local stats = stat_resource(resource)
      local real_mtime = stats.mtime
      -- check up to single-second precision
      -- sub-second precision is OS and FS dependant
      return math.floor(mtime) == math.floor(real_mtime.sec)
    end

    function expect_errno(syscall, resource, err, errno)
      if err and (err.code == errno or err.code == 'ENOSYS') then
        tests_ok = tests_ok + 1
      else
        p(string.format('FAILED: %s %s %s %s', syscall, resource, tostring(err), errno))
      end
    end

    function expect_ok(syscall, resource, err, atime, mtime)
      if not err and check_mtime(resource, mtime) or
          err and err.code == 'ENOSYS' then
        tests_ok = tests_ok + 1
      else
        p(string.format('expect_ok FAILED: %s %s %s %f %f', syscall, resource, tostring(err), atime, mtime))
      end
    end

    -- the tests assume that __filename belongs to the user running the tests
    -- this should be a fairly safe assumption testing against a temp file
    -- would be even better though (node doesn't have such functionality yet)
    function runTest(atime, mtime, callback)

      local fd, err
      --
      -- test synchronized code paths, these functions throw on failure
      --
      function syncTests()
        FS.utimeSync(__filename, atime, mtime)
        expect_ok('utimeSync', __filename, undefined, atime, mtime)
        tests_run = tests_run + 1

        -- some systems don't have futime
        -- if there's an error, it should be ENOSYS
        local ok, err
        ok, err = pcall(function()
          tests_run = tests_run + 1
          FS.futimeSync(fd, atime, mtime)
          expect_ok('futimeSync', fd, undefined, atime, mtime)
        end)
        if not ok then
          expect_errno('futimeSync', fd, err, 'ENOSYS')
        end

        ok, err = pcall(FS.utimeSync, 'foobarbaz', atime, mtime)
        expect_errno('utimeSync', 'foobarbaz', err, 'ENOENT')
        tests_run = tests_run + 1

        ok, err = pcall(FS.futimeSync, -1, atime, mtime)
        expect_errno('futimeSync', -1, err, 'EBADF')
        tests_run = tests_run + 1
      end

      --
      -- test async code paths
      --
      if (type(atime)=='table') then
        atime = atime.sec
      end
      if (type(mtime)=='table') then
        mtime = mtime.sec
      end
      FS.utime(__filename, atime, mtime, function(err)
        expect_ok('utime', __filename, err, atime, mtime)

        FS.utime('foobarbaz', atime, mtime, function(err)
          expect_errno('utime', 'foobarbaz', err, 'ENOENT')

          -- don't close this fd
          if is_windows then
            fd = FS.openSync(__filename, 'r+')
          else
            fd = FS.openSync(__filename, 'r')
          end

          FS.futime(fd, atime, mtime, function(err)
            expect_ok('futime', fd, err, atime, mtime)

            FS.futime(-1, atime, mtime, function(err)
              expect_errno('futime', -1, err, 'EBADF')
              syncTests()
              callback()
            end)
            tests_run = tests_run + 1
          end)
          tests_run = tests_run + 1
        end)
        tests_run = tests_run + 1
      end)
      tests_run = tests_run + 1
    end

    local stats = FS.statSync(module.path)

    function newDate(dateStr)
      if dateStr == nil then
        return os.time()
      end

      return os.time{
        year = tonumber(string.sub(dateStr, 1, 4)),
        month = tonumber(string.sub(dateStr, 6, 7)),
        day = tonumber(string.sub(dateStr, 9, 10)),
        hour = tonumber(string.sub(dateStr, 12, 13)),
        min = tonumber(string.sub(dateStr, 15, 16))
      }
    end

    runTest(newDate('1982-09-10 13:37'), newDate('1982-09-10 13:37'), function()
      runTest(newDate(), newDate(), function()
        runTest(123456.789, 123456.789, function()
          runTest(stats.mtime, stats.mtime, function()
            -- done
          end)
        end)
      end)
    end)
--[[
    process:on('exit', function()
      p('Tests run / ok:' ..  tests_run .. '/' .. tests_ok)
      assert(tests_ok == tests_run)
    end)
--]]
  end)
end)
