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
  local fs = require('fs')
  local Path = require('path')
  local string = require('string')
  local bit = require('bit')
  local los = require('los')

  local is_windows = los.type() == 'win32'
  local __dirname = module.dir

  test('fs chmod', function(expect)
    local mode_async
    local mode_sync

    -- On Windows chmod is only able to manipulate read-only bit
    -- TODO: test on windows
    if is_windows then
      mode_async = 256 --[[tonumber('0400', 8)]] -- read-only
      mode_sync = 438  --[[tonumber('0600', 8)]] -- read-write
    else
      mode_async = 511 --[[tonumber('0777', 8)]]
      mode_sync = 420 --[[tonumber('0644', 8)]]
    end

    local file1 = Path.join(__dirname, 'fixtures', 'a.lua')
    local file2 = Path.join(__dirname, 'fixtures', 'a1.lua')

    local function maskMode(mode, mask)
      return bit.band(mode, mask or 511 --[[tonumber('0777',8)]])
    end

    fs.chmod(file1, mode_async, expect(function(err)
      assert(not err)
      if is_windows then
        assert(maskMode(maskMode(fs.statSync(file1).mode), mode_async))
      else
        assert(mode_async == maskMode(fs.statSync(file1).mode))
      end

      -- TODO: accept mode in number
      assert(fs.chmodSync(file1, mode_sync))
      if is_windows then
        assert(maskMode(maskMode(fs.statSync(file1).mode), mode_sync))
      else
        assert(mode_sync == maskMode(fs.statSync(file1).mode))
      end
    end))

    fs.open(file2, 'a', tonumber('0666', 8), expect(function(err, fd)
      assert(not err)
      fs.fchmod(fd, mode_async, expect(function(err)
        assert(not err)
        if is_windows then
          assert(maskMode(maskMode(fs.fstatSync(fd).mode), mode_async))
        else
          assert(mode_async == maskMode(fs.fstatSync(fd).mode))
        end

        -- TODO: accept mode in number
        assert(fs.fchmodSync(fd, mode_sync))
        if is_windows then
          assert(maskMode(maskMode(fs.fstatSync(fd).mode), mode_sync))
        else
          assert(mode_sync == maskMode(fs.fstatSync(fd).mode))
        end
        fs.close(fd)
      end))
    end))

    -- lchmod
    if fs.lchmod then
      local link = Path.join(__dirname, 'tmp', 'symbolic-link')
      fs.unlinkSync(link)
      fs.symlinkSync(file2, link)

      fs.lchmod(link, mode_async, expect(function(err)
        assert(not err)
        p(fs.lstatSync(link).mode)
        assert(mode_async == maskMode(fs.lstatSync(link).mode))

        -- TODO: accept mode in number
        fs.lchmodSync(link, string.format('%o', mode_sync))
        assert(mode_sync == maskMode(fs.lstatSync(link).mode))
      end))
    end
  end)
end)
