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

if not pcall(require, 'ffi') then
  return
end

local ffi = require('ffi')
local timer = require('timer')
local los = require('los')

require('tap')(function(test)
  test('ffi', function(expect)
    local is_windows, timeout, success, onTimeout

    is_windows = los.type() == 'win32'
    timeout = 20

    if is_windows then
      -- approximated the call signature DWORDs are unsinged ints and BOOLs are ints
      ffi.cdef[[ unsigned int  SleepEx(unsigned int dwMilliseconds, int bAlertable); ]]
    else
      ffi.cdef[[ int poll(struct pollfd *fds, unsigned long nfds, int timeout); ]]
    end

    function onTimeout()
      assert(success)
    end

    -- On the next tick the poll will have unblocked the run loop
    timer.setTimeout(1, expect(onTimeout))

    if is_windows then
      if ffi.C.SleepEx(timeout, 0) then
        success = true
      end
    else
      if ffi.C.poll(nil, 0, timeout) then
        success = true
      end
    end
  end)
end)
