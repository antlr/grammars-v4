--[[

Copyright 2014-2015 The Luvit Authors. All Rights Reserved.

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

local spawn = require('coro-spawn')
local split = require('coro-split')

return function (command, ...)
  local child, err = spawn(command, {
    args = {...},
    -- Tell spawn to create coroutine pipes for stdout and stderr only
    stdio = {nil, true, true}
  })

  if err then
    return nil, err
  end
  
  local stdout, stderr, code, signal

  -- Split the coroutine into three sub-coroutines and wait for all three.
  split(function ()
    local parts = {}
    for data in child.stdout.read do
      parts[#parts + 1] = data
    end
    stdout = table.concat(parts)
  end, function ()
    local parts = {}
    for data in child.stderr.read do
      parts[#parts + 1] = data
    end
    stderr = table.concat(parts)
  end, function ()
    code, signal = child.waitExit()
  end)


  return stdout, stderr, code, signal
end
