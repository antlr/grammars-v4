--[[

Copyright 2016 The Luvit Authors. All Rights Reserved.

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
local getContext = require('./context')
local bioWrap = require('./biowrap')

local function assertResume(thread, ...)
  local success, err = coroutine.resume(thread, ...)
  if not success then
    error(debug.traceback(thread, err), 0)
  end
end

return function (socket, options, callback)
  if options == true then options = {} end
  local ctx = getContext(options)
  local thread
  if not callback then
    thread = coroutine.running()
  end
  bioWrap(ctx, options.server, socket, callback or function (err, ssocket)
    return assertResume(thread, ssocket, err)
  end, options.servername)
  if not callback then
    return coroutine.yield()
  end
end
