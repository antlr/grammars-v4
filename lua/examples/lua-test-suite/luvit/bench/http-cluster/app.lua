--[[

Copyright 2014 The Luvit Authors. All Rights Reserved.

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

-- This file represents the high-level logic of the clustered app.
return function (req)
  -- p(req)
  -- print("Writing response headers")
  local body = string.format("%s %s\n", req.method, req.path)
  local res = {
    code = 200,
    { "Server", "Luvit" },
    { "Content-Type", "text/plain" },
    { "Content-Length", #body },
  }
  if req.keepAlive then
    res[#res + 1] = { "Connection", "Keep-Alive" }
  end

  return res, body
end
