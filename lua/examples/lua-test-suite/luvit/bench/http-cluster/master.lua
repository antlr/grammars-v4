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

local uv = require('uv')
local pathJoin = require('luvi').path.join

local server = uv.new_tcp()
uv.tcp_bind(server, "0.0.0.0", 8080)

local workerPath = pathJoin(module.dir, "worker.lua")
for i = 1, #uv.cpu_info() do
  local pipe = uv.new_pipe(true)
  local child, pid
  child, pid = uv.spawn(uv.exepath(), {
    args = {workerPath},
    stdio = {0,1,2,pipe},
  }, function (code, signal)
    print("Worker " .. i .. " exited with code " .. code .. " and signal " .. signal)
    uv.close(child)
  end)
  uv.write2(pipe, ".", server, function (err)
    assert(not err, err)
    uv.close(pipe)
  end)
  print("Worker " .. i .. " spawned at pid " .. pid)
end
print("Test HTTP server at http://127.0.0.1:8080/")
