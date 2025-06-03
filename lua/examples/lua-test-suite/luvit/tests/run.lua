--[[

Copyright 2012 The Luvit Authors. All Rights Reserved.

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

local tap = require("tap")
local uv = require("uv")

local req = uv.fs_scandir("tests")

while true do
  local name = uv.fs_scandir_next(req)

  if not name then
    break
  end
  if type(name) == "table" then
    name = name.name
  end
  local match = string.match(name, "^test%-(.*).lua$")
  if match then
    local path = "./test-" .. match
    tap(match)
    require(path)
  end
end

-- run the tests!
tap(true)
