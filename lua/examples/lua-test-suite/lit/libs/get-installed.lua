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

local pathJoin = require('luvi').path.join
local pkgQuery = require('pkg').query

return function (fs, rootPath)
  local deps = {}
  local function check(dir)
    local iter = fs.scandir(dir)
    if not iter then return end
    for entry in iter do
      local baseName
      if not entry.type then
        local stat, err = fs.stat(pathJoin(dir, entry.name))
        entry.type = stat and stat.type
      end
      if entry.type == "file" then
        baseName = entry.name:match("^(.*)%.lua$")
      elseif entry.type == "directory" then
        baseName = entry.name
      end
      if baseName then
        local path, meta
        path = pathJoin(dir, entry.name)
        meta, path = pkgQuery(fs, path)
        if meta then
          meta.fs = fs
          meta.path = path
          meta.location = dir:match("[^/]+$")
          deps[baseName] = meta
        end
      end
    end
  end
  check(pathJoin(rootPath, "deps"))
  check(pathJoin(rootPath, "libs"))
  return deps
end
