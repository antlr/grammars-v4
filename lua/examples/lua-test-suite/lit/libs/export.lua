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
local modes = require('git').modes

-- Export a db hash to the fs at path.

-- db is a git db instance
-- fs is a coro-fs instance
return function (db, rootHash, fs, rootPath)
  local function exportEntry(path, hash, mode)
    local kind, value = db.loadAny(hash)
    if kind == "tag" then
      return exportEntry(path, value.object)
    end
    if not mode then
      mode = modes[kind]
    else
      assert(modes.toType(mode) == kind, "Git kind mismatch")
    end
    if mode == modes.tree then
      for i = 1, #value do
        local entry = value[i]
        exportEntry(pathJoin(path, entry.name), entry.hash, entry.mode)
      end
    elseif mode == modes.sym then
      local success, err = fs.symlink(value, path)
      if not success and err:match("^ENOENT:") then
        assert(fs.mkdirp(pathJoin(path, "..")))
        assert(fs.symlink(value, path))
      end
    elseif modes.isFile(mode) then
      local success, err = fs.writeFile(path, value)
      if not success and err:match("^ENOENT:") then
        assert(fs.mkdirp(pathJoin(path, "..")))
        assert(fs.writeFile(path, value))
      end
      assert(fs.chmod(path, mode))
    else
      error("Unsupported mode at " .. path .. ": " .. mode)
    end
    return kind
  end

  return exportEntry(rootPath, rootHash)
end
