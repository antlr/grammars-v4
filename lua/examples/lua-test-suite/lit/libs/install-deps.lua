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

local import = require('import')
local modes = require('git').modes
local export = require('export')
local pathJoin = require('luvi').path.join
local filterTree = require('rules').filterTree
local log = require('log').log

-- Given a db tree and a set of dependencies, create a new tree with the deps
-- folder synthisized from the deps list.

local function toDb(db, rootHash, deps, nativeOnly)
  local tree = db.loadAs("tree", rootHash)
  local depsTree = {}
  for alias, meta in pairs(deps) do
    local entry = {}
    local kind, hash
    if meta.hash then
      hash = meta.hash
      kind = meta.kind
      if nativeOnly and kind == "tree" then
        hash = filterTree(db, "deps/" .. alias, hash, nil, nativeOnly)
      end
    else
      kind, hash = import(db, meta.fs, meta.path, nil, nativeOnly)
    end
    if kind == "blob" then
      entry.name = alias .. ".lua"
    else
      entry.name = alias
    end
    entry.mode = assert(modes[kind])
    entry.hash = hash

    depsTree[#depsTree + 1] = entry
  end
  if #depsTree == 0 then return rootHash end
  tree[#tree + 1] = {
    name = "deps",
    mode = modes.tree,
    hash = db.saveAs("tree", depsTree)
  }
  return db.saveAs("tree", tree)
end

local function toFs(db, fs, rootPath, deps, nativeOnly)
  for alias, meta in pairs(deps) do
    if meta.hash then
      local path = pathJoin(rootPath, "deps", alias)
      local hash = meta.hash
      if meta.kind == "blob" then
        path = path .. ".lua"
      elseif nativeOnly then
        hash = filterTree(db, path, hash, nil, nativeOnly)
      end
      log("installing package", string.format("%s@v%s", meta.name, meta.version), "highlight")
      export(meta.db, hash, fs, path)
    end
  end
end

return {
  toDb = toDb,
  toFs = toFs,
}
