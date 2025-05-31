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

-- Hooks are intended to be a global event emitter for internal
-- Luvit events. For example, process.exit and signals can feed
-- through this emitter.

--[[lit-meta
  name = "luvit/hooks"
  version = "2.0.0"
  dependencies = {
    "luvit/core@2.0.0",
  }
  license = "Apache 2"
  homepage = "https://github.com/luvit/luvit/blob/master/deps/hooks.lua"
  description = "Core global event hooks for luvit."
  tags = {"luvit", "events", "hooks"}
]]
local Emitter = require('core').Emitter
--luacheck: new globals exports
local e = exports or {}
setmetatable(e, Emitter.meta)
if e.init then e:init() end
return e
