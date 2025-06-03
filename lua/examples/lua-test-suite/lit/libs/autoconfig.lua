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

local log = require('log').log
local fs = require('coro-fs')
local env = require('env')

local prefix
if package.config:sub(1,1) == '\\' and env.get('APPDATA') then
  prefix = env.get("APPDATA") .. "\\"
else
  prefix = env.get("HOME") .. "/."
end

local configFile = env.get("LIT_CONFIG") or (prefix .. "litconfig")

local loaded = false
local config = {}
return function ()
  local data = fs.readFile(configFile)
  if data then
    loaded = true
    log("load config", configFile)
    for key, value in string.gmatch(data, "([^:\n]+): *([^\n]+)") do
      config[key] = value
    end
  end

  local function save()
    if loaded then
      log("update config", configFile)
    else
      log("create config", configFile)
      loaded = true
    end
    local lines = {}
    for key, value in pairs(config) do
      lines[#lines + 1] = key .. ": " .. value
    end
    fs.writeFile(configFile, table.concat(lines, "\n") .. '\n')
  end

  local dirty = false
  if not config.defaultUpstream then
    config.defaultUpstream = "wss://lit.luvit.io/"
    if not loaded then
      config.upstream = config.defaultUpstream
    end
    dirty = true
  end

  if not config.database then
    config.database = prefix .. "litdb.git"
    dirty = true
  end

  if dirty then save() end

  setmetatable(config, {
    __index = {save = save}
  })

  return config
end
