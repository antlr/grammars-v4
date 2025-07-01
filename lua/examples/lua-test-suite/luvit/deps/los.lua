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

--[[lit-meta
  name = "luvit/los"
  version = "2.0.1"
  license = "Apache 2"
  homepage = "https://github.com/luvit/luvit/blob/master/deps/los.lua"
  description = "Tiny helper to get os name in luvit."
  tags = {"os"}
]]

local uv = require('uv')

local function get_os_name()
  -- shortcuts for luajit if available
  local has_jit, jit = pcall(require, 'jit')
  if has_jit and jit and jit.os then
    return jit.os
  end

  local has_ffi, ffi = pcall(require, 'ffi')
  if has_ffi and ffi and ffi.os then
    return ffi.os
  end

  -- Handles windows
  if os and os.getenv then
    if os.getenv('OS') then
      return os.getenv('OS')
    end
  end

  -- use libuv provided uname if possible, but it's not always available
  if uv.os_uname then
    local info = uv.os_uname()
    if info then
      return info.sysname
    end
  end

  -- Handles most other posix platforms
  if io and io.popen then
    local uname_child = io.popen('uname -s')
    if uname_child then
      local os_name = uname_child:read('*l')
      uname_child:close()
      return os_name
    end
  end

  return 'other'
end

local os_patterns = {
  ['windows'] = 'win32',
  ['linux'] = 'linux',
  ['mac'] = 'darwin',
  ['darwin'] = 'darwin',
  ['^mingw'] = 'win32',
  ['^msys'] = 'win32',
  ['^cygwin'] = 'win32',
  ['bsd$'] = 'bsd',
  ['posix'] = 'posix',
}

local os_name = string.lower(get_os_name())
for pattern, name in pairs(os_patterns) do
  if os_name:match(pattern) then
    os_name = name
    break
  end
end

local function type()
  return os_name
end

return { type = type }
