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

-- Create a luvit powered main that does the luvit CLI interface
return require('./init')(function (...)

  local luvi = require('luvi')
  local uv = require('uv')
  local utils = require('utils')
  local package = require('./package.lua')

  local startRepl
  local combo
  local script
  local extra = {}

  local function usage()
    print("Usage: " .. args[0] .. " [options] script.lua [arguments]"..[[


  Options:
    -h, --help          Print this help screen.
    -v, --version       Print the version.
    -e code_chunk       Evaluate code chunk and print result.
    -i, --interactive   Enter interactive repl after executing script.
    -l libname          Require library.
    -n, --no-color      Disable colors.
    -c, --16-colors     Use simple ANSI colors
    -C, --256-colors    Use 256-mode ANSI colors
                        (Note, if no script is provided, a repl is run instead.)
  ]])
    startRepl = false
  end

  local function version()
    print('luvit version: ' .. package.version)
    print('luvi version: ' .. require('luvi').version)
    for k, v in pairs(require('luvi').options) do
      print(k .. ' version: ' .. tostring(v))
    end
    startRepl = false
  end


  local shorts = {
    h = "help",
    v = "version",
    e = "eval",
    i = "interactive",
    l = "require",
    n = "no-color",
    c = "16-colors",
    C = "256-colors",
  }

  local flags = {
    help = usage,
    version = version,
    eval = function ()
      local repl = require('repl')(utils.stdin, utils.stdout)
      combo = repl.evaluateLine
      startRepl = false
    end,
    interactive = function ()
      startRepl = true
    end,
    ["no-color"] = function ()
      utils.loadColors(false)
    end,
    ["16-colors"] = function ()
      utils.loadColors(16)
    end,
    ["256-colors"] = function ()
      utils.loadColors(256)
    end,
  }

  local i, arg = 1
  arg = args[i]
  while arg do
    if script then
      extra[#extra + 1] = arg
    elseif combo then
      combo(arg)
      combo = nil
    elseif string.sub(arg, 1, 1) == "-" then
      local flag
      if (string.sub(arg, 2, 2) == "-") then
        flag = string.sub(arg, 3)
      else
        arg = string.sub(arg, 2)
        flag = shorts[arg] or arg
      end
      if flag=='require' then
        i=i+1
        require(args[i])
      else
        local fn = flags[flag] or usage
        fn()
      end
    else
      script = arg
    end
    i = i + 1
    arg = args[i]
  end

  if combo then error("Missing flag value") end

  if startRepl == nil and not script then startRepl = true end

  if script then
    require(luvi.path.join(uv.cwd(), script))
  end

  if startRepl then
    local env = require('env')
    local pathJoin = require('luvi').path.join
    local fs = require('fs')
    local c = utils.color
    local greeting = "Welcome to the " .. c("err") .. "L" .. c("quotes") .. "uv" .. c("table") .. "it" .. c() .. " repl!"
    local historyFile
    if require('los').type() == "win32" then
      historyFile = pathJoin(env.get("APPDATA"), "luvit_history")
    else
      historyFile = pathJoin(env.get("HOME"), ".luvit_history")
    end
    local lines = fs.readFileSync(historyFile) or ""
    local function saveHistory(lines)
      fs.writeFileSync(historyFile, lines)
    end
    require('repl')(utils.stdin, utils.stdout, greeting, ...).start(lines, saveHistory)

  end
end, ...)

