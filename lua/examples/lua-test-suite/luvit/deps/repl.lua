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
  name = "luvit/repl"
  version = "2.1.3"
  dependencies = {
    "luvit/utils@2.0.0",
    "luvit/readline@2.0.0",
  }
  license = "Apache 2"
  homepage = "https://github.com/luvit/luvit/blob/master/deps/repl.lua"
  description = "Advanced auto-completing repl for luvit lua."
  tags = {"luvit", "tty", "repl"}
]]

local uv = require('uv')
local utils = require('utils')
local pathJoin = require('luvi').path.join
local Editor = require('readline').Editor
local History = require('readline').History

local _builtinLibs = { 'buffer', 'childprocess', 'codec', 'core',
  'dgram', 'dns', 'fs', 'helpful', 'hooks', 'http-codec', 'http',
  'https', 'json', 'los', 'net', 'pretty-print',
  'querystring', 'readline', 'timer', 'url', 'utils',
  'stream', 'tls', 'path'
}

return function (stdin, stdout, greeting)

  local req, mod = require('require')(pathJoin(uv.cwd(), "repl"))
  local oldGlobal = _G
  local global = setmetatable({
    require = req,
    module = mod,
  }, {
    __index = function (_, key)
      if key == "thread" then return coroutine.running() end
      return oldGlobal[key]
    end
  })

  if greeting then
    stdout:write(greeting)
    stdout:write('\n')
  end

  local c = utils.color

  local function gatherResults(success, ...)
    local n = select('#', ...)
    return success, { n = n, ... }
  end

  local function printResults(results)
    for i = 1, results.n do
      results[i] = utils.dump(results[i])
    end
    stdout:write(table.concat(results, '\t'))
    stdout:write('\n')
  end

  local buffer = ''

  local function evaluateLine(line)
    if line == "<3" or line == "♥" or line == "❤" then
      stdout:write("I " .. c("err") .. "♥" .. c() .. " you too!\n")
      return '> '
    end
    local chunk  = buffer .. line
    local f, err = load('return ' .. chunk, 'REPL', nil, global) -- first we prefix return

    if not f then
      f, err = load(chunk, 'REPL', nil, global) -- try again without return
    end


    if f then
      buffer = ''
      local success, results = gatherResults(xpcall(f, debug.traceback))

      if success then
        -- successful call
        if results.n > 0 then
          printResults(results)
        end
      elseif type(results[1]) == 'string' then
        -- error
        stdout:write(results[1])
        stdout:write('\n')
      else
        -- error calls with non-string message objects will pass through debug.traceback without a stacktrace added
        stdout:write('error with unexpected error message type (' .. type(results[1]) .. '), no stacktrace available\n')
      end
    else

      if err:match "'<eof>'$" then
        -- Lua expects some more input; stow it away for next time
        buffer = chunk .. '\n'
        return '>> '
      else
        stdout:write(err)
        stdout:write('\n')
        buffer = ''
      end
    end

    return '> '
  end

  local function completionCallback(line)
    local base, sep, rest = string.match(line, "^(.*)([.:])(.*)")
    if not base then
      rest = line
    end
    local prefix = string.match(rest, "^[%a_][%a%d_]*")
    if prefix and prefix ~= rest then return end
    local scope
    if base then
      local f = load("return " .. base, nil, nil, global)
      if not f then return {} end
      local ok
      ok, scope = pcall(f)
      if not ok then return {} end
    else
      base = ''
      sep = ''
      scope = global
    end
    local matches = {}
    local prop = sep ~= ':'
    while type(scope) == "table" do
      for key, value in pairs(scope) do
        if (prop or (type(value) == "function")) and
           ((not prefix) or (string.match(key, "^" .. prefix))) then
          matches[key] = true
        end
      end
      scope = getmetatable(scope)
      scope = scope and scope.__index
    end
    local items = {}
    for key in pairs(matches) do
      items[#items + 1] = key
    end
    table.sort(items)
    if #items == 1 then
      return base .. sep .. items[1]
    elseif #items > 1 then
      return items
    end
  end

  local function start(historyLines, onSaveHistoryLines)
    local prompt = "> "
    local history = History.new()
    if historyLines then
      history:load(historyLines)
    end
    local editor = Editor.new({
      stdin = stdin,
      stdout = stdout,
      completionCallback = completionCallback,
      history = history
    })

    local function onLine(err, line)
      assert(not err, err)
      coroutine.wrap(function ()
        if line then
          prompt = evaluateLine(line)
          editor:readLine(prompt, onLine)
          -- TODO: break out of >> with control+C
        elseif onSaveHistoryLines then
          onSaveHistoryLines(history:dump())
        end
      end)()
    end

    editor:readLine(prompt, onLine)

    -- Namespace builtin libs to make the repl easier to play with
    -- Requires with filenames with a - in them will be camelcased
    -- e.g. pretty-print -> prettyPrint
    for _, lib in pairs(_builtinLibs) do
      local requireName = lib:gsub('-.', function (char) return char:sub(2):upper() end)
      local req = string.format('%s = require("%s")', requireName, lib)
      evaluateLine(req)
    end
  end

  return {
    start = start,
    evaluateLine = evaluateLine,
  }
end
