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
  name = "luvit/readline"
  version = "2.2.0"
  dependencies = {
    "luvit/ustring@2.0.0",
  }
  homepage = "https://github.com/luvit/luvit/blob/master/deps/readline.lua"
  description = "A readline interface for terminals in pure lua."
  tags = {"readline", "tty"}
  license = "Apache 2"
  author = { name = "Tim Caswell" }
]]

-- Heavily inspired by ljlinenoise : <http://fperrad.github.io/ljlinenoise/>

local ustring = require "ustring"
local emptyline = ustring.new()
local sub = ustring.sub
local chlen = ustring.chlen
local gmatch = ustring.gmatch
local find = ustring.find
local remove = table.remove
local insert = table.insert
local concat = table.concat
local tostring = tostring
local unpack = unpack or table.unpack ---@diagnostic disable-line: deprecated

local History = {}
function History:add(line)
  assert(type(line) == "string", "line must be string")
  while #self >= self.maxLength do
    remove(self, 1)
  end
  insert(self, line)
  return true
end
function History:setMaxLength(length)
  assert(type(length) == "number", "max length length must be number")
  self.maxLength = length
  while #self > length do
    remove(self, 1)
  end
  return true
end
function History:clean()
  for i = 1, #self do
    self[i] = nil
  end
  return true
end
function History:dump()
  return concat(self, "\n") .. '\n'
end
function History:load(data)
  assert(type(data) == "string", "history dump required as string")
  for line in gmatch(data, "[^\n]+") do
    insert(self, line)
  end
  return true
end
function History:updateLastLine(line)
  self[#self] = line
end
History.__index = History
function History.new()
  local history = { maxLength = 100 }
  return setmetatable(history, History)
end

local Editor = {}
function Editor:refreshLine()
  local line = self.line
  if self.cover then
    line = string.rep(self.cover, #line)
  end

  local position = self.position
  if self.cover then
    position = (#self.cover * (position - 1)) + 1
  end

  -- Cursor to left edge
  local command = "\x1b[0G"
  -- Write the prompt and the data before cursor in buffer content.
               .. self.prompt .. tostring(line:sub(1,self.position - 1))
  -- Save the position,erase to right and write data left.
               .. "\x1b7\x1b[0K" .. tostring(line:sub(self.position,-1))
  -- Move cursor to original position.
               .. "\x1b8"
  self.stdout:write(command)
end
function Editor:insertAbove(line)
  -- Cursor to left edge
  local command = "\x1b[0G"
  -- Erase to right
               .. "\x1b[0K"

  self.stdout:write(command .. line .. "\n", function()
    self:refreshLine()
  end)
end
function Editor:insert(character)
  local display = self.cover and string.rep(self.cover, #character) or character
  local line = self.line
  local position = self.position
  character = ustring.new(character)
  if #line == position - 1 then
    self.line = line .. character
    self.position = position + #character
    self.stdout:write(display)
  else
    -- Insert the letter in the middle of the line
    self.line = sub(line, 1, position - 1) .. ustring.new(character) .. sub(line, position)
    self.position = position + #character
    self:refreshLine()
  end
  self.history:updateLastLine(tostring(self.line))
end
function Editor:moveLeft()
  if self.position > 1 then
    self.position = self.position - 1
    self:refreshLine()
  end
end
function Editor:moveRight()
  if self.position - 1 ~= #self.line then
    self.position = self.position + 1
    self:refreshLine()
  end
end
function Editor:getHistory(delta)
  local history = self.history
  local length = #history
  local index = self.historyIndex
  if length > 1 then
    index = index + delta
    if index < 1 then
      index = 1
    elseif index > length then
      index = length
    end
    if index == self.historyIndex then return end
    local line = ustring.new(self.history[index])
    self.line = line
    self.historyIndex = index
    self.position = #line + 1
    self:refreshLine()
  end
end
function Editor:backspace()
  local line = self.line
  local position = self.position
  if position > 1 and #line > 0 then
    self.line = sub(line, 1, position - 2) .. sub(line, position)
    self.position = position - 1
    self.history:updateLastLine(tostring(self.line))
    self:refreshLine()
  end
end
function Editor:delete()
  local line = self.line
  local position = self.position
  if position > 0 and #line > 0 then
    self.line = sub(line, 1, position - 1) .. sub(line, position + 1)
    self.history:updateLastLine(tostring(self.line))
    self:refreshLine()
  end
end
function Editor:swap()
  local line = self.line
  local position = self.position
  if position > 1 and position <= #line then
    self.line = sub(line, 1, position - 2)
             .. sub(line, position, position)
             .. sub(line, position - 1, position - 1)
             .. sub(line, position + 1)
    if position ~= #line then
      self.position = position + 1
    end
    self.history:updateLastLine(tostring(self.line))
    self:refreshLine()
  end
end
function Editor:deleteLine()
  self.line = emptyline
  self.position = 1
  self.history:updateLastLine(tostring(self.line))
  self:refreshLine()
end
function Editor:deleteEnd()
  self.line = sub(self.line, 1, self.position - 1)
  self.history:updateLastLine(tostring(self.line))
  self:refreshLine()
end
function Editor:moveHome()
  self.position = 1
  self:refreshLine()
end

function Editor:moveEnd()
  self.position = #self.line + 1
  self:refreshLine()
end
local function findLeft(line, position, wordPattern)
  local pattern = wordPattern .. "$"
  if position == 1 then return 1 end
  local s
  repeat
    local start = sub(line, 1, position - 1)
    s = find(start, pattern)
    if not s then
      position = position - 1
    end
  until s or position == 1
  return s or position
end

function Editor:deleteWord()
  local position = self.position
  local line = self.line
  self.position = findLeft(line, position, self.wordPattern)
  self.line = sub(line, 1, self.position - 1) .. sub(line, position)
  self:refreshLine()
end

function Editor:jumpLeft()
  self.position = findLeft(self.line, self.position, self.wordPattern)
  self:refreshLine()
end
function Editor:jumpRight()
  local _, e = find(self.line, self.wordPattern, self.position)
  self.position = e and e + 1 or #self.line + 1
  self:refreshLine()
end
function Editor:clearScreen()
  self.stdout:write('\x1b[H\x1b[2J')
  self:refreshLine()
end
function Editor:beep()
  self.stdout:write('\x07')
end
function Editor:complete()
  if not self.completionCallback then
    return self:beep()
  end
  local line = self.line
  local position = self.position
  local res = self.completionCallback(tostring(sub(line, 1, position)))
  if not res then
    return self:beep()
  end
  local typ = type(res)
  if typ == "string" then
    res = ustring.new(res)
    self.line = res .. sub(line, position + 1)
    self.position = #res + 1
    self.history:updateLastLine(tostring(self.line))
  elseif typ == "table" then
    print()
    print(unpack(res))
  end
  self:refreshLine()
end

local function escapeKeysForDisplay(keys)
  return string.gsub(keys, '[%c\\\128-\255]', function(c)
    local b = string.byte(c, 1)
    if b < 10 then return '\\00' .. b end
    if b <= 31 then return '\\0' .. b end
    if b == 92 then return '\\\\' end
    if b >= 128 and b <= 255 then return '\\' .. b end
  end)
end

-- an array of tables so that the iteration order is consistent
-- each entry is an array with two entries: a table and a function
-- the table can contain any number of the following:
--   numbers (to be compared to the char value),
--   strings (to be compared to the input string that has been truncated to the same length),
--   functions (to be called with the (key, char) values and returns either the consumed keys or nil)
-- the function recieves the Editor instance as the first parameter and the consumedKeys as the second
--   its returns will be propagated to Editor:onKey if either of them are non-nil
--   note: the function is only called if the key handler is the one doing the consuming
local keyHandlers =
{
  -- Enter
  {{13}, function(self)
    local history = self.history
    local line = self.line
    -- Only record new history if it's non-empty and new
    if #line > 0 and history[#history - 1] ~= line then
      history[#history] = tostring(line)
    else
      history[#history] = nil
    end
    return tostring(self.line)
  end},
  -- Tab
  {{9}, function(self)
    self:complete()
  end},
  -- Control-C
  {{3}, function(self)
    self.stdout:write("^C\n")
    if #self.line > 0 then
      self:deleteLine()
    else
      return false, "SIGINT in readLine"
    end
  end},
  -- Backspace, Control-H
  {{127, 8}, function(self)
    self:backspace()
  end},
  -- Control-D
  {{4}, function(self)
    if #self.line > 0 then
      self:delete()
    else
      self.history:updateLastLine()
      return nil, "EOF in readLine"
    end
  end},
  -- Control-T
  {{20}, function(self)
    self:swap()
  end},
  -- Up Arrow, Control-P
  {{'\027[A', 16}, function(self)
    self:getHistory(-1)
  end},
  -- Down Arrow, Control-N
  {{'\027[B', 14}, function(self)
    self:getHistory(1)
  end},
  -- Right Arrow, Control-F
  {{'\027[C', 6}, function(self)
    self:moveRight()
  end},
  -- Left Arrow, Control-B
  {{'\027[D', 2}, function(self)
    self:moveLeft()
  end},
  -- Home Key, Home for terminator, Home for CMD.EXE, Control-A
  {{'\027[H', '\027OH', '\027[1~', 1}, function(self)
    self:moveHome()
  end},
  -- End Key, End for terminator, End for CMD.EXE, Control-E
  {{'\027[F', '\027OF', '\027[4~', 5}, function(self)
    self:moveEnd()
  end},
  -- Control-U
  {{21}, function(self)
    self:deleteLine()
  end},
  -- Control-K
  {{11}, function(self)
    self:deleteEnd()
  end},
  -- Control-L
  {{12}, function(self)
    self:clearScreen()
  end},
  -- Control-W
  {{23}, function(self)
    self:deleteWord()
  end},
  -- Delete Key
  {{'\027[3~'}, function(self)
    self:delete()
  end},
  -- Control Left Arrow, Alt Left Arrow (iTerm.app), Alt Left Arrow (Terminal.app)
  {{'\027[1;5D', '\027\027[D', '\027b'}, function(self)
    self:jumpLeft()
  end},
  -- Control Right Arrow, Alt Right Arrow (iTerm.app), Alt Right Arrow (Terminal.app)
  {{'\027[1;5C', '\027\027[C', '\027f'}, function(self)
    self:jumpRight()
  end},
  -- Alt Up Arrow (iTerm.app), Page Up
  {{'\027\027[A', '\027[5~'}, function(self)
    self:getHistory(-10)
  end},
  -- Alt Down Arrow (iTerm.app), Page Down
  {{'\027\027[B', '\027[6~'}, function(self)
    self:getHistory(10)
  end},
  -- Printable characters
  {{function(key, char) return char > 31 and key:sub(1,chlen(key:byte())) or nil end}, function(self, consumedKeys)
    self:insert(consumedKeys)
  end},
}

function Editor:onKey(key)
  local char = string.byte(key, 1)
  local consumedKeys

  for _, keyHandler in ipairs(keyHandlers) do
    local handledKeys = keyHandler[1]
    local handlerFn = keyHandler[2]
    for _, handledKey in ipairs(handledKeys) do
      if type(handledKey) == "number" then
        consumedKeys = handledKey == char and key:sub(1,1) or nil
      elseif type(handledKey) == "string" then
        -- test against the first key using the same strlen as the handled key
        local testKey = (type(handledKey) == "string" and #key >= #handledKey) and key:sub(1,#handledKey) or nil
        consumedKeys = (testKey and testKey == handledKey) and testKey or nil
      elseif type(handledKey) == "function" then
        consumedKeys = handledKey(key, char)
      end
      if consumedKeys ~= nil then
        local ret, err = handlerFn(self, consumedKeys)
        if err ~= nil or ret ~= nil then
          return ret, err
        end
        break
      end
    end
    if consumedKeys ~= nil then break end
  end

  if consumedKeys ~= nil then
    assert(#consumedKeys > 0)
    if #consumedKeys < #key then
      local unconsumedKeys = key:sub(#consumedKeys+1)
      if #unconsumedKeys > 0 then
        self:onKey(unconsumedKeys)
      end
    end
  else
    self:insertAbove(string.format("Unhandled key(s): %s", escapeKeysForDisplay(key)))
  end
  return true
end
function Editor:readLine(prompt, callback)

  local onKey, finish

  self.prompt = prompt
  self.promptLength = #prompt
  self.columns = self.stdout.get_winsize and self.stdout:get_winsize() or 80

  function onKey(err, key)
    local r, out, reason = pcall(function ()
      assert(not err, err)
      return self:onKey(key)
    end)
    if r then
      if out == true then return end
      return finish(nil, out, reason)
    else
      return finish(out)
    end
  end

  function finish(...)
    self.stdin:read_stop()
    self.stdin:set_mode(0)
    self.stdout:write('\n')
    return callback(...)
  end

  self.line = emptyline
  self.position = 1
  self.stdout:write(self.prompt)
  self.history:add(tostring(self.line))
  self.historyIndex = #self.history

  self.stdin:set_mode(1)
  self.stdin:read_start(function (...)
    return coroutine.wrap(onKey)(...)
  end)

end
Editor.__index = Editor
function Editor.new(options)
  options = options or {}
  local history = options.history or History.new()

  if not (options.stdin and options.stdout) then
    local prettyPrint = require('pretty-print')
    options.stdin = options.stdin or prettyPrint.stdin
    options.stdout = options.stdout or prettyPrint.stdout
  end

  local editor = {
    wordPattern = options.wordPattern or "%w+",
    history = history,
    completionCallback = options.completionCallback,
    stdin = options.stdin,
    stdout = options.stdout,
    cover = options.cover,
  }
  return setmetatable(editor, Editor)
end

local function readLine(prompt, options, callback)
  if type(options) == "function" and callback == nil then
    callback, options = options, nil
  end
  local editor = Editor.new(options)
  editor:readLine(prompt, callback)
  return editor
end

return {
  History = History,
  Editor = Editor,
  readLine = readLine,
}
