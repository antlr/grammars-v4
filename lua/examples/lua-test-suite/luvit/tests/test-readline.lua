--[[

Copyright 2015 The Luvit Authors. All Rights Reserved.

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

local readline = require('readline')
local prettyPrint = require('pretty-print')
local Editor = readline.Editor

require('tap')(function(test)
  test('readline Editor:onKey', function(expect)

    -- If stdin is not a TTY, we can't run these tests.
    if not prettyPrint.stdin.set_mode then return end

    local editor = Editor.new({
      stdin = prettyPrint.stdin,
      stdout = prettyPrint.stdout
    })

    -- need to call readLine to initialize some variables
    editor:readLine("", function() end)
    -- but don't want to actually read anything
    editor.stdin:read_stop()
    editor.stdin:set_mode(0)

    -- starting position
    assert(#editor.line == 0)
    assert(editor.position == 1)

    -- single key
    editor:onKey('a')
    assert(tostring(editor.line) == "a")
    assert(editor.position == 2)

    -- left arrow
    editor:onKey('\027[D')
    assert(tostring(editor.line) == "a")
    assert(editor.position == 1)

    -- multiple keys
    editor:onKey('abc')
    assert(tostring(editor.line) == "abca")
    assert(editor.position == 4)

    -- multiple control sequences (left arrows)
    editor:onKey('\027[D\027[D\027[D')
    assert(tostring(editor.line) == "abca")
    assert(editor.position == 1)

    -- mixture of characters and control sequences
    editor:onKey('a\027[Db\027[Dc\027[Dd')
    assert(tostring(editor.line) == "dcbaabca")
    assert(editor.position == 2)
  end)
end)
