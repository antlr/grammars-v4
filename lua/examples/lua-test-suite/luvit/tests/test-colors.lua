--[[

Copyright 2012 The Luvit Authors. All Rights Reserved.

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

local utils = require('utils')
local dump = utils.dump
local strip = utils.strip

require('tap')(function (test)

  test("Recursive values", function ()
    local data = { a = 'value' }
    data.data = data
    local out = dump(data)
    local stripped = strip(out)
    print("recursive", out, dump(stripped))
    -- iteration order is not consistent, so we need to check either ordering
    local matches = string.match(stripped, "{ a = 'value', data = table: 0x%x+ }") ~= nil
    matches = matches or string.match(stripped, "{ data = table: 0x%x+, a = 'value' }") ~= nil
    assert(matches)
  end)

  test("string escapes", function ()
    local tests = {
      '\000\001\002\003\004\005\006\a\b\t\n\v\f\r\014\015',  "'\\000\\001\\002\\003\\004\\005\\006\\a\\b\\t\\n\\v\\f\\r\\014\\015'",
      '\016\017\018\019\020\021\022\023\024\025\026\027\028\029\030\031',  "'\\016\\017\\018\\019\\020\\021\\022\\023\\024\\025\\026\\027\\028\\029\\030\\031'",
      ' !"#$%&\'()*+,-./', '\' !"#$%&\\\'()*+,-./\'',
      '0123456789:;<=>?',  "'0123456789:;<=>?'",
      '@ABCDEFGHIJKLMNO',  "'@ABCDEFGHIJKLMNO'",
      'PQRSTUVWXYZ[\\]^_', "'PQRSTUVWXYZ[\\\\]^_'",
      '`abcdefghijklmno',  "'`abcdefghijklmno'",
      'pqrstuvwxyz{|}',  "'pqrstuvwxyz{|}'",
    }
    for i = 1, 16, 2 do
      local out = dump(tests[i])
      local stripped = strip(out)
      print(out, dump(stripped))
      assert(stripped == tests[i + 1])
    end

  end)

  test("Smart quotes in string escapes", function ()
    local tests = {
      "It's a wonderful life",
      '"It\'s a wonderful life"',

      'To "quote" or not to "quote"...',
      '\'To "quote" or not to "quote"...\'',

      "I've always liked \"quotes\".",
      '\'I\\\'ve always liked "quotes".\'',
    }

    for i = 1, 6, 2 do
      local out = dump(tests[i])
      local stripped = strip(out)
      print(out, dump(stripped))
      assert(stripped == tests[i + 1])
    end
  end)

  test("Color mode switching", function ()
    local data = {42,true,"A\nstring"}

    utils.loadColors(false)
    local plain = dump(data)
    utils.loadColors()
    print("plain", plain, dump(plain))
    assert(plain == "{ 42, true, 'A\\nstring' }")

    utils.loadColors(16)
    local colored = dump(data)
    utils.loadColors()
    print("colored", colored, dump(colored))
    assert(colored == "\027[1;30m{ \027[0m\027[1;33m42\027[0m\027[1;30m, \027[0m\027[0;33mtrue\027[0m\027[1;30m, \027[0m\027[1;32m'\027[0;32mA\027[1;32m\\n\027[0;32mstring\027[1;32m'\027[0m \027[1;30m}\027[0m")

    utils.loadColors(256)
    local super = dump(data)
    utils.loadColors()
    print("super", super, dump(super))
    assert(super == "\027[38;5;247m{ \027[0m\027[38;5;202m42\027[0m\027[38;5;240m, \027[0m\027[38;5;220mtrue\027[0m\027[38;5;240m, \027[0m\027[38;5;40m'\027[38;5;34mA\027[38;5;46m\\n\027[38;5;34mstring\027[38;5;40m'\027[0m \027[38;5;247m}\027[0m")
  end)

end)

