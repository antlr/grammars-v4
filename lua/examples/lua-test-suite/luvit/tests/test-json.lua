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

local JSON = require('json')
local deepEqual = require('deep-equal')

require('tap')(function(test)
  test('smoke', function()
    assert(JSON.stringify({a = 'a'}) == '{"a":"a"}')
    assert(deepEqual({a = 'a'}, JSON.parse('{"a":"a"}')))
  end)
  test('parse invalid json', function()
    for _, x in ipairs({ '', ' ', '{', '[', '{"f":', '{"f":1', '{"f":1', }) do
      local status, _, result = pcall(JSON.parse, x)
      assert(status)
      assert(result)
    end
    for _, x in ipairs({ '{]', '[}', }) do
      local status, _, result = pcall(JSON.parse, x)
      assert(status)
      assert(result)
    end
  end)
  test('parse valid json', function()
    for _, x in ipairs({ '[]', '{}', }) do
      local _, result = pcall(JSON.parse, x)
      assert(type(result) == 'table')
    end
  end)
  test('stringify', function()
    assert(JSON.stringify() == 'null')
    for _, x in ipairs({ {}, {1, 2, 3}, {a = 'a'}, 'string', 0, 0.1, 3.1415926, true, false, }) do
      local status, result = pcall(JSON.stringify, x)
      assert(status)
      assert(type(result) == 'string')
    end
  end)
  test('edge cases', function()
    assert(JSON.stringify({}) == '[]')

    -- escaped strings
    assert(JSON.stringify('a"b\tc\nd') == '"a\\"b\\tc\\nd"')
    assert(JSON.parse('"a\\"b\\tc\\nd"') == 'a"b\tc\nd')

    -- booleans
    assert(JSON.stringify(true) == 'true')
    assert(JSON.stringify(false) == 'false')
    assert(JSON.parse('true') == true)
    assert(JSON.parse('false') == false)
  end)
  test('strict', function()
    for _, x in ipairs({ '{f:1}', "{'f':1}", }) do
      local status, _, _ = pcall(JSON.parse, x)
      assert(status)
    end
  end)
  test('unicode', function()
    local s = "{\"f\":\"こんにちは 世界\"}"
    local obj = JSON.parse(s)
    assert(obj.f and obj.f == "こんにちは 世界")
  end)
end)
