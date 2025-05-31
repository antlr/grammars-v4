--[[

Copyright 2012-2014 The Luvit Authors. All Rights Reserved.

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

local timer = require('timer')

require('tap')(function (test)
  test("simple timeout", function (expect)
    timer.setTimeout(20, expect(function (arg1)
      assert(arg1 == 'test1')
    end), "test1")
  end)

  test("simple interval", function (expect)
    local count = 0
    local interval
    interval = timer.setInterval(20, expect(function (arg1)
      count = count + 1
      assert(arg1 == 'test2')
      if count == 2 then
        timer.clearInterval(interval)
      end
    end, 2), 'test2')
  end)

  test("Canceled timer", function ()
    local timeout = timer.setTimeout(200, function ()
      assert(nil, "Should not get here!")
    end)
    timer.clearTimeout(timeout)
  end)

  test("setImmediate", function (expect)
    timer.setImmediate(expect(function (arg1)
      assert(arg1 == 'test3')
    end), 'test3')
  end)

  test('double close', function ()
    local t1 = timer.setTimeout(200, function()
      assert(nil, "Should not get here!")
    end)
    timer.clearTimeout(t1)
    timer.clearTimeout(t1)
  end)
end)

