local utils = require('utils')
local Object = require('core').Object

require('tap')(function(test)
  test('utils', function(expect)

    local BindHelper = Object:extend()

    function BindHelper:func1(arg1, callback, ...)
      assert(self ~= nil)
      callback(arg1)
    end

    function BindHelper:func2(arg1, arg2, callback)
      assert(self ~= nil)
      callback(arg1, arg2)
    end

    function BindHelper:func3(arg1, arg2, arg3, callback)
      assert(self ~= nil)
      callback(arg1, arg2, arg3)
    end

    local testObj = BindHelper:new()
    local bound

    bound = utils.bind(BindHelper.func1, testObj)
    bound('hello world', function(arg1)
      assert(arg1 == 'hello world')
    end)
    bound('hello world1', function(arg1)
      assert(arg1 == 'hello world1')
    end)

    bound = utils.bind(BindHelper.func1, testObj, 'hello world')
    bound(function(arg1)
      assert(arg1 == 'hello world')
    end)
    bound(function(arg1)
      assert(arg1 == 'hello world')
    end)
    bound(function(arg1)
      assert(arg1 == 'hello world')
    end)

    bound = utils.bind(BindHelper.func2, testObj)
    bound('hello', 'world', function(arg1, arg2)
      assert(arg1 == 'hello')
      assert(arg2 == 'world')
    end)
    bound('hello', 'world', function(arg1, arg2)
      assert(arg1 == 'hello')
      assert(arg2 == 'world')
    end)

    bound = utils.bind(BindHelper.func2, testObj, 'hello')
    bound('world', function(arg1, arg2)
      assert(arg1 == 'hello')
      assert(arg2 == 'world')
    end)

    bound = utils.bind(BindHelper.func3, testObj)
    bound('hello', 'world', '!', function(arg1, arg2, arg3)
      assert(arg1 == 'hello')
      assert(arg2 == 'world')
      assert(arg3 == '!')
    end)

    bound = utils.bind(BindHelper.func3, testObj)
    bound('hello', nil, '!', function(arg1, arg2, arg3)
      assert(arg1 == 'hello')
      assert(arg2 == nil)
      assert(arg3 == '!')
    end)

    bound = utils.bind(BindHelper.func3, testObj, 'hello', 'world')
    bound('!', function(arg1, arg2, arg3)
      assert(arg1 == 'hello')
      assert(arg2 == 'world')
      assert(arg3 == '!')
    end)

    bound = utils.bind(BindHelper.func3, testObj, 'hello', nil)
    bound('!', function(arg1, arg2, arg3)
      assert(arg1 == 'hello')
      assert(arg2 == nil)
      assert(arg3 == '!')
    end)

    bound = utils.bind(BindHelper.func3, testObj, nil, 'world')
    bound('!', function(arg1, arg2, arg3)
      assert(arg1 == nil)
      assert(arg2 == 'world')
      assert(arg3 == '!')
    end)

    local tblA = { 1, 2, 3 }
    local tblB = { tblA, 'test1', 'test2', { tblA } }
    local s = utils.dump(tblB, true, true)
    assert(s == "{ { 1, 2, 3 }, 'test1', 'test2', { { 1, 2, 3 } } }")

    local Error = require('core').Error
    local MyError = Error:extend()
    assert(pcall(utils.dump, MyError))
  end)
end)
