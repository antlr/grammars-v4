
local coroutine = require('coroutine')
local debug = require 'debug'
local fiber = {}
local unpack = unpack or table.unpack ---@diagnostic disable-line: deprecated

function fiber.new(block) return function (callback)
  local paused
  local co = coroutine.create(block)

  local function formatError(err)
    local stack = debug.traceback(co, tostring(err))
    if type(err) == "table" then
      err.message = stack
      return err
    end
    return stack
  end

  local function check(success, ...)
    if not success then
      if callback then
        return callback(formatError(...))
      else
        error(formatError(...))
      end
    end
    if not paused then
      return callback and callback(nil, ...)
    end
    paused = false
  end

  local function wait(fn)
    if type(fn) ~= "function" then
      error("can only wait on functions")
    end
    local sync, ret
    fn(function (...)
      if sync == nil then
        sync = true
        ret = {...}
        return
      end
      check(coroutine.resume(co, ...))
    end)
    if sync then
      return unpack(ret)
    end
    sync = false
    paused = true
    return coroutine.yield()
  end

  local function await(fn)
    local results = {wait(fn)}
    if results[1] then
      error(results[1])
    end
    return unpack(results, 2)
  end

  check(coroutine.resume(co, wait, await))

end end

return fiber
