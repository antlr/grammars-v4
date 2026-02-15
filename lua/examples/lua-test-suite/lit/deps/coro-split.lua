--[[lit-meta
  name = "luvit/coro-split"
  version = "2.0.3"
  homepage = "https://github.com/luvit/lit/blob/master/deps/coro-split.lua"
  description = "An coro style helper for running tasks concurrently."
  tags = {"coro", "split"}
  license = "MIT"
  author = { name = "Tim Caswell" }
]]

-- Split takes several functions as input and runs them in concurrent coroutines.
-- The split function will itself block and wait for all three to finish.
-- The results of the functions will be returned from split.
local unpack = unpack or table.unpack

local function assertResume(thread, ...)
  local success, err = coroutine.resume(thread, ...)
  if not success then
    error(debug.traceback(thread, err), 0)
  end
end

return function (...)
  local tasks = {...}
  for i = 1, #tasks do
    assert(type(tasks[i]) == "function", "all tasks must be functions")
  end
  local thread = coroutine.running()
  local left = #tasks
  local results = {}
  local yielded = false
  local function check()
    left = left - 1
    if left == 0 and yielded then
      assertResume(thread, unpack(results))
    end
  end
  for i = 1, #tasks do
    coroutine.wrap(function ()
      results[i] = tasks[i]()
      check()
    end)()
  end
  if left <= 0 then
    return unpack(results)
  end
  yielded = true
  return coroutine.yield()
end
