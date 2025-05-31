--[[lit-meta
  name = "luvit/prompt"
  version = "2.0.1"
  dependencies = {
    "luvit/readline@2.0.0"
  }
  homepage = "https://github.com/luvit/lit/blob/master/deps/prompt.lua"
  description = "A simple wrapper around readline for quick terminal prompts."
  tags = {"tty", "prompt"}
  license = "MIT"
  author = { name = "Tim Caswell" }
]]

local readLine = require('readline').readLine

local function assertResume(thread, ...)
  local success, err = coroutine.resume(thread, ...)
  if not success then
    error(debug.traceback(thread, err), 0)
  end
end

return function (options)
  -- Wrapper around readline to provide a nice blocking version for coroutines
  return function (message, default)
    local thread = coroutine.running()

    message = message .. ": "
    if default then
      message = message .. "(" .. default .. ") "
    end

    local value
    repeat
      readLine(message, options, function (err, line, reason)
        if err then
          return assertResume(thread, nil, err)
        end
        return assertResume(thread, line, reason)
      end)
      value = assert(coroutine.yield())
      if default and #value == 0 then
        value = default
      end
    until #value > 0
    return value
  end
end
