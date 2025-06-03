local Emitter = require('core').Emitter

--[Example 1]
local emitter = Emitter:new()

emitter:on("foo", function (...)
  p("on_foo", ...)
end)

emitter:once("foo", function (...)
  p("on_foo2", ...)
end)

p(emitter)

emitter:emit("foo", 1, 2, 3)
emitter:emit("foo", 4, 5, 6)

--[Example 2]
local parent = Emitter:new()
local child = Emitter:new()

child:propagate('error', parent)
parent:on('error', p)

child:emit('error', 'from child')

