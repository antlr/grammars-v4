print(require("foo").bar)
print(coroutine.wrap(print)())
getmetatable({}).__index = function() end
