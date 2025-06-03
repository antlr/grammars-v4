local uv = require('uv')

local timer = uv.new_timer()
local timer2 = uv.new_timer()

timer:start(2000, 0, function (...)
  p("on_timeout", ...)
  timer2:stop()
  timer:stop()
  timer:close(p)
  timer2:close(p)
end)

timer2:start(333, 333, function (...)
  p("on_interval", ...)
  local period = timer2:get_repeat()
  p("period", period)
  timer2:set_repeat(period / 1.2 + 1);
end)

p(timer, timer2)
