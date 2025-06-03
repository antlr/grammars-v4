local timer = require('timer')

print("Starting 200ms interval")
local interval = timer.setInterval(200, function ()
  p("on_interval")
end)
print("Starting 1000ms timer")
timer.setTimeout(1000, function ()
  p("on_timeout!")
  timer.clearInterval(interval)
end)


