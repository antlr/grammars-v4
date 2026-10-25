local timer = require'timer'
local thread = require'thread'

local interval = timer.setInterval(1000, function ()
    print('Main Thread',thread.self(), os.date())
end)

print("Main ...running...")

function entry(cli)
  local timer = require'timer'
  local thread = require'thread'
  local interval = timer.setInterval(1000, function ()
    print(cli,thread.self(),os.date())
  end)
end

thread.start(entry,'cli1')
thread.start(entry,'cli2')
thread.start(entry,'cli3')
