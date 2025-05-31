local snapshot = require('snapshot')
local i = 0
local S1 = snapshot()
local potential = {}

return function (_, res)
  collectgarbage()
  collectgarbage()
  local memoryUsed = 1024 * collectgarbage("count")
  local S2 = snapshot()
  -- Find new heap objects since last snapshot
  local c = 0
  local details = {}
  for k, v in pairs(potential) do
    if S2[k] then
      c = c + 1
      details[c] = v
    end
  end
  table.sort(details)
  potential = {}
  local total = 0
  local pot = 0
  for k,v in pairs(S2) do
    total = total + 1
    if S1[k] == nil then
      pot = pot + 1
      potential[k] = v
    end
  end
  S1 = S2
  i = i + 1
  res.code = 200
  res.headers["Content-Type"] = "text/plain"
  res.body = "id: " .. i ..
    "\nheap bytes: " .. memoryUsed ..
    "\nheap count: " .. total ..
    "\npotential count: " .. pot ..
    "\nleak count: " .. c ..
    "\n\n" .. table.concat(details, "\n") ..
    "\n"

end
