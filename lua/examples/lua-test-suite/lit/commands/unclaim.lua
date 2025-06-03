return function ()
  local core = require('core')()
  if #args ~= 2 then
    error("Usage: lit unclaim orgname")
  end
  assert(core.unclaim(args[2]))
end
