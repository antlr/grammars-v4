return function ()
  local core = require('core')()
  if #args ~= 2 then
    error("Usage: lit claim orgname")
  end
  assert(core.claim(args[2]))
end
