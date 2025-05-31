return function ()
  local core = require('core')()
  if #args ~= 3 then
    error("Usage: lit share orgname friendname")
  end
  assert(core.share(args[2], args[3]))
end
