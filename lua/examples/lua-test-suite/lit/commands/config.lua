return function ()
  local log = require('log').log
  local core = require('core')()
  for key, value in pairs(core.config) do
    log(key, value, "string")
  end
end
