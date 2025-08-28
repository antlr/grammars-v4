return function ()
  local config = require('core')().config
  local log = require('log').log

  log("upstream", "disabled", "nil")
  config.upstream = nil
  config.save()
end
