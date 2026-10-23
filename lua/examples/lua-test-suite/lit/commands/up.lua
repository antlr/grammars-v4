return function ()
  local config = require('core')().config
  local log = require('log').log

  local upstream = args[2] or config.defaultUpstream
  log("upstream", upstream, "highlight")
  config.upstream = upstream
  config.save()
end
