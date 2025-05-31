return function ()
  local core = require('core')()

  assert(core.config.upstream, "Must have upstream to sync to")
  core.sync(args[2], args[3])
end
