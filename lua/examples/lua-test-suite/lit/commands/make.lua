return function ()
  local core = require('core')()
  local uv = require('uv')
  local pathJoin = require('luvi').path.join

  local cwd = uv.cwd()
  local source = args[2] and pathJoin(cwd, args[2])
  local target = args[3] and pathJoin(cwd, args[3])
  local luvi_source = args[4] and pathJoin(cwd, args[4])
  if not source or uv.fs_access(source, "r") then
    core.make(source or cwd, target, luvi_source)
  else
    core.makeUrl(args[2], target, luvi_source)
  end
end
