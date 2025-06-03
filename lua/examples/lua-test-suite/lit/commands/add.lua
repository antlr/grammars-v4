return function ()
  local core = require('core')()
  local uv = require('uv')
  local pathJoin = require('luvi').path.join

  local cwd = uv.cwd()
  if #args > 1 then
    for i = 2, #args do
      core.add(pathJoin(cwd, args[i]))
    end
  else
    core.add(cwd)
  end
end
