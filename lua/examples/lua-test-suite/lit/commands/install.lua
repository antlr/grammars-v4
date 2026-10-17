return function ()
  local uv = require('uv')
  local core = require('core')()

  if #args < 2 then
    core.installDeps(uv.cwd())
  else
    local list = {}
    for i = 2, #args do
      list[#list + 1] = args[i]
    end
    core.installList(uv.cwd(), list)
  end
end
