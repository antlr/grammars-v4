local loadstring = loadstring or load

local bundle = require('luvi').bundle
loadstring(bundle.readfile("../luvit-loader.lua"), "bundle:luvit-loader.lua")()

local uv = require('uv')
local unpack = unpack or table.unpack

coroutine.wrap(function ()
  local success, err = xpcall(function ()
    local script = args[1]
    local fn = assert(loadfile(script))
    fn(unpack(args))
  end, debug.traceback)
  if not success then
    error(err)
  end
end)()
uv.run()
