--[[lit-meta
  name = "creationix/weblit-app"
  version = "3.2.1"
  dependencies = {
    'luvit/coro-net@3.0.0',
    'luvit/http-codec@3.0.0',
    'luvit/querystring@2.0.0',
    'creationix/weblit-server@3.0.0',
    'creationix/weblit-router@3.0.0'
  }
  description = "Weblit is a webapp framework designed around routes and middleware layers."
  tags = {"weblit", "router", "framework"}
  license = "MIT"
  author = { name = "Tim Caswell" }
  homepage = "https://github.com/creationix/weblit/blob/master/libs/weblit-app.lua"
]]

-- Ignore SIGPIPE if it exists on platform
local uv = require('uv')
if uv.constants.SIGPIPE then
  uv.new_signal():start("sigpipe")
end

local router = require('weblit-router').newRouter()
local server = require('weblit-server').newServer(router.run)

-- Forward router methods from app instance
local serverMeta = {}
function serverMeta:__index(name)
  if type(router[name]) == "function" then
    return function(...)
      router[name](...)
      return self
    end
  end
end
setmetatable(server, serverMeta)

return server
