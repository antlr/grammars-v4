--[[lit-meta
  name = "creationix/weblit-router"
  version = "3.0.1"
  dependencies = {
    'luvit/querystring@2.0.0'
  }
  description = "Weblit is a webapp framework designed around routes and middleware layers."
  tags = {"weblit", "router", "framework"}
  license = "MIT"
  author = { name = "Tim Caswell" }
  homepage = "https://github.com/creationix/weblit/blob/master/libs/weblit-app.lua"
]]

local parseQuery = require('querystring').parse

local quotepattern = '(['..("%^$().[]*+-?"):gsub("(.)", "%%%1")..'])'
local function escape(str)
    return str:gsub(quotepattern, "%%%1")
end

local function compileGlob(glob)
  local parts = {"^"}
  for a, b in glob:gmatch("([^*]*)(%**)") do
    if #a > 0 then
      parts[#parts + 1] = escape(a)
    end
    if #b > 0 then
      parts[#parts + 1] = "(.*)"
    end
  end
  parts[#parts + 1] = "$"
  local pattern = table.concat(parts)
  return function (string)
    return string and string:match(pattern)
  end
end

local function compileRoute(route)
  local parts = {"^"}
  local names = {}
  for a, b, c, d in route:gmatch("([^:]*):([_%a][_%w]*)(:?)([^:]*)") do
    if #a > 0 then
      parts[#parts + 1] = escape(a)
    end
    if #c > 0 then
      parts[#parts + 1] = "(.*)"
    else
      parts[#parts + 1] = "([^/]*)"
    end
    names[#names + 1] = b
    if #d > 0 then
      parts[#parts + 1] = escape(d)
    end
  end
  if #parts == 1 then
    return function (string)
      if string == route then return {} end
    end
  end
  parts[#parts + 1] = "$"
  local pattern = table.concat(parts)
  return function (string)
    local matches = {string:match(pattern)}
    if #matches > 0 then
      local results = {}
      for i = 1, #matches do
        results[i] = matches[i]
        results[names[i]] = matches[i]
      end
      return results
    end
  end
end

local function newRouter()
  local handlers = {}
  local router = {}

  function router.use(handler)
    handlers[#handlers + 1] = handler
    return router
  end

  function router.route(options, handler)
    local method = options.method
    local path = options.path and compileRoute(options.path)
    local host = options.host and compileGlob(options.host)
    local filter = options.filter
    router.use(function (req, res, go)
      if method and req.method ~= method then return go() end
      if host and not host(req.headers.host) then return go() end
      if filter and not filter(req) then return go() end
      local params
      if path then
        local pathname, query = req.path:match("^([^?]*)%??(.*)")
        params = path(pathname)
        if not params then return go() end
        if #query > 0 then
          req.query = parseQuery(query)
        end
      end
      req.params = params or {}
      return handler(req, res, go)
    end)
    return router
  end

  function router.run(req, res, go)
    local len = #handlers
    local function run(i)
      if i > len then
        return (go or function () end)()
      end
      return handlers[i](req, res, function ()
        return run(i + 1)
      end)
    end
    run(1)
  end

  return router
end

return {
  newRouter = newRouter,
  escape, escape,
  compileGlob, compileGlob,
  compileRoute, compileRoute,
}
