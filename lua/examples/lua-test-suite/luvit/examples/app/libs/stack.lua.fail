--[[

Copyright 2012 The Luvit Authors. All Rights Reserved.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS-IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

--]]

local url = require('url')
local stack = {}

function stack.stack(...)
  local errorHandler = stack.errorHandler
  local handle = errorHandler

  local layers  = {...}
  for i = #layers, 1, -1 do
    local layer = layers[i]
    local child = handle
    handle = function(req, res)
      local success, err = pcall(function ()
        layer(req, res, function (err)
          if err then return errorHandler(req, res, err) end
          child(req, res)
        end)
      end)
      if not success and err then
        errorHandler(req, res, err)
      end
    end
  end

  return handle
end

local function core(req, res, continue) continue() end

-- Build a composite stack made of several layers
function stack.compose(...)
  local layers = {...}

  -- Don't bother composing singletons
  if #layers == 1 then return layers[1] end

  local stack = core
  for i = #layers, 1, -1 do
    local layer = layers[i]
    local child = stack
    stack = function (req, res, continue)
      local success, err = pcall(function ()
        layer(req, res, function (err)
          if err then return continue(err) end
          child(req, res, continue)
        end)
      end)
      if not success and err then
        continue(err)
      end
    end
  end

  return stack
end

-- Mounts a substack app at a url subtree
function stack.mount(mountpoint, ...)

  if mountpoint:sub(#mountpoint) == "/" then
    mountpoint = mountpoint:sub(1, #mountpoint - 1)
  end

  local matchpoint = mountpoint .. "/"

  return stack.translate(mountpoint, matchpoint, ...)

end

function stack.translate(mountpoint, matchpoint, ...)
  local stack = stack.compose(...)

  return function(req, res, continue)
    local url = req.url
    local uri = req.uri

    if not (url:sub(1, #matchpoint) == matchpoint) then return continue() end

    -- Modify the url
    if not req.real_url then req.real_url = url end

    req.url = url:sub(#mountpoint + 1)
    -- We only want to set the parsed uri if there was already one there
    if req.uri then req.uri = url.parse(req.url) end

    stack(req, res, function (err)
      req.url = url
      req.uri = uri
      continue(err)
    end)

  end
end

local Debug = require('debug')
function stack.errorHandler(req, res, err)
  if err then
    res.code = 500
    res:finish(Debug.traceback(err) .. "\n")
    return
  end
  res.code = 404
  res:finish("Not Found\n")
end

return stack

