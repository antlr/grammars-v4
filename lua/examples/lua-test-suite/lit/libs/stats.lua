local jsonStringify = require('json').stringify
local uv = require('uv')
local scandir = require('coro-fs').scandir
local exists = require('coro-fs').exists

return function (_, res)
  local handles = {}
  uv.walk(function (handle)
    local name = tostring(handle)
    if name:match("^uv_tcp_t:") then
      local peer = handle:getpeername()
      local sock = handle:getsockname()

      name = {name,sock,peer}
    end
    handles[#handles + 1] = name
  end)


  collectgarbage()
  collectgarbage()
  local memoryUsed = 1024 * collectgarbage("count")

  -- Count file descriptors
  local path = "/proc/self/fd"
  if not exists(path) then
    path = "/dev/fd"
  end
  local entries = 0
  local iter = scandir(path)
  for _ in iter do
    entries = entries + 1
  end

  res.code = 200
  res.headers["Content-Type"] = "application/json"
  res.body = jsonStringify{
    handles = handles,
    lua_heap = memoryUsed,
    fd_count = entries,
  }
end
