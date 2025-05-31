local uv = require('uv')
local pathJoin = require('luvi').path.join
local getInstalled = require('get-installed')

return function ()
  local source = uv.cwd()
  if args[2] then
    source = pathJoin(source, args[2])
  end
  local fs
  fs, source = require('vfs')(source)

  local deps = getInstalled(fs, source)

  local list = {}
  for alias, meta in pairs(deps) do
    meta.alias = alias
    list[#list + 1] = meta
  end
  table.sort(list, function (a, b)
    return a.name < b.name
  end)

  local max1, max2 = 0, 0
  for i = 1, #list do
    local meta = list[i]
    if #meta.name > max1 then
      max1 = #meta.name
    end
    if #meta.version > max2 then
      max2 = #meta.version
    end
  end

  for i = 1, #list do
    local meta = list[i]
    local base = meta.name:match("([^/]+)$")
    local line = string.format("%-" .. max1 .. "s  v%-" .. max2 .. "s ", meta.name, meta.version)
    if meta.location == "libs" then
      line = line .. " (in libs)"
    end
    if base ~= meta.alias then
      line = line .. " (as " .. meta.alias .. ")"
    end
    print(line)
  end
end
