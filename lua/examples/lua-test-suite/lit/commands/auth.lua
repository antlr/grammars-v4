return function ()
  local core = require('core')()
  local prompt = require('prompt')(require('pretty-print'))
  local fs = require('coro-fs')
  local env = require('env')
  local log = require('log').log
  local pathJoin = require('luvi').path.join

  local config = core.config
  local dirty = false

  local function confirm(name, value)
    if not value then
      value = config[name]
    end
    if not value then
      value = prompt(name)
    else
      log(name, value)
    end
    if value ~= config[name] then
      config[name] = value
      dirty = true
    end
    return value
  end

  local home = env.get("HOME") or (env.get("HOMEDRIVE") and env.get("HOMEPATH") and (env.get("HOMEDRIVE") .. env.get("HOMEPATH"))) or env.get("HOMEPATH") or ""
  local ini
  local function getConfig(name)
    ini = ini or fs.readFile(pathJoin(home, ".gitconfig"))
    if not ini then return end
    local section
    for line in ini:gmatch("[^\n]+") do
      local s = line:match("^%[([^%]]+)%]$")
      if s then
        section = s
      else
        local key, value = line:match("^%s*(%w+)%s*=%s*(.+)$")
        if key and section .. '.' .. key == name then
          if tonumber(value) then return tonumber(value) end
          if value == "true" then return true end
          if value == "false" then return false end
          return value
        end
      end
    end
  end

  confirm("username", args[2])
  confirm("name", args[3] or getConfig("user.name"))
  confirm("email", args[4] or getConfig("user.email"))

  if not config.privateKey then
    local path = pathJoin(home, ".ssh", "id_rsa")
    if fs.access(path, "r") then
      config.privateKey = path
      dirty = true
    end
    confirm("privateKey")
  end

  core.authUser()

  if dirty then config.save() end
end
