return function ()
  local core = require("core")()
  local prompt = require("prompt")(require("pretty-print"))
  local fs = require("coro-fs")
  local env = require("env")
  local log = require("log").log
  local pathJoin = require("luvi").path.join
  local cwd = require('uv').cwd()
  local sprintf = require("string").format

  local config = core.config

  local function getOutput()
    local output = prompt("Output to package.lua (1), or a init.lua (2)?")
    -- response was blank, run again
    if not output then
      return getOutput()
    end
    -- fail on any other options
    if not output:match('[1-2]') then
      log("Error", "You must select a valid option. [1 or 2]")
      return getOutput()
    else
      if output == "1" then
        output = "package.lua"
        log("Creating", "package.lua")
      elseif output == "2" then
        output = "init.lua"
        log("Creating", "init.lua")
      end
    end
    return output
  end

  local output = getOutput()

  local home = env.get("HOME") or (env.get("HOMEDRIVE") and env.get("HOMEPATH") and (env.get("HOMEDRIVE") .. env.get("HOMEPATH"))) or env.get("HOMEPATH") or ""
  local ini
  local function getConfig(name)
    ini = ini or fs.readFile(pathJoin(home, ".gitconfig"))
    if not ini then return end
    local section
    for line in ini:gmatch("[^\n\r]+") do
      local s = line:match("^%[([^%]]+)%]$")
      if s then
        section = s
      else
        local key, value = line:match("^%s*(%w+)%s*=%s*(.+)$")
        if key and section .. "." .. key == name then
          if tonumber(value) then return tonumber(value) end
          if value == "true" then return true end
          if value == "false" then return false end
          return value
        end
      end
    end
  end

  -- trim and wrap words in quotes
  function makeTags(csv)
    local tags = "{ "
    for word in csv:gmatch('([^,]+)') do
      tags = tags .. "\"" .. word:gsub("^%s*(.-)%s*$", "%1") .. "\", "
    end
    -- trim trailing comma and space
    tags = tags:sub(0, (#tags - 2))
    return tags .. " }"
  end

  local userName = prompt("Username", config["username"])
  local projectName = prompt("Project Name", userName .. "/project-name")
  local projectVersion = prompt("Version", "0.0.1")
  local projectDescription = prompt("Description", "A simple description of my little package.")
  local projectTags = makeTags(prompt("Tags (Comma Separated)", "lua, lit, luvit"))
  local authorName = prompt("Author Name", getConfig("user.name"))
  local authorEmail = prompt("Author Email", getConfig("user.email"))
  local projectLicense = prompt("License", "MIT")
  local projectHomepage = prompt("Homepage", "https://github.com/" .. projectName)

  local data = ""

  if output == "init.lua" then
    local init = [=[
  --[[lit-meta
    name = %q
    version = %q
    dependencies = {}
    description = %q
    tags = %s
    license = %q
    author = { name = %q, email = %q }
    homepage = %q
  ]]
  ]=]
    data = sprintf(init, projectName, projectVersion, projectDescription, projectTags, projectLicense, authorName, authorEmail, projectHomepage)
  elseif output == "package.lua" then
    local package = [[
  return {
    name = %q,
    version = %q,
    description = %q,
    tags = %s,
    license = %q,
    author = { name = %q, email = %q },
    homepage = %q,
    dependencies = {},
    files = {
      "**.lua",
      "!test*"
    }
  }
  ]]
    data = sprintf(package, projectName, projectVersion, projectDescription, projectTags, projectLicense, authorName, authorEmail, projectHomepage)
  end

  -- give us a preview
  print("\n" .. data .. "\n")

  local message = "Enter to continue"
  local finish = prompt("Is this ok?", message)

  if finish == message then
    local data, err = fs.writeFile(cwd .. "/" .. output, data)
    if err == nil then
      log("Complete", "Created a new " .. output .. " file.")
    else
      log("Error", "Could not write file.")
    end
  else
    log("Aborted", "No files will be written")
  end
end
