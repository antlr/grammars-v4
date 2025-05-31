local spawn = require('childprocess').spawn
local path = require('path')
local childPath = path.join(__dirname, 'child-process-persistent.lua')

local child = spawn(process.execPath, { childPath }, {
  detached = true,
  stdio = 'ignore'
})

print(child.pid)

child:unref()
