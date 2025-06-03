local fs = require('fs')

print("Reading file as stream")
local path = module.dir .. "/../README.markdown"
p('path:',path)
local stream = fs.createReadStream(path)

print("Adding data listener")
stream:on('data', function (chunk)
  p("on_data")
  print(chunk)
end)

print("Adding end listener")
stream:on('end', function ()
  p("on_end")
end)

print("Adding close listener")
stream:on('close', function ()
  p("on_close")
end)
