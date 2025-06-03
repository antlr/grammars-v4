local fs = require('file')
local fiber = require('wait')

local input = module.path
local output = module.path .. ".out"
local output2 = module.path .. ".out2"

-- Create a normal continuable using callbacks
local normal = function (callback)
  fs.open(input, "r")(function (err, fd)
    if err then return callback(err) end
    local readable = fs.ReadStream:new(fd)
    fs.open(output, "w")(function (err, fd)
      if err then return callback(err) end
      local writable = fs.WriteStream:new(fd)
      local consume, onRead, onDone
      consume = function (err)
        if err then return callback(err) end
        readable:read()(onRead)
      end
      onRead = function (err, chunk)
        if err then return callback(err) end
        return writable:write(chunk)(chunk and consume or onDone)
      end
      onDone = function (err)
        if err then return callback(err) end
        callback(nil, "done")
      end
      consume()
    end)
  end)
end

-- Create another that runs in a coroutine
local fibered = fiber.new(function (wait, await)
  local readable = fs.ReadStream:new(await(fs.open(input, "r")))
  local writable = fs.WriteStream:new(await(fs.open(output2, "w")))
  repeat
    local chunk = await(readable:read())
    await(writable:write(chunk))
  until not chunk
  return "done2"
end)

-- Run the normal one
normal(function (err, message)
  p{name="normal", err=err, message=message}
end)

-- Also run the fibered one in parallel
fibered(function (err, message)
  p{name="fibered", err=err, message=message}
end)
