-- This is a re-implementation of core.iStream, but using the new stream API and continuables

local Emitter = require('core').Emitter

local core = {}

local iStream = Emitter:extend()
core.iStream = iStream

function iStream:pipe(dest)
  return function (callback)
    local consume, onRead
    consume = function (err)
      if err then return callback(err) end
      self:read()(onRead)
    end
    onRead = function (err, chunk)
      if err then return callback(err) end
      return dest:write(chunk)(chunk and consume or callback)
    end
    consume()
  end
end

return core
