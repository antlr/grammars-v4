local fs = require('fs')
local Buffer = require('buffer').Buffer
local Transform = require('stream').Transform

local JoystickTransform = Transform:extend()

function JoystickTransform:initialize()
  Transform.initialize(self, {objectMode = true})
end

-- https://www.kernel.org/doc/Documentation/input/joystick-api.txt
function JoystickTransform:_transform(chunk, _, callback)
  local buffer = Buffer:new(chunk)
  local event = {
    time   = buffer:readUInt32LE(1),
    number = buffer:readUInt8(8),
    value  = buffer:readUInt16LE(5),
  }
  local type = buffer:readUInt8(7)
  if bit.band(type, 0x80) > 0 then event.init = true end
  if bit.band(type, 0x01) > 0 then event.type = "button" end
  if bit.band(type, 0x02) > 0 then event.type = "axis" end
  self:push(event)
  callback()
end

--------------------------------------------------------------------------------

-- Sample usage
fs.createReadStream("/dev/input/js0", { chunkSize = 8 })
  :pipe(JoystickTransform:new())
  :on("data", p)
