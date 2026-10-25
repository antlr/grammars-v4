--[[

Copyright 2014 The Luvit Authors. All Rights Reserved.

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

--[[
--Observable is a stream that can be observed outside the pipeline. observe()
--returns a new Readable stream that emits all data that passes through this
--stream. Streams created by observe() do not affect back-pressure.
--]]

local Transform = require('./stream_transform').Transform
local Readable = require('./stream_readable').Readable

local Observable = Transform:extend()

function Observable:initialize(options)
  --[[
  if (!(this instanceof PassThrough))
    return new PassThrough(options)
  --]]

  Transform.initialize(self, options)

  self.options = options
  self.observers = {}
end

function Observable:_transform(chunk, cb)
  for _,v in pairs(self.observers) do
    v:push(chunk)
  end
  cb(nil, chunk)
end

function Observable:observe()
  local obs = Readable:new(self.options)
  obs._read = function() end
  table.insert(self.observers, obs)
  return obs
end

return { Observable = Observable }
