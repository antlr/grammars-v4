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
// a passthrough stream.
// basically just the most minimal sort of Transform stream.
// Every written chunk gets output as-is.
--]]

local Transform = require('./stream_transform').Transform

local PassThrough = Transform:extend()

function PassThrough:initialize(options)
  --[[
 if (!(this instanceof PassThrough))
    return new PassThrough(options)
  --]]

  Transform.initialize(self, options)
end

function PassThrough:_transform(chunk, cb)
  cb(nil, chunk)
end

return { PassThrough = PassThrough }
