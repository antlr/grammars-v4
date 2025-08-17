--[[

Copyright 2014-2015 The Luvit Authors. All Rights Reserved.

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

local pp = require('pretty-print')
local colorize = pp.colorize

local exports = {}
exports.stream = pp.stdout
function exports.log(key, value, color)
  exports.stream:write(key .. ": " .. (color and colorize(color, value) or value) .. "\n")
end

return exports
