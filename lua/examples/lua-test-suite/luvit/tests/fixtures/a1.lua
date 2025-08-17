--[[

Copyright 2012 The Luvit Authors. All Rights Reserved.

Licensed under the Apache License, Version 2.0 (the "License")
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS-IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

--]]

local c = require('./b/c')

print('load fixtures/a.lua')

local string = 'A'

exports.SomeClass = c.SomeClass

exports.A = function()
  return string
end

exports.C = function()
  return c.C()
end

exports.D = function()
  return c.D()
end

exports.number = 42

_G.onexit(function()
  string = 'A done'
end)
