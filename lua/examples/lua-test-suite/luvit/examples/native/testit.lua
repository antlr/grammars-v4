-- Load our native module
local vector = require('./vector.so')

p(vector)

local v = vector.new(20, 10)

p({x=v.x,y=v.y,angle=v.angle})

