-- This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
print('testing function calls through API')

function add(a, b)
  return a + b
end

local m = { __eq = function(a, b) return a.a == b.a end }

function create_with_tm(x)
  return setmetatable({ a = x }, m)
end

local gen = 0
function incuv()
  gen += 1
  return gen
end

pi = 3.1415926
function getpi()
  return pi
end

return('OK')
