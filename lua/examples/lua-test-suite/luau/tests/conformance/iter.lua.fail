-- This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
-- This file is based on Lua 5.x tests -- https://github.com/lua/lua/tree/master/testes
print('testing iteration')

-- basic for loop tests
do
  local a
  for a,b in pairs{} do error("not here") end
  for i=1,0 do error("not here") end
  for i=0,1,-1 do error("not here") end
  a = nil; for i=1,1 do assert(not a); a=1 end; assert(a)
  a = nil; for i=1,1,-1 do assert(not a); a=1 end; assert(a)
  a = 0; for i=0, 1, 0.1 do a=a+1 end; assert(a==11)
end

-- precision tests for for loops
do
  local a
  --a = 0; for i=1, 0, -0.01 do a=a+1 end; assert(a==101)
  a = 0; for i=0, 0.999999999, 0.1 do a=a+1 end; assert(a==10)
  a = 0; for i=1, 1, 1 do a=a+1 end; assert(a==1)
  a = 0; for i=1e10, 1e10, -1 do a=a+1 end; assert(a==1)
  a = 0; for i=1, 0.99999, 1 do a=a+1 end; assert(a==0)
  a = 0; for i=99999, 1e5, -1 do a=a+1 end; assert(a==0)
  a = 0; for i=1, 0.99999, -1 do a=a+1 end; assert(a==1)
end

-- for loops do string->number coercion
do
  local a = 0; for i="10","1","-2" do a=a+1 end; assert(a==5)
end

-- generic for with function iterators
do
  local function f (n, p)
    local t = {}; for i=1,p do t[i] = i*10 end
    return function (_,n)
             if n > 0 then
               n = n-1
               return n, unpack(t)
             end
           end, nil, n
  end

  local x = 0
  for n,a,b,c,d in f(5,3) do
    x = x+1
    assert(a == 10 and b == 20 and c == 30 and d == nil)
  end
  assert(x == 5)
end

-- generic for with __call (tables)
do
  local f = {}
  setmetatable(f, { __call = function(_, _, n) if n > 0 then return n - 1 end end })

  local x = 0
  for n in f, nil, 5 do
    x += n
  end
  assert(x == 10)
end

-- generic for with __call (userdata)
do
  local f = newproxy(true)
  getmetatable(f).__call = function(_, _, n) if n > 0 then return n - 1 end end

  local x = 0
  for n in f, nil, 5 do
    x += n
  end
  assert(x == 10)
end

-- generic for with pairs
do
  local x = 0
  for k, v in pairs({a = 1, b = 2, c = 3}) do
    x += v
  end
  assert(x == 6)
end

-- generic for with pairs with holes
do
  local x = 0
  for k, v in pairs({1, 2, 3, nil, 5}) do
    x += v
  end
  assert(x == 11)
end

-- generic for with ipairs
do
  local x = 0
  for k, v in ipairs({1, 2, 3, nil, 5}) do
    x += v
  end
  assert(x == 6)
end

-- generic for with __iter (tables)
do
  local f = {}
  setmetatable(f, { __iter = function(x)
    assert(f == x)
    return next, {1, 2, 3, 4}
  end })

  local x = 0
  for n in f do
    x += n
  end
  assert(x == 10)
end

-- generic for with __iter (userdata)
do
  local f = newproxy(true)
  getmetatable(f).__iter = function(x)
    assert(f == x)
    return next, {1, 2, 3, 4}
  end

  local x = 0
  for n in f do
    x += n
  end
  assert(x == 10)
end

-- generic for with tables (dictionary)
do
  local x = 0
  for k, v in {a = 1, b = 2, c = 3} do
    print(k, v)
    x += v
  end
  assert(x == 6)
end

-- generic for with tables (arrays)
do
  local x = ''
  for k, v in {1, 2, 3, nil, 5} do
    x ..= tostring(v)
  end
  assert(x == "1235")
end

-- generic for with tables (mixed)
do
  local x = 0
  for k, v in {1, 2, 3, nil, 5, a = 1, b = 2, c = 3} do
    x += v
  end
  assert(x == 17)
end

-- generic for over a non-iterable object
do
  local ok, err = pcall(function() for x in 42 do end end)
  assert(not ok and err:match("attempt to iterate"))
end

-- generic for over an iterable object that doesn't return a function
do
  local obj = {}
  setmetatable(obj, { __iter = function() end })

  local ok, err = pcall(function() for x in obj do end end)
  assert(not ok and err:match("attempt to call a nil value"))
end

-- it's okay to iterate through a table with a single variable
do
  local x = 0
  for k in {1, 2, 3, 4, 5} do
    x += k
  end
  assert(x == 15)
end

-- all extra variables should be set to nil during builtin traversal
do
  local x = 0
  for k,v,a,b,c,d,e in {1, 2, 3, 4, 5} do
    x += k
    assert(a == nil and b == nil and c == nil and d == nil and e == nil)
  end
  assert(x == 15)
end

return"OK"
