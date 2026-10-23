-- This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
-- This file is based on Lua 5.x tests -- https://github.com/lua/lua/tree/master/testes
print('testing vararg')

local unpack = table.unpack

_G.arg = nil

function f(a, ...)
  local arg = { n=select('#',...), ... }
  if a then
    assert(#arg == #a)
  else
    assert(#arg == 0)
  end
  for i=1,#arg do assert(a[i]==arg[i]) end
  return arg.n
end

function c12 (...)
  local x = {...}; x.n = table.getn(x)
  local res = (x.n==2 and x[1] == 1 and x[2] == 2)
  if res then res = 55 end
  return res, 2
end

function vararg (...) return { n=select('#',...), ... } end

local call = function (f, args) return f(unpack(args, 1, args.n)) end

assert(f() == 0)
assert(f({1,2,3}, 1, 2, 3) == 3)
assert(f({"alo", nil, 45, f, nil}, "alo", nil, 45, f, nil) == 5)

assert(c12(1,2)==55)
a,b = assert(call(c12, {1,2}))
assert(a == 55 and b == 2)
a = call(c12, {1,2;n=2})
assert(a == 55 and b == 2)
a = call(c12, {1,2;n=1})
assert(not a)
assert(c12(1,2,3) == false)
local G = {foo=1,bar=2,foobar=3}
local a = vararg(call(next, {G,nil;n=2}))
local b,c = next(G)
assert(a[1] == b and a[2] == c and a.n == 2)
a = vararg(call(call, {c12, {1,2}}))
assert(a.n == 2 and a[1] == 55 and a[2] == 2)
a = call(print, {'+'})
assert(a == nil)

local t = {1, 10}
function t:f (...)
  local arg = { n=select('#',...), ... }
  return self[arg[1]]+arg.n
end
assert(t:f(1,4) == 3 and t:f(2) == 11)
print('+')

lim = 20
local i, a = 1, {}
while i <= lim do a[i] = i+0.3; i=i+1 end

function f(a, b, c, d, ...)
  local more = {...}
  assert(a == 1.3 and more[1] == 5.3 and
         more[lim-4] == lim+0.3 and not more[lim-3])
end

function g(a,b,c)
  assert(a == 1.3 and b == 2.3 and c == 3.3)
end

call(f, a)
call(g, a)

a = {}
i = 1
while i <= lim do a[i] = i; i=i+1 end
assert(call(math.max, a) == lim)

print("+")


-- new-style varargs

function oneless (a, ...) return ... end

function f (n, a, ...)
  local b
  assert(arg == nil)
  if n == 0 then
    local b, c, d = ...
    return a, b, c, d, oneless(oneless(oneless(...)))
  else
    n, b, a = n-1, ..., a
    assert(b == ...)
    return f(n, a, ...)
  end
end

a,b,c,d,e = assert(f(10,5,4,3,2,1))
assert(a==5 and b==4 and c==3 and d==2 and e==1)

a,b,c,d,e = f(4)
assert(a==nil and b==nil and c==nil and d==nil and e==nil)

-- select tests
a = {select(3, unpack{10,20,30,40})}
assert(table.getn(a) == 2 and a[1] == 30 and a[2] == 40)
a = {select(1)}
assert(next(a) == nil)
a = {select(-1, 3, 5, 7)}
assert(a[1] == 7 and a[2] == nil)
a = {select(-2, 3, 5, 7)}
assert(a[1] == 5 and a[2] == 7 and a[3] == nil)
pcall(select, 10000)
pcall(select, -10000)

-- select(_, ...) has special optimizations so it needs extra testing
function selectone(n, ...)
    local e = select(n, ...)
    return e
end

function selectmany(n, ...)
    return table.concat({select(n, ...)}, ',')
end

assert(selectone('#') == 0)
assert(selectmany('#') == "0")

assert(selectone('#', 10, 20, 30) == 3)
assert(selectmany('#', 10, 20, 30) == "3")

assert(selectone(1, 10, 20, 30) == 10)
assert(selectmany(1, 10, 20, 30) == "10,20,30")

assert(selectone(2, 10, 20, 30) == 20)
assert(selectmany(2, 10, 20, 30) == "20,30")

assert(selectone(3, 10, 20, 30) == 30)
assert(selectmany(3, 10, 20, 30) == "30")

assert(selectone(4, 10, 20, 30) == nil)
assert(selectmany(4, 10, 20, 30) == "")

assert(selectone(-2, 10, 20, 30) == 20)
assert(selectmany(-2, 10, 20, 30) == "20,30")

assert(selectone('3', 10, 20, 30) == 30)
assert(selectmany('3', 10, 20, 30) == "30")

-- varargs for main chunks
f = loadstring[[ return {...} ]]
x = f(2,3)
assert(x[1] == 2 and x[2] == 3 and x[3] == nil)


f = loadstring[[
  local x = {...}
  for i=1,select('#', ...) do assert(x[i] == select(i, ...)) end
  assert(x[select('#', ...)+1] == nil)
  return true
]]

assert(f("a", "b", nil, {}, assert))
assert(f())

return('OK')

