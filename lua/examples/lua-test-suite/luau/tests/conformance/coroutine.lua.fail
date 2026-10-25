-- This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
-- This file is based on Lua 5.x tests -- https://github.com/lua/lua/tree/master/testes
print "testing coroutines"

local f

local main = coroutine.running()
assert(main == nil)
-- assert(not coroutine.resume(main))
assert(coroutine.isyieldable()) -- note: we run this in context of a yieldable thread like all other Lua code
--assert(not pcall(coroutine.yield))


-- trivial errors
assert(not pcall(coroutine.resume, 0))
assert(not pcall(coroutine.status, 0))


-- tests for multiple yield/resume arguments

local function eqtab (t1, t2)
  assert(#t1 == #t2)
  for i = 1, #t1 do
    local v = t1[i]
    assert(t2[i] == v)
  end
end

_G.x = nil   -- declare x
function foo (a, ...)
  local x = coroutine.running()
  assert(x == f)
  -- next call should not corrupt coroutine (but must fail,
  -- as it attempts to resume the running coroutine)
  assert(coroutine.resume(f) == false)
  assert(coroutine.status(f) == "running")
  local arg = {...}
  assert(coroutine.isyieldable())
  for i=1,#arg do
    _G.x = {coroutine.yield(table.unpack(arg[i]))}
  end
  return table.unpack(a)
end

f = coroutine.create(foo)
assert(type(f) == "thread" and coroutine.status(f) == "suspended")
assert(string.find(tostring(f), "thread"))
local s,a,b,c,d
s,a,b,c,d = coroutine.resume(f, {1,2,3}, {}, {1}, {'a', 'b', 'c'})
assert(s and a == nil and coroutine.status(f) == "suspended")
s,a,b,c,d = coroutine.resume(f)
eqtab(_G.x, {})
assert(s and a == 1 and b == nil)
s,a,b,c,d = coroutine.resume(f, 1, 2, 3)
eqtab(_G.x, {1, 2, 3})
assert(s and a == 'a' and b == 'b' and c == 'c' and d == nil)
s,a,b,c,d = coroutine.resume(f, "xuxu")
eqtab(_G.x, {"xuxu"})
assert(s and a == 1 and b == 2 and c == 3 and d == nil)
assert(coroutine.status(f) == "dead")
s, a = coroutine.resume(f, "xuxu")
assert(not s and string.find(a, "dead") and coroutine.status(f) == "dead")


-- yields in tail calls
local function foo (i) return coroutine.yield(i) end
f = coroutine.wrap(function ()
  for i=1,10 do
    assert(foo(i) == _G.x)
  end
  return 'a'
end)
for i=1,10 do _G.x = i; assert(f(i) == i) end
_G.x = 'xuxu'; assert(f('xuxu') == 'a')

-- recursive
function pf (n, i)
  coroutine.yield(n)
  pf(n*i, i+1)
end

f = coroutine.wrap(pf)
local s=1
for i=1,10 do
  assert(f(1, 1) == s)
  s = s*i
end

-- sieve
function gen (n)
  return coroutine.wrap(function ()
    for i=2,n do coroutine.yield(i) end
  end)
end


function filter (p, g)
  return coroutine.wrap(function ()
    while 1 do
      local n = g()
      if n == nil then return end
      if math.fmod(n, p) ~= 0 then coroutine.yield(n) end
    end
  end)
end

local x = gen(80)
local a = {}
while 1 do
  local n = x()
  if n == nil then break end
  table.insert(a, n)
  x = filter(n, x)
end

assert(#a == 22 and a[#a] == 79)
x, a = nil


-- yielding across C boundaries

co = coroutine.wrap(function()
       assert(not pcall(table.sort,{1,2,3}, coroutine.yield))
       assert(coroutine.isyieldable())
       coroutine.yield(20)
       return 30
     end)

assert(co() == 20)
assert(co() == 30)

-- unyieldable C call
do
  local function f (c)
          assert(not coroutine.isyieldable())
          return c .. c
        end

  local co = coroutine.wrap(function (c)
               assert(coroutine.isyieldable())
               local s = string.gsub("a", ".", f)
               return s
             end)
  assert(co() == "aa")
end



-- errors in coroutines
function foo ()
  coroutine.yield(3)
  error(foo)
end

function goo() foo() end
x = coroutine.wrap(goo)
assert(x() == 3)
local a,b = pcall(x)
assert(not a and b == foo)

x = coroutine.create(goo)
a,b = coroutine.resume(x)
assert(a and b == 3)
a,b = coroutine.resume(x)
assert(not a and b == foo and coroutine.status(x) == "dead")
a,b = coroutine.resume(x)
assert(not a and string.find(b, "dead") and coroutine.status(x) == "dead")


-- co-routines x for loop
function all (a, n, k)
  if k == 0 then coroutine.yield(a)
  else
    for i=1,n do
      a[k] = i
      all(a, n, k-1)
    end
  end
end

local a = 0
for t in coroutine.wrap(function () all({}, 5, 4) end) do
  a = a+1
end
assert(a == 5^4)


-- access to locals of collected coroutines
local C = {}; setmetatable(C, {__mode = "kv"})
local x = coroutine.wrap (function ()
            local a = 10
            local function f () a = a+10; return a end
            while true do
              a = a+1
              coroutine.yield(f)
            end
          end)

C[1] = x;

local f = x()
assert(f() == 21 and x()() == 32 and x() == f)
x = nil
collectgarbage()
assert(C[1] == undef)
assert(f() == 43 and f() == 53)


-- old bug: attempt to resume itself

function co_func (current_co)
  assert(coroutine.running() == current_co)
  assert(coroutine.resume(current_co) == false)
  coroutine.yield(10, 20)
  assert(coroutine.resume(current_co) == false)
  coroutine.yield(23)
  return 10
end

local co = coroutine.create(co_func)
local a,b,c = coroutine.resume(co, co)
assert(a == true and b == 10 and c == 20)
a,b = coroutine.resume(co, co)
assert(a == true and b == 23)
a,b = coroutine.resume(co, co)
assert(a == true and b == 10)
assert(coroutine.resume(co, co) == false)
assert(coroutine.resume(co, co) == false)


-- attempt to resume 'normal' coroutine
local co1, co2
co1 = coroutine.create(function () return co2() end)
co2 = coroutine.wrap(function ()
        assert(coroutine.status(co1) == 'normal')
        assert(not coroutine.resume(co1))
        coroutine.yield(3)
      end)

a,b = coroutine.resume(co1)
assert(a and b == 3)
assert(coroutine.status(co1) == 'dead')

if not limitedstack then
  -- infinite recursion of coroutines
  a = function(a) coroutine.wrap(a)(a) end
  assert(not pcall(a, a))
  a = nil
end

-- access to locals of erroneous coroutines
local x = coroutine.create (function ()
            local a = 10
            _G.f = function () a=a+1; return a end
            error('x')
          end)

assert(not coroutine.resume(x))
-- overwrite previous position of local `a'
assert(not coroutine.resume(x, 1, 1, 1, 1, 1, 1, 1))
assert(_G.f() == 11)
assert(_G.f() == 12)

-- leaving a pending coroutine open
_X = coroutine.wrap(function ()
      local a = 10
      local x = function () a = a+1 end
      coroutine.yield()
    end)

_X()


if not limitedstack then
  -- bug (stack overflow)
  local j = 2^9
  local lim = 1000000    -- (C stack limit; assume 32-bit machine)
  local t = {lim - 10, lim - 5, lim - 1, lim, lim + 1}
  for i = 1, #t do
    local j = t[i]
    co = coroutine.create(function()
           local t = {}
           for i = 1, j do t[i] = i end
           return table.unpack(t)
         end)
    local r, msg = coroutine.resume(co)
    assert(not r)
  end
  co = nil
end


assert(coroutine.running() == main)

-- bug in nCcalls
local co = coroutine.wrap(function ()
  local a = {pcall(pcall,pcall,pcall,pcall,pcall,pcall,pcall,error,"hi")}
  return pcall(assert, table.unpack(a))
end)

local a = {co()}
assert(a[10] == "hi")

-- test coroutine with C functions
local co = coroutine.create(coroutine.yield)
assert(coroutine.status(co) == "suspended")
coroutine.resume(co)
assert(coroutine.status(co) == "suspended")
coroutine.resume(co)
assert(coroutine.status(co) == "dead")

-- test correct handling of coroutine.yield returns for 0-30 values
for i=0,30 do
  local T = table.create(i, 42)
  local co = coroutine.create(function() coroutine.yield(table.unpack(T)) end)
  local T2 = table.pack(coroutine.resume(co))
  assert(T2[1] == true)
  assert(1 + #T == #T2)
  assert(#T2 == 1 or T2[#T2] == 42)
end

-- test coroutine.close
do
  -- ok to close a dead coroutine
  local co = coroutine.create(type)
  assert(coroutine.resume(co, "testing 'coroutine.close'"))
  assert(coroutine.status(co) == "dead")
  local st, msg = coroutine.close(co)
  assert(st and msg == nil)
  -- also ok to close it again
  st, msg = coroutine.close(co)
  assert(st and msg == nil)


  -- cannot close the running coroutine
  coroutine.wrap(function()
    local st, msg = pcall(coroutine.close, coroutine.running())
    assert(not st and string.find(msg, "running"))
  end)()

  -- cannot close a "normal" coroutine
  coroutine.wrap(function()
    local co = coroutine.running()
    coroutine.wrap(function ()
      local st, msg = pcall(coroutine.close, co)
      assert(not st and string.find(msg, "normal"))
    end)()
  end)()

  -- closing a coroutine after an error
  local co = coroutine.create(error)
  local obj = {42}
  local st, msg = coroutine.resume(co, obj)
  assert(not st and msg == obj)
  st, msg = coroutine.close(co)
  assert(not st and msg == obj)
  -- after closing, no more errors
  st, msg = coroutine.close(co)
  assert(st and msg == nil)

  -- closing a coroutine that has outstanding upvalues
  local f
  local co = coroutine.create(function()
    local a = 42
    f = function() return a end
    coroutine.yield()
    a = 20
  end)
  coroutine.resume(co)
  assert(f() == 42)
  st, msg = coroutine.close(co)
  assert(st and msg == nil)
  assert(f() == 42)

  -- closing a coroutine with a large stack
  co = coroutine.create(function()
    local function f(depth) return if depth > 0 then f(depth - 1) + depth else 0 end
    coroutine.yield(f(100))
  end)
  assert(coroutine.resume(co))
  st, msg = coroutine.close(co)
  assert(st and msg == nil)
end

return 'OK'
