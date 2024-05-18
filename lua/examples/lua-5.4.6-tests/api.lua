-- $Id: testes/api.lua $
-- See Copyright Notice in file all.lua

if T==nil then
  (Message or print)('\n >>> testC not active: skipping API tests <<<\n')
  return
end

local debug = require "debug"

local pack = table.pack


-- standard error message for memory errors
local MEMERRMSG = "not enough memory"

local function tcheck (t1, t2)
  assert(t1.n == (t2.n or #t2) + 1)
  for i = 2, t1.n do assert(t1[i] == t2[i - 1]) end
end


local function checkerr (msg, f, ...)
  local stat, err = pcall(f, ...)
  assert(not stat and string.find(err, msg))
end


print('testing C API')

local a = T.testC("pushvalue R; return 1")
assert(a == debug.getregistry())


-- absindex
assert(T.testC("settop 10; absindex -1; return 1") == 10)
assert(T.testC("settop 5; absindex -5; return 1") == 1)
assert(T.testC("settop 10; absindex 1; return 1") == 1)
assert(T.testC("settop 10; absindex R; return 1") < -10)

-- testing alignment
a = T.d2s(12458954321123.0)
assert(a == string.pack("d", 12458954321123.0))
assert(T.s2d(a) == 12458954321123.0)

local a,b,c = T.testC("pushnum 1; pushnum 2; pushnum 3; return 2")
assert(a == 2 and b == 3 and not c)

local f = T.makeCfunc("pushnum 1; pushnum 2; pushnum 3; return 2")
a,b,c = f()
assert(a == 2 and b == 3 and not c)

-- test that all trues are equal
a,b,c = T.testC("pushbool 1; pushbool 2; pushbool 0; return 3")
assert(a == b and a == true and c == false)
a,b,c = T.testC"pushbool 0; pushbool 10; pushnil;\
                      tobool -3; tobool -3; tobool -3; return 3"
assert(a==false and b==true and c==false)


a,b,c = T.testC("gettop; return 2", 10, 20, 30, 40)
assert(a == 40 and b == 5 and not c)

local t = pack(T.testC("settop 5; return *", 2, 3))
tcheck(t, {n=4,2,3})

t = pack(T.testC("settop 0; settop 15; return 10", 3, 1, 23))
assert(t.n == 10 and t[1] == nil and t[10] == nil)

t = pack(T.testC("remove -2; return *", 2, 3, 4))
tcheck(t, {n=2,2,4})

t = pack(T.testC("insert -1; return *", 2, 3))
tcheck(t, {n=2,2,3})

t = pack(T.testC("insert 3; return *", 2, 3, 4, 5))
tcheck(t, {n=4,2,5,3,4})

t = pack(T.testC("replace 2; return *", 2, 3, 4, 5))
tcheck(t, {n=3,5,3,4})

t = pack(T.testC("replace -2; return *", 2, 3, 4, 5))
tcheck(t, {n=3,2,3,5})

t = pack(T.testC("remove 3; return *", 2, 3, 4, 5))
tcheck(t, {n=3,2,4,5})

t = pack(T.testC("copy 3 4; return *", 2, 3, 4, 5))
tcheck(t, {n=4,2,3,3,5})

t = pack(T.testC("copy -3 -1; return *", 2, 3, 4, 5))
tcheck(t, {n=4,2,3,4,3})

do   -- testing 'rotate'
  local t = {10, 20, 30, 40, 50, 60}
  for i = -6, 6 do
    local s = string.format("rotate 2 %d; return 7", i)
    local t1 = pack(T.testC(s, 10, 20, 30, 40, 50, 60))
    tcheck(t1, t)
    table.insert(t, 1, table.remove(t))
  end

  t = pack(T.testC("rotate -2 1; return *", 10, 20, 30, 40))
  tcheck(t, {10, 20, 40, 30})
  t = pack(T.testC("rotate -2 -1; return *", 10, 20, 30, 40))
  tcheck(t, {10, 20, 40, 30})

  -- some corner cases
  t = pack(T.testC("rotate -1 0; return *", 10, 20, 30, 40))
  tcheck(t, {10, 20, 30, 40})
  t = pack(T.testC("rotate -1 1; return *", 10, 20, 30, 40))
  tcheck(t, {10, 20, 30, 40})
  t = pack(T.testC("rotate 5 -1; return *", 10, 20, 30, 40))
  tcheck(t, {10, 20, 30, 40})
end


-- testing warnings
T.testC([[
  warningC "#This shold be a"
  warningC " single "
  warning "warning"
  warningC "#This should be "
  warning "another one"
]])


-- testing message handlers
do
  local f = T.makeCfunc[[
    getglobal error
    pushstring bola
    pcall 1 1 1   # call 'error' with given handler
    pushstatus
    return 2     # return error message and status
  ]]

  local msg, st = f(string.upper)   -- function handler
  assert(st == "ERRRUN" and msg == "BOLA")
  local msg, st = f(string.len)     -- function handler
  assert(st == "ERRRUN" and msg == 4)

end

t = pack(T.testC("insert 3; pushvalue 3; remove 3; pushvalue 2; remove 2; \
                  insert 2; pushvalue 1; remove 1; insert 1; \
      insert -2; pushvalue -2; remove -3; return *",
      2, 3, 4, 5, 10, 40, 90))
tcheck(t, {n=7,2,3,4,5,10,40,90})

t = pack(T.testC("concat 5; return *", "alo", 2, 3, "joao", 12))
tcheck(t, {n=1,"alo23joao12"})

-- testing MULTRET
t = pack(T.testC("call 2,-1; return *",
     function (a,b) return 1,2,3,4,a,b end, "alo", "joao"))
tcheck(t, {n=6,1,2,3,4,"alo", "joao"})

do  -- test returning more results than fit in the caller stack
  local a = {}
  for i=1,1000 do a[i] = true end; a[999] = 10
  local b = T.testC([[pcall 1 -1 0; pop 1; tostring -1; return 1]],
                    table.unpack, a)
  assert(b == "10")
end


-- testing globals
_G.AA = 14; _G.BB = "a31"
local a = {T.testC[[
  getglobal AA;
  getglobal BB;
  getglobal BB;
  setglobal AA;
  return *
]]}
assert(a[2] == 14 and a[3] == "a31" and a[4] == nil and _G.AA == "a31")

_G.AA, _G.BB = nil

-- testing arith
assert(T.testC("pushnum 10; pushnum 20; arith /; return 1") == 0.5)
assert(T.testC("pushnum 10; pushnum 20; arith -; return 1") == -10)
assert(T.testC("pushnum 10; pushnum -20; arith *; return 1") == -200)
assert(T.testC("pushnum 10; pushnum 3; arith ^; return 1") == 1000)
assert(T.testC("pushnum 10; pushstring 20; arith /; return 1") == 0.5)
assert(T.testC("pushstring 10; pushnum 20; arith -; return 1") == -10)
assert(T.testC("pushstring 10; pushstring -20; arith *; return 1") == -200)
assert(T.testC("pushstring 10; pushstring 3; arith ^; return 1") == 1000)
assert(T.testC("arith /; return 1", 2, 0) == 10.0/0)
a = T.testC("pushnum 10; pushint 3; arith \\; return 1")
assert(a == 3.0 and math.type(a) == "float")
a = T.testC("pushint 10; pushint 3; arith \\; return 1")
assert(a == 3 and math.type(a) == "integer")
a = assert(T.testC("pushint 10; pushint 3; arith +; return 1"))
assert(a == 13 and math.type(a) == "integer")
a = assert(T.testC("pushnum 10; pushint 3; arith +; return 1"))
assert(a == 13 and math.type(a) == "float")
a,b,c = T.testC([[pushnum 1;
                  pushstring 10; arith _;
                  pushstring 5; return 3]])
assert(a == 1 and b == -10 and c == "5")
local mt = {
      __add = function (a,b) return setmetatable({a[1] + b[1]}, mt) end,
      __mod = function (a,b) return setmetatable({a[1] % b[1]}, mt) end,
      __unm = function (a) return setmetatable({a[1]* 2}, mt) end}
a,b,c = setmetatable({4}, mt),
        setmetatable({8}, mt),
        setmetatable({-3}, mt)
local x,y,z = T.testC("arith +; return 2", 10, a, b)
assert(x == 10 and y[1] == 12 and z == nil)
assert(T.testC("arith %; return 1", a, c)[1] == 4%-3)
assert(T.testC("arith _; arith +; arith %; return 1", b, a, c)[1] ==
               8 % (4 + (-3)*2))

-- errors in arithmetic
checkerr("divide by zero", T.testC, "arith \\", 10, 0)
checkerr("%%0", T.testC, "arith %", 10, 0)


-- testing lessthan and lessequal
assert(T.testC("compare LT 2 5, return 1", 3, 2, 2, 4, 2, 2))
assert(T.testC("compare LE 2 5, return 1", 3, 2, 2, 4, 2, 2))
assert(not T.testC("compare LT 3 4, return 1", 3, 2, 2, 4, 2, 2))
assert(T.testC("compare LE 3 4, return 1", 3, 2, 2, 4, 2, 2))
assert(T.testC("compare LT 5 2, return 1", 4, 2, 2, 3, 2, 2))
assert(not T.testC("compare LT 2 -3, return 1", "4", "2", "2", "3", "2", "2"))
assert(not T.testC("compare LT -3 2, return 1", "3", "2", "2", "4", "2", "2"))

-- non-valid indices produce false
assert(not T.testC("compare LT 1 4, return 1"))
assert(not T.testC("compare LE 9 1, return 1"))
assert(not T.testC("compare EQ 9 9, return 1"))

local b = {__lt = function (a,b) return a[1] < b[1] end}
local a1,a3,a4 = setmetatable({1}, b),
                 setmetatable({3}, b),
                 setmetatable({4}, b)
assert(T.testC("compare LT 2 5, return 1", a3, 2, 2, a4, 2, 2))
assert(T.testC("compare LE 2 5, return 1", a3, 2, 2, a4, 2, 2))
assert(T.testC("compare LT 5 -6, return 1", a4, 2, 2, a3, 2, 2))
a,b = T.testC("compare LT 5 -6, return 2", a1, 2, 2, a3, 2, 20)
assert(a == 20 and b == false)
a,b = T.testC("compare LE 5 -6, return 2", a1, 2, 2, a3, 2, 20)
assert(a == 20 and b == false)
a,b = T.testC("compare LE 5 -6, return 2", a1, 2, 2, a1, 2, 20)
assert(a == 20 and b == true)


do  -- testing lessthan and lessequal with metamethods
  local mt = {__lt = function (a,b) return a[1] < b[1] end,
              __le = function (a,b) return a[1] <= b[1] end,
              __eq = function (a,b) return a[1] == b[1] end}
  local function O (x)
    return setmetatable({x}, mt)
  end

  local a, b = T.testC("compare LT 2 3; pushint 10; return 2", O(1), O(2))
  assert(a == true and b == 10)
  local a, b = T.testC("compare LE 2 3; pushint 10; return 2", O(3), O(2))
  assert(a == false and b == 10)
  local a, b = T.testC("compare EQ 2 3; pushint 10; return 2", O(3), O(3))
  assert(a == true and b == 10)
end

-- testing length
local t = setmetatable({x = 20}, {__len = function (t) return t.x end})
a,b,c = T.testC([[
   len 2;
   Llen 2;
   objsize 2;
   return 3
]], t)
assert(a == 20 and b == 20 and c == 0)

t.x = "234"; t[1] = 20
a,b,c = T.testC([[
   len 2;
   Llen 2;
   objsize 2;
   return 3
]], t)
assert(a == "234" and b == 234 and c == 1)

t.x = print; t[1] = 20
a,c = T.testC([[
   len 2;
   objsize 2;
   return 2
]], t)
assert(a == print and c == 1)


-- testing __concat

a = setmetatable({x="u"}, {__concat = function (a,b) return a.x..'.'..b.x end})
x,y = T.testC([[
  pushnum 5
  pushvalue 2;
  pushvalue 2;
  concat 2;
  pushvalue -2;
  return 2;
]], a, a)
assert(x == a..a and y == 5)

-- concat with 0 elements
assert(T.testC("concat 0; return 1") == "")

-- concat with 1 element
assert(T.testC("concat 1; return 1", "xuxu") == "xuxu")



-- testing lua_is

local function B (x) return x and 1 or 0 end

local function count (x, n)
  n = n or 2
  local prog = [[
    isnumber %d;
    isstring %d;
    isfunction %d;
    iscfunction %d;
    istable %d;
    isuserdata %d;
    isnil %d;
    isnull %d;
    return 8
  ]]
  prog = string.format(prog, n, n, n, n, n, n, n, n)
  local a,b,c,d,e,f,g,h = T.testC(prog, x)
  return B(a)+B(b)+B(c)+B(d)+B(e)+B(f)+B(g)+(100*B(h))
end

assert(count(3) == 2)
assert(count('alo') == 1)
assert(count('32') == 2)
assert(count({}) == 1)
assert(count(print) == 2)
assert(count(function () end) == 1)
assert(count(nil) == 1)
assert(count(io.stdin) == 1)
assert(count(nil, 15) == 100)


-- testing lua_to...

local function to (s, x, n)
  n = n or 2
  return T.testC(string.format("%s %d; return 1", s, n), x)
end

local null = T.pushuserdata(0)
local hfunc = string.gmatch("", "")    -- a "heavy C function" (with upvalues)
assert(debug.getupvalue(hfunc, 1))
assert(to("tostring", {}) == nil)
assert(to("tostring", "alo") == "alo")
assert(to("tostring", 12) == "12")
assert(to("tostring", 12, 3) == nil)
assert(to("objsize", {}) == 0)
assert(to("objsize", {1,2,3}) == 3)
assert(to("objsize", "alo\0\0a") == 6)
assert(to("objsize", T.newuserdata(0)) == 0)
assert(to("objsize", T.newuserdata(101)) == 101)
assert(to("objsize", 124) == 0)
assert(to("objsize", true) == 0)
assert(to("tonumber", {}) == 0)
assert(to("tonumber", "12") == 12)
assert(to("tonumber", "s2") == 0)
assert(to("tonumber", 1, 20) == 0)
assert(to("topointer", 10) == null)
assert(to("topointer", true) == null)
assert(to("topointer", nil) == null)
assert(to("topointer", "abc") ~= null)
assert(to("topointer", string.rep("x", 10)) ==
       to("topointer", string.rep("x", 10)))    -- short strings
do    -- long strings
  local s1 = string.rep("x", 300)
  local s2 = string.rep("x", 300)
  assert(to("topointer", s1) ~= to("topointer", s2))
end
assert(to("topointer", T.pushuserdata(20)) ~= null)
assert(to("topointer", io.read) ~= null)           -- light C function
assert(to("topointer", hfunc) ~= null)        -- "heavy" C function
assert(to("topointer", function () end) ~= null)   -- Lua function
assert(to("topointer", io.stdin) ~= null)   -- full userdata
assert(to("func2num", 20) == 0)
assert(to("func2num", T.pushuserdata(10)) == 0)
assert(to("func2num", io.read) ~= 0)     -- light C function
assert(to("func2num", hfunc) ~= 0)  -- "heavy" C function (with upvalue)
a = to("tocfunction", math.deg)
assert(a(3) == math.deg(3) and a == math.deg)


print("testing panic function")
do
  -- trivial error
  assert(T.checkpanic("pushstring hi; error") == "hi")

  -- using the stack inside panic
  assert(T.checkpanic("pushstring hi; error;",
    [[checkstack 5 XX
      pushstring ' alo'
      pushstring ' mundo'
      concat 3]]) == "hi alo mundo")

  -- "argerror" without frames
  assert(T.checkpanic("loadstring 4") ==
      "bad argument #4 (string expected, got no value)")


  -- memory error
  T.totalmem(T.totalmem()+10000)   -- set low memory limit (+10k)
  assert(T.checkpanic("newuserdata 20000") == MEMERRMSG)
  T.totalmem(0)          -- restore high limit

  -- stack error
  if not _soft then
    local msg = T.checkpanic[[
      pushstring "function f() f() end"
      loadstring -1; call 0 0
      getglobal f; call 0 0
    ]]
    assert(string.find(msg, "stack overflow"))
  end

  -- exit in panic still close to-be-closed variables
  assert(T.checkpanic([[
    pushstring "return {__close = function () Y = 'ho'; end}"
    newtable
    loadstring -2
    call 0 1
    setmetatable -2
    toclose -1
    pushstring "hi"
    error
  ]],
  [[
    getglobal Y
    concat 2         # concat original error with global Y
  ]]) == "hiho")


end

-- testing deep C stack
if not _soft then
  print("testing stack overflow")
  collectgarbage("stop")
  checkerr("XXXX", T.testC, "checkstack 1000023 XXXX")   -- too deep
  -- too deep (with no message)
  checkerr("^stack overflow$", T.testC, "checkstack 1000023 ''")
  local s = string.rep("pushnil;checkstack 1 XX;", 1000000)
  checkerr("overflow", T.testC, s)
  collectgarbage("restart")
  print'+'
end

local lim = _soft and 500 or 12000
local prog = {"checkstack " .. (lim * 2 + 100) .. "msg", "newtable"}
for i = 1,lim do
  prog[#prog + 1] = "pushnum " .. i
  prog[#prog + 1] = "pushnum " .. i * 10
end

prog[#prog + 1] = "rawgeti R 2"   -- get global table in registry
prog[#prog + 1] = "insert " .. -(2*lim + 2)

for i = 1,lim do
  prog[#prog + 1] = "settable " .. -(2*(lim - i + 1) + 1)
end

prog[#prog + 1] = "return 2"

prog = table.concat(prog, ";")
local g, t = T.testC(prog)
assert(g == _G)
for i = 1,lim do assert(t[i] == i*10); t[i] = undef end
assert(next(t) == nil)
prog, g, t = nil

-- testing errors

a = T.testC([[
  loadstring 2; pcall 0 1 0;
  pushvalue 3; insert -2; pcall 1 1 0;
  pcall 0 0 0;
  return 1
]], "XX=150", function (a) assert(a==nil); return 3 end)

assert(type(a) == 'string' and XX == 150)
_G.XX = nil

local function check3(p, ...)
  local arg = {...}
  assert(#arg == 3)
  assert(string.find(arg[3], p))
end
check3(":1:", T.testC("loadstring 2; return *", "x="))
check3("%.", T.testC("loadfile 2; return *", "."))
check3("xxxx", T.testC("loadfile 2; return *", "xxxx"))

-- test errors in non protected threads
local function checkerrnopro (code, msg)
  local th = coroutine.create(function () end)  -- create new thread
  local stt, err = pcall(T.testC, th, code)   -- run code there
  assert(not stt and string.find(err, msg))
end

if not _soft then
  collectgarbage("stop")   -- avoid __gc with full stack
  checkerrnopro("pushnum 3; call 0 0", "attempt to call")
  print"testing stack overflow in unprotected thread"
  function F () F() end
  checkerrnopro("getglobal 'F'; call 0 0;", "stack overflow")
  F = nil
  collectgarbage("restart")
end
print"+"


-- testing table access

do   -- getp/setp
  local a = {}
  local a1 = T.testC("rawsetp 2 1; return 1", a, 20)
  assert(a == a1)
  assert(a[T.pushuserdata(1)] == 20)
  local a1, res = T.testC("rawgetp -1 1; return 2", a)
  assert(a == a1 and res == 20)
end


do  -- using the table itself as index
  local a = {}
  a[a] = 10
  local prog = "gettable -1; return *"
  local res = {T.testC(prog, a)}
  assert(#res == 2 and res[1] == prog and res[2] == 10)

  local prog = "settable -2; return *"
  local res = {T.testC(prog, a, 20)}
  assert(a[a] == 20)
  assert(#res == 1 and res[1] == prog)

  -- raw
  a[a] = 10
  local prog = "rawget -1; return *"
  local res = {T.testC(prog, a)}
  assert(#res == 2 and res[1] == prog and res[2] == 10)

  local prog = "rawset -2; return *"
  local res = {T.testC(prog, a, 20)}
  assert(a[a] == 20)
  assert(#res == 1 and res[1] == prog)

  -- using the table as the value to set
  local prog = "rawset -1; return *"
  local res = {T.testC(prog, 30, a)}
  assert(a[30] == a)
  assert(#res == 1 and res[1] == prog)

  local prog = "settable -1; return *"
  local res = {T.testC(prog, 40, a)}
  assert(a[40] == a)
  assert(#res == 1 and res[1] == prog)

  local prog = "rawseti -1 100; return *"
  local res = {T.testC(prog, a)}
  assert(a[100] == a)
  assert(#res == 1 and res[1] == prog)

  local prog = "seti -1 200; return *"
  local res = {T.testC(prog, a)}
  assert(a[200] == a)
  assert(#res == 1 and res[1] == prog)
end

a = {x=0, y=12}
x, y = T.testC("gettable 2; pushvalue 4; gettable 2; return 2",
                a, 3, "y", 4, "x")
assert(x == 0 and y == 12)
T.testC("settable -5", a, 3, 4, "x", 15)
assert(a.x == 15)
a[a] = print
x = T.testC("gettable 2; return 1", a)  -- table and key are the same object!
assert(x == print)
T.testC("settable 2", a, "x")    -- table and key are the same object!
assert(a[a] == "x")

b = setmetatable({p = a}, {})
getmetatable(b).__index = function (t, i) return t.p[i] end
local k, x = T.testC("gettable 3, return 2", 4, b, 20, 35, "x")
assert(x == 15 and k == 35)
k = T.testC("getfield 2 y, return 1", b)
assert(k == 12)
getmetatable(b).__index = function (t, i) return a[i] end
getmetatable(b).__newindex = function (t, i,v ) a[i] = v end
y = T.testC("insert 2; gettable -5; return 1", 2, 3, 4, "y", b)
assert(y == 12)
k = T.testC("settable -5, return 1", b, 3, 4, "x", 16)
assert(a.x == 16 and k == 4)
a[b] = 'xuxu'
y = T.testC("gettable 2, return 1", b)
assert(y == 'xuxu')
T.testC("settable 2", b, 19)
assert(a[b] == 19)

--
do   -- testing getfield/setfield with long keys
  local t = {_012345678901234567890123456789012345678901234567890123456789 = 32}
  local a = T.testC([[
    getfield 2 _012345678901234567890123456789012345678901234567890123456789
    return 1
  ]], t)
  assert(a == 32)
  local a = T.testC([[
    pushnum 33
    setglobal _012345678901234567890123456789012345678901234567890123456789
  ]])
  assert(_012345678901234567890123456789012345678901234567890123456789 == 33)
  _012345678901234567890123456789012345678901234567890123456789 = nil
end

-- testing next
a = {}
t = pack(T.testC("next; return *", a, nil))
tcheck(t, {n=1,a})
a = {a=3}
t = pack(T.testC("next; return *", a, nil))
tcheck(t, {n=3,a,'a',3})
t = pack(T.testC("next; pop 1; next; return *", a, nil))
tcheck(t, {n=1,a})



-- testing upvalues

do
  local A = T.testC[[ pushnum 10; pushnum 20; pushcclosure 2; return 1]]
  t, b, c = A([[pushvalue U0; pushvalue U1; pushvalue U2; return 3]])
  assert(b == 10 and c == 20 and type(t) == 'table')
  a, b = A([[tostring U3; tonumber U4; return 2]])
  assert(a == nil and b == 0)
  A([[pushnum 100; pushnum 200; replace U2; replace U1]])
  b, c = A([[pushvalue U1; pushvalue U2; return 2]])
  assert(b == 100 and c == 200)
  A([[replace U2; replace U1]], {x=1}, {x=2})
  b, c = A([[pushvalue U1; pushvalue U2; return 2]])
  assert(b.x == 1 and c.x == 2)
  T.checkmemory()
end


-- testing absent upvalues from C-function pointers
assert(T.testC[[isnull U1; return 1]] == true)
assert(T.testC[[isnull U100; return 1]] == true)
assert(T.testC[[pushvalue U1; return 1]] == nil)

local f = T.testC[[ pushnum 10; pushnum 20; pushcclosure 2; return 1]]
assert(T.upvalue(f, 1) == 10 and
       T.upvalue(f, 2) == 20 and
       T.upvalue(f, 3) == nil)
T.upvalue(f, 2, "xuxu")
assert(T.upvalue(f, 2) == "xuxu")


-- large closures
do
  local A = "checkstack 300 msg;" ..
            string.rep("pushnum 10;", 255) ..
            "pushcclosure 255; return 1"
  A = T.testC(A)
  for i=1,255 do
    assert(A(("pushvalue U%d; return 1"):format(i)) == 10)
  end
  assert(A("isnull U256; return 1"))
  assert(not A("isnil U256; return 1"))
end



-- testing get/setuservalue
-- bug in 5.1.2
checkerr("got number", debug.setuservalue, 3, {})
checkerr("got nil", debug.setuservalue, nil, {})
checkerr("got light userdata", debug.setuservalue, T.pushuserdata(1), {})

-- testing multiple user values
local b = T.newuserdata(0, 10)
for i = 1, 10 do
  local v, p = debug.getuservalue(b, i)
  assert(v == nil and p)
end
do   -- indices out of range
  local v, p = debug.getuservalue(b, -2)
  assert(v == nil and not p)
  local v, p = debug.getuservalue(b, 11)
  assert(v == nil and not p)
end
local t = {true, false, 4.56, print, {}, b, "XYZ"}
for k, v in ipairs(t) do
  debug.setuservalue(b, v, k)
end
for k, v in ipairs(t) do
  local v1, p = debug.getuservalue(b, k)
  assert(v1 == v and p)
end

assert(not debug.getuservalue(4))

debug.setuservalue(b, function () return 10 end, 10)
collectgarbage()   -- function should not be collected
assert(debug.getuservalue(b, 10)() == 10)

debug.setuservalue(b, 134)
collectgarbage()   -- number should not be a problem for collector
assert(debug.getuservalue(b) == 134)


-- test barrier for uservalues
do
  local oldmode = collectgarbage("incremental")
  T.gcstate("atomic")
  assert(T.gccolor(b) == "black")
  debug.setuservalue(b, {x = 100})
  T.gcstate("pause")  -- complete collection
  assert(debug.getuservalue(b).x == 100)  -- uvalue should be there
  collectgarbage(oldmode)
end

-- long chain of userdata
for i = 1, 1000 do
  local bb = T.newuserdata(0, 1)
  debug.setuservalue(bb, b)
  b = bb
end
collectgarbage()     -- nothing should not be collected
for i = 1, 1000 do
  b = debug.getuservalue(b)
end
assert(debug.getuservalue(b).x == 100)
b = nil


-- testing locks (refs)

-- reuse of references
local i = T.ref{}
T.unref(i)
assert(T.ref{} == i)

local Arr = {}
local Lim = 100
for i=1,Lim do   -- lock many objects
  Arr[i] = T.ref({})
end

assert(T.ref(nil) == -1 and T.getref(-1) == nil)
T.unref(-1); T.unref(-1)

for i=1,Lim do   -- unlock all them
  T.unref(Arr[i])
end

local function printlocks ()
  local f = T.makeCfunc("gettable R; return 1")
  local n = f("n")
  print("n", n)
  for i=0,n do
    print(i, f(i))
  end
end


for i=1,Lim do   -- lock many objects
  Arr[i] = T.ref({})
end

for i=1,Lim,2 do   -- unlock half of them
  T.unref(Arr[i])
end

assert(type(T.getref(Arr[2])) == 'table')


assert(T.getref(-1) == nil)


a = T.ref({})

collectgarbage()

assert(type(T.getref(a)) == 'table')


-- colect in cl the `val' of all collected userdata
local tt = {}
local cl = {n=0}
A = nil; B = nil
local F
F = function (x)
  local udval = T.udataval(x)
  table.insert(cl, udval)
  local d = T.newuserdata(100)   -- create garbage
  d = nil
  assert(debug.getmetatable(x).__gc == F)
  assert(load("table.insert({}, {})"))()   -- create more garbage
  assert(not collectgarbage())    -- GC during GC (no op)
  local dummy = {}    -- create more garbage during GC
  if A ~= nil then
    assert(type(A) == "userdata")
    assert(T.udataval(A) == B)
    debug.getmetatable(A)    -- just access it
  end
  A = x   -- ressurect userdata
  B = udval
  return 1,2,3
end
tt.__gc = F


-- test whether udate collection frees memory in the right time
do
  collectgarbage();
  collectgarbage();
  local x = collectgarbage("count");
  local a = T.newuserdata(5001)
  assert(T.testC("objsize 2; return 1", a) == 5001)
  assert(collectgarbage("count") >= x+4)
  a = nil
  collectgarbage();
  assert(collectgarbage("count") <= x+1)
  -- udata without finalizer
  x = collectgarbage("count")
  collectgarbage("stop")
  for i=1,1000 do T.newuserdata(0) end
  assert(collectgarbage("count") > x+10)
  collectgarbage()
  assert(collectgarbage("count") <= x+1)
  -- udata with finalizer
  collectgarbage()
  x = collectgarbage("count")
  collectgarbage("stop")
  a = {__gc = function () end}
  for i=1,1000 do debug.setmetatable(T.newuserdata(0), a) end
  assert(collectgarbage("count") >= x+10)
  collectgarbage()  -- this collection only calls TM, without freeing memory
  assert(collectgarbage("count") >= x+10)
  collectgarbage()  -- now frees memory
  assert(collectgarbage("count") <= x+1)
  collectgarbage("restart")
end


collectgarbage("stop")

-- create 3 userdatas with tag `tt'
a = T.newuserdata(0); debug.setmetatable(a, tt); local na = T.udataval(a)
b = T.newuserdata(0); debug.setmetatable(b, tt); local nb = T.udataval(b)
c = T.newuserdata(0); debug.setmetatable(c, tt); local nc = T.udataval(c)

-- create userdata without meta table
x = T.newuserdata(4)
y = T.newuserdata(0)

checkerr("FILE%* expected, got userdata", io.input, a)
checkerr("FILE%* expected, got userdata", io.input, x)

assert(debug.getmetatable(x) == nil and debug.getmetatable(y) == nil)

local d = T.ref(a);
local e = T.ref(b);
local f = T.ref(c);
t = {T.getref(d), T.getref(e), T.getref(f)}
assert(t[1] == a and t[2] == b and t[3] == c)

t=nil; a=nil; c=nil;
T.unref(e); T.unref(f)

collectgarbage()

-- check that unref objects have been collected
assert(#cl == 1 and cl[1] == nc)

x = T.getref(d)
assert(type(x) == 'userdata' and debug.getmetatable(x) == tt)
x =nil
tt.b = b  -- create cycle
tt=nil    -- frees tt for GC
A = nil
b = nil
T.unref(d);
local n5 = T.newuserdata(0)
debug.setmetatable(n5, {__gc=F})
n5 = T.udataval(n5)
collectgarbage()
assert(#cl == 4)
-- check order of collection
assert(cl[2] == n5 and cl[3] == nb and cl[4] == na)

collectgarbage"restart"


a, na = {}, {}
for i=30,1,-1 do
  a[i] = T.newuserdata(0)
  debug.setmetatable(a[i], {__gc=F})
  na[i] = T.udataval(a[i])
end
cl = {}
a = nil; collectgarbage()
assert(#cl == 30)
for i=1,30 do assert(cl[i] == na[i]) end
na = nil


for i=2,Lim,2 do   -- unlock the other half
  T.unref(Arr[i])
end

x = T.newuserdata(41); debug.setmetatable(x, {__gc=F})
assert(T.testC("objsize 2; return 1", x) == 41)
cl = {}
a = {[x] = 1}
x = T.udataval(x)
collectgarbage()
-- old `x' cannot be collected (`a' still uses it)
assert(#cl == 0)
for n in pairs(a) do a[n] = undef end
collectgarbage()
assert(#cl == 1 and cl[1] == x)   -- old `x' must be collected

-- testing lua_equal
assert(T.testC("compare EQ 2 4; return 1", print, 1, print, 20))
assert(T.testC("compare EQ 3 2; return 1", 'alo', "alo"))
assert(T.testC("compare EQ 2 3; return 1", nil, nil))
assert(not T.testC("compare EQ 2 3; return 1", {}, {}))
assert(not T.testC("compare EQ 2 3; return 1"))
assert(not T.testC("compare EQ 2 3; return 1", 3))

-- testing lua_equal with fallbacks
do
  local map = {}
  local t = {__eq = function (a,b) return map[a] == map[b] end}
  local function f(x)
    local u = T.newuserdata(0)
    debug.setmetatable(u, t)
    map[u] = x
    return u
  end
  assert(f(10) == f(10))
  assert(f(10) ~= f(11))
  assert(T.testC("compare EQ 2 3; return 1", f(10), f(10)))
  assert(not T.testC("compare EQ 2 3; return 1", f(10), f(20)))
  t.__eq = nil
  assert(f(10) ~= f(10))
end

print'+'



-- testing changing hooks during hooks
_G.TT = {}
T.sethook([[
  # set a line hook after 3 count hooks
  sethook 4 0 '
    getglobal TT;
    pushvalue -3; append -2
    pushvalue -2; append -2
  ']], "c", 3)
local a = 1   -- counting
a = 1   -- counting
a = 1   -- count hook (set line hook)
a = 1   -- line hook
a = 1   -- line hook
debug.sethook()
local t = _G.TT
assert(t[1] == "line")
local line = t[2]
assert(t[3] == "line" and t[4] == line + 1)
assert(t[5] == "line" and t[6] == line + 2)
assert(t[7] == nil)
_G.TT = nil


-------------------------------------------------------------------------
do   -- testing errors during GC
  warn("@off")
  collectgarbage("stop")
  local a = {}
  for i=1,20 do
    a[i] = T.newuserdata(i)   -- creates several udata
  end
  for i=1,20,2 do   -- mark half of them to raise errors during GC
    debug.setmetatable(a[i],
      {__gc = function (x) error("@expected error in gc") end})
  end
  for i=2,20,2 do   -- mark the other half to count and to create more garbage
    debug.setmetatable(a[i], {__gc = function (x) load("A=A+1")() end})
  end
  a = nil
  _G.A = 0
  collectgarbage()
  assert(A == 10)  -- number of normal collections
  collectgarbage("restart")
  warn("@on")
end
_G.A = nil
-------------------------------------------------------------------------
-- test for userdata vals
do
  local a = {}; local lim = 30
  for i=0,lim do a[i] = T.pushuserdata(i) end
  for i=0,lim do assert(T.udataval(a[i]) == i) end
  for i=0,lim do assert(T.pushuserdata(i) == a[i]) end
  for i=0,lim do a[a[i]] = i end
  for i=0,lim do a[T.pushuserdata(i)] = i end
  assert(type(tostring(a[1])) == "string")
end


-------------------------------------------------------------------------
-- testing multiple states
T.closestate(T.newstate());
L1 = T.newstate()
assert(L1)

assert(T.doremote(L1, "X='a'; return 'a'") == 'a')


assert(#pack(T.doremote(L1, "function f () return 'alo', 3 end; f()")) == 0)

a, b = T.doremote(L1, "return f()")
assert(a == 'alo' and b == '3')

T.doremote(L1, "_ERRORMESSAGE = nil")
-- error: `sin' is not defined
a, b, c = T.doremote(L1, "return sin(1)")
assert(a == nil and c == 2)   -- 2 == run-time error

-- error: syntax error
a, b, c = T.doremote(L1, "return a+")
assert(a == nil and c == 3 and type(b) == "string")   -- 3 == syntax error

T.loadlib(L1)
a, b, c = T.doremote(L1, [[
  string = require'string'
  a = require'_G'; assert(a == _G and require("_G") == a)
  io = require'io'; assert(type(io.read) == "function")
  assert(require("io") == io)
  a = require'table'; assert(type(a.insert) == "function")
  a = require'debug'; assert(type(a.getlocal) == "function")
  a = require'math'; assert(type(a.sin) == "function")
  return string.sub('okinama', 1, 2)
]])
assert(a == "ok")

T.closestate(L1);


L1 = T.newstate()
T.loadlib(L1)
T.doremote(L1, "a = {}")
T.testC(L1, [[getglobal "a"; pushstring "x"; pushint 1;
             settable -3]])
assert(T.doremote(L1, "return a.x") == "1")

T.closestate(L1)

L1 = nil

print('+')
-------------------------------------------------------------------------
-- testing to-be-closed variables
-------------------------------------------------------------------------
print"testing to-be-closed variables"

do
  local openresource = {}

  local function newresource ()
    local x = setmetatable({10}, {__close = function(y)
      assert(openresource[#openresource] == y)
      openresource[#openresource] = nil
      y[1] = y[1] + 1
    end})
    openresource[#openresource + 1] = x
    return x
  end

  local a, b = T.testC([[
    call 0 1   # create resource
    pushnil
    toclose -2  # mark call result to be closed
    toclose -1  # mark nil to be closed (will be ignored)
    return 2
  ]], newresource)
  assert(a[1] == 11 and b == nil)
  assert(#openresource == 0)    -- was closed

  -- repeat the test, but calling function in a 'multret' context
  local a = {T.testC([[
    call 0 1   # create resource
    toclose 2 # mark it to be closed
    return 2
  ]], newresource)}
  assert(type(a[1]) == "string" and a[2][1] == 11)
  assert(#openresource == 0)    -- was closed

  -- closing by error
  local a, b = pcall(T.makeCfunc[[
    call 0 1   # create resource
    toclose -1 # mark it to be closed
    error       # resource is the error object
  ]], newresource)
  assert(a == false and b[1] == 11)
  assert(#openresource == 0)    -- was closed

  -- non-closable value
  local a, b = pcall(T.makeCfunc[[
    newtable   # create non-closable object
    toclose -1 # mark it to be closed (should raise an error)
    abort  # will not be executed
  ]])
  assert(a == false and
    string.find(b, "non%-closable value"))

  local function check (n)
    assert(#openresource == n)
  end

  -- closing resources with 'closeslot'
  _ENV.xxx = true
  local a = T.testC([[
    pushvalue 2  # stack: S, NR, CH, NR
    call 0 1   # create resource; stack: S, NR, CH, R
    toclose -1 # mark it to be closed
    pushvalue 2  #  stack: S, NR, CH, R, NR
    call 0 1   # create another resource; stack: S, NR, CH, R, R
    toclose -1 # mark it to be closed
    pushvalue 3  # stack: S, NR, CH, R, R, CH
    pushint 2   # there should be two open resources
    call 1 0  #  stack: S, NR, CH, R, R
    closeslot -1   # close second resource
    pushvalue 3  # stack: S, NR, CH, R, R, CH
    pushint 1   # there should be one open resource
    call 1 0  # stack: S, NR, CH, R, R
    closeslot 4
    setglobal "xxx"  # previous op. erased the slot
    pop 1       # pop other resource from the stack
    pushint *
    return 1    # return stack size
  ]], newresource, check)
  assert(a == 3 and _ENV.xxx == nil)   -- no extra items left in the stack

  -- closing resources with 'pop'
  local a = T.testC([[
    pushvalue 2  # stack: S, NR, CH, NR
    call 0 1   # create resource; stack: S, NR, CH, R
    toclose -1 # mark it to be closed
    pushvalue 2  #  stack: S, NR, CH, R, NR
    call 0 1   # create another resource; stack: S, NR, CH, R, R
    toclose -1 # mark it to be closed
    pushvalue 3  # stack: S, NR, CH, R, R, CH
    pushint 2   # there should be two open resources
    call 1 0  #  stack: S, NR, CH, R, R
    pop 1   # pop second resource
    pushvalue 3  # stack: S, NR, CH, R, CH
    pushint 1   # there should be one open resource
    call 1 0  # stack: S, NR, CH, R
    pop 1       # pop other resource from the stack
    pushvalue 3  # stack: S, NR, CH, CH
    pushint 0   # there should be no open resources
    call 1 0  # stack: S, NR, CH
    pushint *
    return 1    # return stack size
  ]], newresource, check)
  assert(a == 3)   -- no extra items left in the stack

  -- non-closable value
  local a, b = pcall(T.makeCfunc[[
    pushint 32
    toclose -1
  ]])
  assert(not a and string.find(b, "(C temporary)"))

end


--[[
** {==================================================================
** Testing memory limits
** ===================================================================
--]]

print("memory-allocation errors")

checkerr("block too big", T.newuserdata, math.maxinteger)
collectgarbage()
local f = load"local a={}; for i=1,100000 do a[i]=i end"
T.alloccount(10)
checkerr(MEMERRMSG, f)
T.alloccount()          -- remove limit


-- test memory errors; increase limit for maximum memory by steps,
-- o that we get memory errors in all allocations of a given
-- task, until there is enough memory to complete the task without
-- errors.
local function testbytes (s, f)
  collectgarbage()
  local M = T.totalmem()
  local oldM = M
  local a,b = nil
  while true do
    collectgarbage(); collectgarbage()
    T.totalmem(M)
    a, b = T.testC("pcall 0 1 0; pushstatus; return 2", f)
    T.totalmem(0)  -- remove limit
    if a and b == "OK" then break end       -- stop when no more errors
    if b ~= "OK" and b ~= MEMERRMSG then    -- not a memory error?
      error(a, 0)   -- propagate it
    end
    M = M + 7   -- increase memory limit
  end
  print(string.format("minimum memory for %s: %d bytes", s, M - oldM))
  return a
end

-- test memory errors; increase limit for number of allocations one
-- by one, so that we get memory errors in all allocations of a given
-- task, until there is enough allocations to complete the task without
-- errors.

local function testalloc (s, f)
  collectgarbage()
  local M = 0
  local a,b = nil
  while true do
    collectgarbage(); collectgarbage()
    T.alloccount(M)
    a, b = T.testC("pcall 0 1 0; pushstatus; return 2", f)
    T.alloccount()  -- remove limit
    if a and b == "OK" then break end       -- stop when no more errors
    if b ~= "OK" and b ~= MEMERRMSG then    -- not a memory error?
      error(a, 0)   -- propagate it
    end
    M = M + 1   -- increase allocation limit
  end
  print(string.format("minimum allocations for %s: %d allocations", s, M))
  return a
end


local function testamem (s, f)
  testalloc(s, f)
  return testbytes(s, f)
end


-- doing nothing
b = testamem("doing nothing", function () return 10 end)
assert(b == 10)

-- testing memory errors when creating a new state

testamem("state creation", function ()
  local st = T.newstate()
  if st then T.closestate(st) end   -- close new state
  return st
end)

testamem("empty-table creation", function ()
  return {}
end)

testamem("string creation", function ()
  return "XXX" .. "YYY"
end)

testamem("coroutine creation", function()
           return coroutine.create(print)
end)


-- testing to-be-closed variables
testamem("to-be-closed variables", function()
  local flag
  do
    local x <close> =
              setmetatable({}, {__close = function () flag = true end})
    flag = false
    local x = {}
  end
  return flag
end)


-- testing threads

-- get main thread from registry (at index LUA_RIDX_MAINTHREAD == 1)
local mt = T.testC("rawgeti R 1; return 1")
assert(type(mt) == "thread" and coroutine.running() == mt)



local function expand (n,s)
  if n==0 then return "" end
  local e = string.rep("=", n)
  return string.format("T.doonnewstack([%s[ %s;\n collectgarbage(); %s]%s])\n",
                              e, s, expand(n-1,s), e)
end

G=0; collectgarbage(); a =collectgarbage("count")
load(expand(20,"G=G+1"))()
assert(G==20); collectgarbage();  -- assert(gcinfo() <= a+1)
G = nil

testamem("running code on new thread", function ()
  return T.doonnewstack("local x=1") == 0  -- try to create thread
end)


-- testing memory x compiler

testamem("loadstring", function ()
  return load("x=1")  -- try to do load a string
end)


local testprog = [[
local function foo () return end
local t = {"x"}
AA = "aaa"
for i = 1, #t do AA = AA .. t[i] end
return true
]]

-- testing memory x dofile
_G.AA = nil
local t =os.tmpname()
local f = assert(io.open(t, "w"))
f:write(testprog)
f:close()
testamem("dofile", function ()
  local a = loadfile(t)
  return a and a()
end)
assert(os.remove(t))
assert(_G.AA == "aaax")


-- other generic tests

testamem("gsub", function ()
  local a, b = string.gsub("alo alo", "(a)", function (x) return x..'b' end)
  return (a == 'ablo ablo')
end)

testamem("dump/undump", function ()
  local a = load(testprog)
  local b = a and string.dump(a)
  a = b and load(b)
  return a and a()
end)

_G.AA = nil

local t = os.tmpname()
testamem("file creation", function ()
  local f = assert(io.open(t, 'w'))
  assert (not io.open"nomenaoexistente")
  io.close(f);
  return not loadfile'nomenaoexistente'
end)
assert(os.remove(t))

testamem("table creation", function ()
  local a, lim = {}, 10
  for i=1,lim do a[i] = i; a[i..'a'] = {} end
  return (type(a[lim..'a']) == 'table' and a[lim] == lim)
end)

testamem("constructors", function ()
  local a = {10, 20, 30, 40, 50; a=1, b=2, c=3, d=4, e=5}
  return (type(a) == 'table' and a.e == 5)
end)

local a = 1
local close = nil
testamem("closure creation", function ()
  function close (b)
   return function (x) return b + x end
  end
  return (close(2)(4) == 6)
end)

testamem("using coroutines", function ()
  local a = coroutine.wrap(function ()
              coroutine.yield(string.rep("a", 10))
              return {}
            end)
  assert(string.len(a()) == 10)
  return a()
end)

do   -- auxiliary buffer
  local lim = 100
  local a = {}; for i = 1, lim do a[i] = "01234567890123456789" end
  testamem("auxiliary buffer", function ()
    return (#table.concat(a, ",") == 20*lim + lim - 1)
  end)
end

testamem("growing stack", function ()
  local function foo (n)
    if n == 0 then return 1 else return 1 + foo(n - 1) end
  end
  return foo(100)
end)

-- }==================================================================


do   -- testing failing in 'lua_checkstack'
  local res = T.testC([[rawcheckstack 500000; return 1]])
  assert(res == false)
  local L = T.newstate()
  T.alloccount(0)   -- will be unable to reallocate the stack
  res = T.testC(L, [[rawcheckstack 5000; return 1]])
  T.alloccount()
  T.closestate(L)
  assert(res == false)
end

do   -- closing state with no extra memory
  local L = T.newstate()
  T.alloccount(0)
  T.closestate(L)
  T.alloccount()
end

do   -- garbage collection with no extra memory
  local L = T.newstate()
  T.loadlib(L)
  local res = (T.doremote(L, [[
    _ENV = require"_G"
    local T = require"T"
    local a = {}
    for i = 1, 1000 do a[i] = 'i' .. i end    -- grow string table
    local stsize, stuse = T.querystr()
    assert(stuse > 1000)
    local function foo (n)
      if n > 0 then foo(n - 1) end
    end
    foo(180)    -- grow stack
    local _, stksize = T.stacklevel()
    assert(stksize > 180)
    a = nil
    T.alloccount(0)
    collectgarbage()
    T.alloccount()
    -- stack and string table could not be reallocated,
    -- so they kept their sizes (without errors)
    assert(select(2, T.stacklevel()) == stksize)
    assert(T.querystr() == stsize)
    return 'ok'
  ]]))
  assert(res == 'ok')
  T.closestate(L)
end

print'+'

-- testing some auxlib functions
local function gsub (a, b, c)
  a, b = T.testC("gsub 2 3 4; gettop; return 2", a, b, c)
  assert(b == 5)
  return a
end

assert(gsub("alo.alo.uhuh.", ".", "//") == "alo//alo//uhuh//")
assert(gsub("alo.alo.uhuh.", "alo", "//") == "//.//.uhuh.")
assert(gsub("", "alo", "//") == "")
assert(gsub("...", ".", "/.") == "/././.")
assert(gsub("...", "...", "") == "")


-- testing luaL_newmetatable
local mt_xuxu, res, top = T.testC("newmetatable xuxu; gettop; return 3")
assert(type(mt_xuxu) == "table" and res and top == 3)
local d, res, top = T.testC("newmetatable xuxu; gettop; return 3")
assert(mt_xuxu == d and not res and top == 3)
d, res, top = T.testC("newmetatable xuxu1; gettop; return 3")
assert(mt_xuxu ~= d and res and top == 3)

x = T.newuserdata(0);
y = T.newuserdata(0);
T.testC("pushstring xuxu; gettable R; setmetatable 2", x)
assert(getmetatable(x) == mt_xuxu)

-- testing luaL_testudata
-- correct metatable
local res1, res2, top = T.testC([[testudata -1 xuxu
   	 			  testudata 2 xuxu
				  gettop
				  return 3]], x)
assert(res1 and res2 and top == 4)

-- wrong metatable
res1, res2, top = T.testC([[testudata -1 xuxu1
			    testudata 2 xuxu1
			    gettop
			    return 3]], x)
assert(not res1 and not res2 and top == 4)

-- non-existent type
res1, res2, top = T.testC([[testudata -1 xuxu2
			    testudata 2 xuxu2
			    gettop
			    return 3]], x)
assert(not res1 and not res2 and top == 4)

-- userdata has no metatable
res1, res2, top = T.testC([[testudata -1 xuxu
			    testudata 2 xuxu
			    gettop
			    return 3]], y)
assert(not res1 and not res2 and top == 4)

-- erase metatables
do
  local r = debug.getregistry()
  assert(r.xuxu == mt_xuxu and r.xuxu1 == d)
  r.xuxu = nil; r.xuxu1 = nil
end

print'OK'

