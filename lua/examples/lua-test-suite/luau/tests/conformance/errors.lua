-- This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
-- This file is based on Lua 5.x tests -- https://github.com/lua/lua/tree/master/testes
print("testing errors")

function doit (s)
  local f, msg = loadstring(s)
  if f == nil then return msg end
  local cond, msg = pcall(f)
  return (not cond) and msg
end


function checkmessage (prog, msg)
  -- assert(string.find(doit(prog), msg, 1, true))
end

function checksyntax (prog, extra, token, line)
  local msg = doit(prog)
  token = string.gsub(token, "(%p)", "%%%1")
  local pt = string.format([[^%%[string ".*"%%]:]]) -- only check that an error happened - ignore token/line since Luau error messages differ from Lua substantially
  assert(string.find(msg, pt))
  assert(string.find(msg, msg, 1, true))
end


-- test error message with no extra info
assert(doit("error('hi', 0)") == 'hi')

-- test error message with no info
-- assert(doit("error()") == nil)


-- test common errors/errors that crashed in the past
assert(doit("unpack({}, 1, n=2^30)"))
assert(doit("a=math.sin()"))
assert(not doit("tostring(1)") and doit("tostring()"))
assert(doit("tonumber()"))
assert(doit("repeat until 1; a"))
checksyntax("break label", "", "label", 1)
assert(doit(";"))
assert(doit("a=1;;"))
assert(doit("return;;"))
assert(doit("assert(false)"))
assert(doit("assert(nil)"))
assert(doit("a=math.sin\n(3)"))
assert(doit("function a (... , ...) end"))
assert(doit("function a (, ...) end"))

checksyntax([[
  local a = {4

]], "'}' expected (to close '{' at line 1)", "<eof>", 3)


-- tests for better error messages

checkmessage("a=1; bbbb=2; a=math.sin(3)+bbbb(3)", "global 'bbbb'")
checkmessage("a=1; local a,bbbb=2,3; a = math.sin(1) and bbbb(3)",
       "local 'bbbb'")
checkmessage("a={}; do local a=1 end a:bbbb(3)", "method 'bbbb'")
checkmessage("local a={}; a.bbbb(3)", "field 'bbbb'")
assert(not string.find(doit("a={13}; local bbbb=1; a[bbbb](3)"), "'bbbb'"))
checkmessage("a={13}; local bbbb=1; a[bbbb](3)", "number")

aaa = nil
checkmessage("aaa.bbb:ddd(9)", "global 'aaa'")
checkmessage("local aaa={bbb=1}; aaa.bbb:ddd(9)", "field 'bbb'")
checkmessage("local aaa={bbb={}}; aaa.bbb:ddd(9)", "method 'ddd'")
checkmessage("local a,b,c; (function () a = b+1 end)()", "upvalue 'b'")
assert(not doit("local aaa={bbb={ddd=next}}; aaa.bbb:ddd(nil)"))

checkmessage("b=1; local aaa='a'; x=aaa+b", "local 'aaa'")
checkmessage("aaa={}; x=3/aaa", "global 'aaa'")
checkmessage("aaa='2'; b=nil;x=aaa*b", "global 'b'")
checkmessage("aaa={}; x=-aaa", "global 'aaa'")
assert(not string.find(doit("aaa={}; x=(aaa or aaa)+(aaa and aaa)"), "'aaa'"))
assert(not string.find(doit("aaa={}; (aaa or aaa)()"), "'aaa'"))

checkmessage([[aaa=9
repeat until 3==3
local x=math.sin(math.cos(3))
if math.sin(1) == x then return math.sin(1) end   -- tail call
local a,b = 1, {
  {x='a'..'b'..'c', y='b', z=x},
  {1,2,3,4,5} or 3+3<=3+3,
  3+1>3+1,
  {d = x and aaa[x or y]}}
]], "global 'aaa'")

checkmessage([[
local x,y = {},1
if math.sin(1) == 0 then return 3 end    -- return
x.a()]], "field 'a'")

checkmessage([[
prefix = nil
insert = nil
while 1 do  
  local a
  if nil then break end
  insert(prefix, a)
end]], "global 'insert'")

checkmessage([[  -- tail call
  return math.sin("a")
]], "'sin'")

checkmessage([[collectgarbage("nooption")]], "invalid option")

checkmessage([[x = print .. "a"]], "concatenate")

checkmessage("getmetatable(io.stdin).__gc()", "no value")

print'+'


-- testing line error

function lineerror (s)
  local err,msg = pcall(loadstring(s))
  local line = string.match(msg, ":(%d+):")
  return line and line+0
end

assert(lineerror("local a\n for i=1,'a' do \n print(i) \n end") == 2)
-- assert(lineerror("\n local a \n for k,v in 3 \n do \n print(k) \n end") == 3)
-- assert(lineerror("\n\n for k,v in \n 3 \n do \n print(k) \n end") == 4)
assert(lineerror("function a.x.y ()\na=a+1\nend") == 1)

local p = [[
function g() f() end
function f(x) error('a', X) end
g()
]]
X=3;assert(lineerror(p) == 3)
X=0;assert(lineerror(p) == nil)
X=1;assert(lineerror(p) == 2)
X=2;assert(lineerror(p) == 1)

lineerror = nil

if not limitedstack then
  C = 0
  -- local l = debug.getinfo(1, "l").currentline
  function y () C=C+1; y() end

  local function checkstackmessage (m)
    return (string.find(m, "^.-:%d+: stack overflow"))
  end

  assert(checkstackmessage(doit('y()')))
  assert(checkstackmessage(doit('y()')))
  assert(checkstackmessage(doit('y()')))

  -- teste de linhas em erro
  C = 0
  local l1
  local function g()
    -- l1 = debug.getinfo(1, "l").currentline
    y()
  end
  local _, stackmsg = xpcall(g, debug.traceback)
  local stack = {}
  for line in string.gmatch(stackmsg, "[^\n]*") do
    local curr = string.match(line, ":(%d+):")
    if curr then table.insert(stack, tonumber(curr)) end
  end
end

-- C stack overflow
if not limitedstack then
  local count = 1
  local cso = setmetatable({}, {
    __index = function(self, i)
      count = count + 1
      return self[i]
    end,
    __newindex = function(self, i, v)
      count = count + 1
      self[i] = v
    end,
    __tostring = function(self)
      count = count + 1
      return tostring(self)
    end
  })

  local ehline
  local function ehassert(cond)
    if not cond then
      ehline = debug.info(2, "l")
      error()
    end
  end

  local userdata = newproxy(true)
  getmetatable(userdata).__index = print
  assert(debug.info(print, "s") == "[C]")

  local s, e = xpcall(tostring, function(e)
    ehassert(string.find(e, "C stack overflow"))
    print("after __tostring C stack overflow", count) -- 198: 1 resume + 1 xpcall + 198 luaB_tostring calls (which runs our __tostring successfully 197 times, erroring on the last attempt)
    ehassert(count > 1)

    local ps, pe

    -- __tostring overflow (lua_call)
    count = 1
    ps, pe = pcall(tostring, cso)
    print("after __tostring overflow in handler", count) -- 23: xpcall error handler + pcall + 23 luaB_tostring calls
    ehassert(not ps and string.find(pe, "error in error handling"))
    ehassert(count > 1)

    -- __index overflow (callTMres)
    count = 1
    ps, pe = pcall(function() return cso[cso] end)
    print("after __index overflow in handler", count) -- 23: xpcall error handler + pcall + 23 __index calls
    ehassert(not ps and string.find(pe, "error in error handling"))
    ehassert(count > 1)

    -- __newindex overflow (callTM)
    count = 1
    ps, pe = pcall(function() cso[cso] = "kohuke" end)
    print("after __newindex overflow in handler", count) -- 23: xpcall error handler + pcall + 23 __newindex calls
    ehassert(not ps and string.find(pe, "error in error handling"))
    ehassert(count > 1)

    -- test various C __index invocations on userdata
    ehassert(pcall(function() return userdata[userdata] end)) -- LOP_GETTABLE
    ehassert(pcall(function() return userdata[1] end)) -- LOP_GETTABLEN
    ehassert(pcall(function() return userdata.StringConstant end)) -- LOP_GETTABLEKS (luau_callTM)

    -- lua_resume test
    local coro = coroutine.create(function() end)
    ps, pe = coroutine.resume(coro)
    ehassert(not ps and string.find(pe, "C stack overflow"))

    return true
  end, cso)

  assert(not s)
  assert(e == true, "error in xpcall eh, line " .. tostring(ehline))
end

--[[
local i=1
while stack[i] ~= l1 do
  assert(stack[i] == l)
  i = i+1
end
assert(i > 15)
]]--

-- error in error handling
local res, msg = xpcall(error, error)
assert(not res and type(msg) == 'string')

local function f (x)
  if x==0 then error('a\n')
  else
    local aux = function () return f(x-1) end
    local a,b = xpcall(aux, aux)
    return a,b
  end
end

if not limitedstack then
  f(3)
end

-- non string messages
function f() error{msg='x'} end
res, msg = xpcall(f, function (r) return {msg=r.msg..'y'} end)
assert(msg.msg == 'xy')

print('+')
checksyntax("syntax error", "", "error", 1)
checksyntax("1.000", "", "1.000", 1)
checksyntax("[[a]]", "", "[[a]]", 1)
checksyntax("'aa'", "", "'aa'", 1)

-- test 255 as first char in a chunk
checksyntax("\255a = 1", "", "\255", 1)

doit('I = loadstring("a=9+"); a=3')
assert(a==3 and I == nil)
print('+')

lim = 1000
if rawget(_G, "_soft") then lim = 100 end
for i=1,lim do
  doit('a = ')
  doit('a = 4+nil')
end


-- testing syntax limits
local function testrep (init, rep)
  local s = "local a; "..init .. string.rep(rep, 300)
  local a,b = loadstring(s)
  assert(not a) -- and string.find(b, "syntax levels"))
end
testrep("a=", "{")
testrep("a=", "(")
testrep("", "a(")
testrep("", "do ")
testrep("", "while a do ")
testrep("", "if a then else ")
testrep("", "function foo () ")
testrep("a=", "a..")
testrep("a=", "a^")


-- testing other limits
-- upvalues
local  s = "function foo ()\n  local "
for j = 1,70 do
  s = s.."a"..j..", "
end
s = s.."b\n"
for j = 1,70 do
  s = s.."function foo"..j.." ()\n a"..j.."=3\n"
end
local a,b = loadstring(s)
assert(not a)
-- assert(string.find(b, "line 3"))

-- local variables
s = "\nfunction foo ()\n  local "
for j = 1,300 do
  s = s.."a"..j..", " 
end
s = s.."b\n"
local a,b = loadstring(s)
assert(not a)
--assert(string.find(b, "line 2"))

-- The xpcall is intentionally going to cause an exception
-- followed by a forced exception in the error handler.
-- If the secondary handler isn't trapped, it will cause
-- the unit test to fail. If the xpcall captures the
-- second fault, it's a success.

a, b = xpcall(
		function() 
			return game[{}]
		end, 
		function() 
			return game.CoreGui.Name
		end)
assert(not a)
print(b)

coroutine.wrap(function()
  assert(not pcall(debug.getinfo, coroutine.running(), 0, ">"))
end)()

-- loadstring chunk truncation
local a,b = loadstring("nope", "@short")
assert(not a and b:match('[^ ]+') == "short:1:")

local a,b = loadstring("nope", "@" .. string.rep("thisisaverylongstringitssolongthatitwontfitintotheinternalbufferprovidedtovariousdebugfacilities", 10))
assert(not a and b:match('[^ ]+') == "...wontfitintotheinternalbufferprovidedtovariousdebugfacilitiesthisisaverylongstringitssolongthatitwontfitintotheinternalbufferprovidedtovariousdebugfacilitiesthisisaverylongstringitssolongthatitwontfitintotheinternalbufferprovidedtovariousdebugfacilities:1:")

local a,b = loadstring("nope", "=short")
assert(not a and b:match('[^ ]+') == "short:1:")

local a,b = loadstring("nope", "=" .. string.rep("thisisaverylongstringitssolongthatitwontfitintotheinternalbufferprovidedtovariousdebugfacilities", 10))
assert(not a and b:match('[^ ]+') == "thisisaverylongstringitssolongthatitwontfitintotheinternalbufferprovidedtovariousdebugfacilitiesthisisaverylongstringitssolongthatitwontfitintotheinternalbufferprovidedtovariousdebugfacilitiesthisisaverylongstringitssolongthatitwontfitintotheinternalbuffe:1:")

-- arith errors
function ecall(fn, ...)
  local ok, err = pcall(fn, ...)
  assert(not ok)
  return err:sub(err:find(": ") + 2, #err)
end

assert(ecall(function() return nil + 5 end) == "attempt to perform arithmetic (add) on nil and number")
assert(ecall(function() return "a" + "b" end) == "attempt to perform arithmetic (add) on string")
assert(ecall(function() return 1 > nil end) == "attempt to compare nil < number") -- note reversed order (by design)
assert(ecall(function() return "a" <= 5 end) == "attempt to compare string <= number")

assert(ecall(function() local t = {} setmetatable(t, { __newindex = function(t,i,v) end }) t[nil] = 2 end) == "table index is nil")

-- for loop type errors
assert(ecall(function() for i='a',2 do end end) == "invalid 'for' initial value (number expected, got string)")
assert(ecall(function() for i=1,'a' do end end) == "invalid 'for' limit (number expected, got string)")
assert(ecall(function() for i=1,2,'a' do end end) == "invalid 'for' step (number expected, got string)")

return('OK')
