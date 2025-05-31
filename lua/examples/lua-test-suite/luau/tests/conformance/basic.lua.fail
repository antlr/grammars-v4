-- This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
print("testing language/library basics")

function concat(head, ...)
    if select('#', ...) == 0 then
        return tostring(head)
    else
        return tostring(head) .. "," .. concat(...)
    end
end

-- constants
assert(tostring(1) == "1")
assert(tostring(-1) == "-1")
assert(tostring(1.125) == "1.125")
assert(tostring(true) == "true")
assert(tostring(nil) == "nil")

-- empty return
assert(select('#', (function() end)()) == 0)
assert(select('#', (function() return end)()) == 0)

-- locals
assert((function() local a = 1 return a end)() == 1)
assert((function() local a, b, c = 1, 2, 3 return c end)() == 3)
assert((function() local a, b, c = 1, 2 return c end)() == nil)
assert((function() local a = 1, 2 return a end)() == 1)

-- function calls
local function foo(a, b) return b end
assert(foo(1) == nil)
assert(foo(1, 2) == 2)
assert(foo(1, 2, 3) == 2)

-- pcall
assert(concat(pcall(function () end)) ==  "true")
assert(concat(pcall(function () return nil end)) ==  "true,nil")
assert(concat(pcall(function () return 1,2,3 end)) ==  "true,1,2,3")
assert(concat(pcall(function () error("oops") end)) ==  "false,basic.lua:39: oops")

-- assignments
assert((function() local a = 1 a = 2 return a end)() == 2)
assert((function() a = 1 a = 2 return a end)() == 2)
assert((function() local a = 1 a, b = 1 return a end)() == 1)
assert((function() local a = 1 a, b = 1 return b end)() == nil)
assert((function() local a = 1 b = 2 a, b = b, a return a end)() == 2)
assert((function() local a = 1 b = 2 a, b = b, a return b end)() == 1)
assert((function() _G.foo = 1 return _G['foo'] end)() == 1)
assert((function() _G['bar'] = 1 return _G.bar end)() == 1)
assert((function() local a = 1 (function () a = 2 end)() return a end)() == 2)

-- upvalues
assert((function() local a = 1 function foo() return a end return foo() end)() == 1)

-- check upvalue propagation - foo must have numupvalues=1
assert((function() local a = 1 function foo() return function() return a end end return foo()() end)() == 1)

-- check that function args are properly closed over
assert((function() function foo(a) return function () return a end end return foo(1)() end)() == 1)

-- this checks local aliasing - b & a should share the same local slot, but the capture must return 1 instead of 2
assert((function() function foo() local f do local a = 1 f = function () return a end end local b = 2 return f end return foo()() end)() == 1)

-- this checks local mutability - we capture a ref to 1 but must return 2
assert((function() function foo() local a = 1 local function f() return a end a = 2 return f end return foo()() end)() == 2)

-- this checks upval mutability - we change the value from a context where it's upval
assert((function() function foo() local a = 1 (function () a = 2 end)() return a end return foo() end)() == 2)

-- check self capture: does self go into any upvalues?
assert((function() local t = {f=5} function t:get() return (function() return self.f end)() end return t:get() end)() == 5)

-- check self capture & close: is self copied to upval?
assert((function() function foo() local t = {f=5} function t:get() return function() return self.f end end return t:get() end return foo()() end)() == 5)

-- if
assert((function() local a = 1 if a then a = 2 end return a end)() == 2)
assert((function() local a if a then a = 2 end return a end)() == nil)

assert((function() local a = 0 if a then a = 1 else a = 2 end return a end)() == 1)
assert((function() local a if a then a = 1 else a = 2 end return a end)() == 2)

-- binary ops
assert((function() local a = 1 a = a + 2 return a end)() == 3)
assert((function() local a = 1 a = a - 2 return a end)() == -1)
assert((function() local a = 1 a = a * 2 return a end)() == 2)
assert((function() local a = 1 a = a / 2 return a end)() == 0.5)
assert((function() local a = 5 a = a % 2 return a end)() == 1)
assert((function() local a = 3 a = a ^ 2 return a end)() == 9)

assert((function() local a = '1' a = a .. '2' return a end)() == "12")
assert((function() local a = '1' a = a .. '2' .. '3' return a end)() == "123")

assert(concat(pcall(function() return '1' .. nil .. '2' end)):match("^false,.*attempt to concatenate nil with string"))

assert((function() local a = 1 a = a == 2 return a end)() == false)
assert((function() local a = 1 a = a ~= 2 return a end)() == true)
assert((function() local a = 1 a = a < 2 return a end)() == true)
assert((function() local a = 1 a = a <= 2 return a end)() == true)
assert((function() local a = 1 a = a > 2 return a end)() == false)
assert((function() local a = 1 a = a >= 2 return a end)() == false)

assert((function() local a = 1 a = a and 2 return a end)() == 2)
assert((function() local a = nil a = a and 2 return a end)() == nil)
assert((function() local a = 1 a = a or 2 return a end)() == 1)
assert((function() local a = nil a = a or 2 return a end)() == 2)

-- binary arithmetics coerces strings to numbers (sadly)
assert(1 + "2" == 3)
assert(2 * "0xa" == 20)

-- unary ops
assert((function() local a = true a = not a return a end)() == false)
assert((function() local a = false a = not a return a end)() == true)
assert((function() local a = nil a = not a return a end)() == true)

assert((function() return #_G end)() == 0)
assert((function() return #{1,2} end)() == 2)
assert((function() return #'g' end)() == 1)

assert((function() local a = 1 a = -a return a end)() == -1)

-- __len metamethod
assert((function() local ud = newproxy(true) getmetatable(ud).__len = function() return 42 end return #ud end)() == 42)
assert((function() local t = {} setmetatable(t, { __len = function() return 42 end }) return #t end)() == 42)

-- while/repeat
assert((function() local a = 10 local b = 1 while a > 1 do b = b * 2 a = a - 1 end return b end)() == 512)
assert((function() local a = 10 local b = 1 repeat b = b * 2 a = a - 1 until a == 1 return b end)() == 512)

assert((function() local a = 10 local b = 1 while true do b = b * 2 a = a - 1 if a == 1 then break end end return b end)() == 512)
assert((function() local a = 10 local b = 1 while true do b = b * 2 a = a - 1 if a == 1 then break else end end return b end)() == 512)
assert((function() local a = 10 local b = 1 repeat b = b * 2 a = a - 1 if a == 1 then break end until false return b end)() == 512)
assert((function() local a = 10 local b = 1 repeat b = b * 2 a = a - 1 if a == 1 then break else end until false return b end)() == 512)

-- this makes sure a - 4 doesn't clobber a (which would happen if the lifetime of locals inside the repeat..until block is contained within
-- the block and ends before the condition is evaluated
assert((function() repeat local a = 5 until a - 4 < 0 or a - 4 >= 0 end)() == nil)

-- numeric for
-- basic tests with positive/negative step sizes
assert((function() local a = 1 for b=1,9 do a = a * 2 end return a end)() == 512)
assert((function() local a = 1 for b=1,9,2 do a = a * 2 end return a end)() == 32)
assert((function() local a = 1 for b=1,9,-2 do a = a * 2 end return a end)() == 1)
assert((function() local a = 1 for b=9,1,-2 do a = a * 2 end return a end)() == 32)

-- make sure break works
assert((function() local a = 1 for b=1,9 do a = a * 2 if a == 128 then break end end return a end)() == 128)
assert((function() local a = 1 for b=1,9 do a = a * 2 if a == 128 then break else end end return a end)() == 128)

-- make sure internal index is protected against modification
assert((function() local a = 1 for b=9,1,-2 do a = a * 2 b = nil end return a end)() == 32)

-- generic for
-- ipairs
assert((function() local a = '' for k in ipairs({5, 6, 7}) do a = a .. k end return a end)() == "123")
assert((function() local a = '' for k,v in ipairs({5, 6, 7}) do a = a .. k end return a end)() == "123")
assert((function() local a = '' for k,v in ipairs({5, 6, 7}) do a = a .. v end return a end)() == "567")

-- ipairs with gaps
assert((function() local a = '' for k in ipairs({5, 6, 7, nil, 8}) do a = a .. k end return a end)() == "123")
assert((function() local a = '' for k,v in ipairs({5, 6, 7, nil, 8}) do a = a .. k end return a end)() == "123")
assert((function() local a = '' for k,v in ipairs({5, 6, 7, nil, 8}) do a = a .. v end return a end)() == "567")

-- manual ipairs/inext
local inext = ipairs({5,6,7})
assert(concat(inext({5,6,7}, 2)) == "3,7")

-- pairs on array
assert((function() local a = '' for k in pairs({5, 6, 7}) do a = a .. k end return a end)() == "123")
assert((function() local a = '' for k,v in pairs({5, 6, 7}) do a = a .. k end return a end)() == "123")
assert((function() local a = '' for k,v in pairs({5, 6, 7}) do a = a .. v end return a end)() == "567")

-- pairs on array with gaps
assert((function() local a = '' for k in pairs({5, 6, 7, nil, 8}) do a = a .. k end return a end)() == "1235")
assert((function() local a = '' for k,v in pairs({5, 6, 7, nil, 8}) do a = a .. k end return a end)() == "1235")
assert((function() local a = '' for k,v in pairs({5, 6, 7, nil, 8}) do a = a .. v end return a end)() == "5678")

-- pairs on table
assert((function() local a = {} for k in pairs({a=1, b=2, c=3}) do a[k] = 1 end return a.a + a.b + a.c end)() == 3)
assert((function() local a = {} for k,v in pairs({a=1, b=2, c=3}) do a[k] = 1 end return a.a + a.b + a.c end)() == 3)
assert((function() local a = {} for k,v in pairs({a=1, b=2, c=3}) do a[k] = v end return a.a + a.b + a.c end)() == 6)

-- pairs on mixed array/table + gaps in the array portion
-- note that a,b,c results in a,c,b during traversal since index is based on hash & size
assert((function() local a = {} for k,v in pairs({1, 2, 3, a=5, b=6, c=7}) do a[#a+1] = v end return table.concat(a, ',') end)() == "1,2,3,5,7,6")
assert((function() local a = {} for k,v in pairs({1, 2, 3, nil, 4, a=5, b=6, c=7}) do a[#a+1] = v end return table.concat(a, ',') end)() == "1,2,3,4,5,7,6")

-- pairs manually
assert((function() local a = '' for k in next,{5, 6, 7} do a = a .. k end return a end)() == "123")
assert((function() local a = '' for k,v in next,{5, 6, 7} do a = a .. k end return a end)() == "123")
assert((function() local a = '' for k,v in next,{5, 6, 7} do a = a .. v end return a end)() == "567")
assert((function() local a = {} for k in next,{a=1, b=2, c=3} do a[k] = 1 end return a.a + a.b + a.c end)() == 3)
assert((function() local a = {} for k,v in next,{a=1, b=2, c=3} do a[k] = 1 end return a.a + a.b + a.c end)() == 3)
assert((function() local a = {} for k,v in next,{a=1, b=2, c=3} do a[k] = v end return a.a + a.b + a.c end)() == 6)

-- too many vars
assert((function() local a = '' for k,v,p in pairs({a=1, b=2, c=3}) do a = a .. tostring(p) end return a end)() == "nilnilnil")

-- make sure break works
assert((function() local a = 1 for _ in pairs({1,2,3}) do a = a * 2 if a == 4 then break end end return a end)() == 4)
assert((function() local a = 1 for _ in pairs({1,2,3}) do a = a * 2 if a == 4 then break else end end return a end)() == 4)

-- make sure internal index is protected against modification
assert((function() local a = 1 for b in pairs({1,2,3}) do a = a * 2 b = nil end return a end)() == 8)

-- make sure custom iterators work! example is from PIL 7.1
function list_iter(t)
    local i = 0
    local n = table.getn(t)
    return function()
        i = i + 1
        if i <= n then return t[i] end
    end
end

assert((function() local a = '' for e in list_iter({4,2,1}) do a = a .. e end return a end)() == "421")

-- make sure multret works in context of pairs() - this is a very painful to handle combination due to complex internal details
assert((function() local function f() return {5,6,7},8,9,0 end local a = '' for k,v in ipairs(f()) do a = a .. v end return a end)() == "567")

-- table literals
-- basic tests
assert((function() local t = {} return #t end)() == 0)

assert((function() local t = {1, 2} return #t end)() == 2)
assert((function() local t = {1, 2} return t[1] + t[2] end)() == 3)

assert((function() local t = {data = 4} return t.data end)() == 4)
assert((function() local t = {[1+2] = 4} return t[3] end)() == 4)

assert((function() local t = {data = 4, [1+2] = 5} return t.data + t[3] end)() == 9)

assert((function() local t = {[1] = 1, [2] = 2} return t[1] + t[2] end)() == 3)

-- since table ctor is chunked in groups of 16, we should be careful with edge cases around that
assert((function() return table.concat({}, ',') end)() == "")
assert((function() return table.concat({1}, ',') end)() == "1")
assert((function() return table.concat({1,2}, ',') end)() == "1,2")
assert((function() return table.concat({1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15}, ',') end)() == 
    "1,2,3,4,5,6,7,8,9,10,11,12,13,14,15")
assert((function() return table.concat({1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16}, ',') end)() == "1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16")
assert((function() return table.concat({1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17}, ',') end)() == "1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17")

-- some scripts rely on exact table traversal order; while it's evil to do so, let's check that it works
assert((function()
    local kSelectedBiomes = {
        ['Mountains'] = true,
        ['Canyons'] = true,
        ['Dunes'] = true,
        ['Arctic'] = true,
        ['Lavaflow'] = true,
        ['Hills'] = true,
        ['Plains'] = true,
        ['Marsh'] = true,
        ['Water'] = true,
    }
    local result = ""
    for k in pairs(kSelectedBiomes) do result = result .. k end
    return result
end)() == "ArcticDunesCanyonsWaterMountainsHillsLavaflowPlainsMarsh")

-- multiple returns
-- local=
assert((function() function foo() return 2, 3, 4 end local a, b, c = foo() return ''..a..b..c end)() == "234")
assert((function() function foo() return 2, 3, 4 end local a, b, c = 1, foo() return ''..a..b..c end)() == "123")
assert((function() function foo() return 2 end local a, b, c = 1, foo() return ''..a..b..tostring(c) end)() == "12nil")

-- assignments
assert((function() function foo() return 2, 3 end a, b, c, d = 1, foo() return ''..a..b..c..tostring(d) end)() == "123nil")
assert((function() function foo() return 2, 3 end local a, b, c, d a, b, c, d = 1, foo() return ''..a..b..c..tostring(d) end)() == "123nil")

-- varargs
-- local=
assert((function() function foo(...) local a, b, c = ... return a + b + c end return foo(1, 2, 3) end)() == 6)
assert((function() function foo(x, ...) local a, b, c = ... return a + b + c end return foo(1, 2, 3, 4) end)() == 9)

-- assignments
assert((function() function foo(...) a, b, c = ... return a + b + c end return foo(1, 2, 3) end)() == 6)
assert((function() function foo(x, ...) a, b, c = ... return a + b + c end return foo(1, 2, 3, 4) end)() == 9)

-- extra nils
assert((function() function foo(...) local a, b, c = ... return tostring(a) .. tostring(b) .. tostring(c) end return foo(1, 2) end)() == "12nil")

-- varargs + multiple returns
-- return
assert((function() function foo(...) return ... end return concat(foo(1, 2, 3)) end)() == "1,2,3")
assert((function() function foo(...) return ... end return foo() end)() == nil)
assert((function() function foo(a, ...) return a + 10, ... end return concat(foo(1, 2, 3)) end)() == "11,2,3")

-- call
assert((function() function foo(...) return ... end function bar(...) return foo(...) end return concat(bar(1, 2, 3)) end)() == "1,2,3")
assert((function() function foo(...) return ... end function bar(...) return foo(...) end return bar() end)() == nil)
assert((function() function foo(a, ...) return a + 10, ... end function bar(a, ...) return foo(a * 2, ...) end return concat(bar(1, 2, 3)) end)() == "12,2,3")

-- manual pack
assert((function() function pack(first, ...) if not first then return {} end local t = pack(...) table.insert(t, 1, first) return t end function foo(...) return pack(...) end return #foo(0, 1, 2) end)() == 3)

-- multret + table literals
-- basic tests
assert((function() function foo(...) return { ... } end return #(foo()) end)() == 0)
assert((function() function foo(...) return { ... } end return #(foo(1, 2, 3)) end)() == 3)
assert((function() function foo() return 1, 2, 3 end return #({foo()}) end)() == 3)

-- since table ctor is chunked in groups of 16, we should be careful with edge cases around that
assert((function() function foo() return 1, 2, 3 end return table.concat({foo()}, ',') end)() == "1,2,3")
assert((function() function foo() return 1, 2, 3 end return table.concat({1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, foo()}, ',') end)() == "1,2,3,4,5,6,7,8,9,10,11,12,13,14,1,2,3")
assert((function() function foo() return 1, 2, 3 end return table.concat({1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, foo()}, ',') end)() == "1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,1,2,3")
assert((function() function foo() return 1, 2, 3 end return table.concat({1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, foo()}, ',') end)() == "1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,1,2,3")
assert((function() function foo() return 1, 2, 3 end return table.concat({1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, foo()}, ',') end)() == "1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,1,2,3")

-- table access
assert((function() local t = {6, 9, 7} return t[2] end)() == 9)
assert((function() local t = {6, 9, 7} return t[0] end)() == nil)
assert((function() local t = {6, 9, 7} return t[4] end)() == nil)
assert((function() local t = {6, 9, 7} return t[4.5] end)() == nil)
assert((function() local t = {6, 9, 7, [4.5]=11} return t[4.5] end)() == 11)
assert((function() local t = {6, 9, 7, a=11} return t['a'] end)() == 11)
assert((function() local t = {6, 9, 7} setmetatable(t, { __index = function(t,i) return i * 10 end }) return concat(t[2],t[5]) end)() == "9,50")

assert((function() local t = {6, 9, 7} t[2] = 10 return t[2] end)() == 10)
assert((function() local t = {6, 9, 7} t[0] = 5 return t[0] end)() == 5)
assert((function() local t = {6, 9, 7} t[4] = 10 return t[4] end)() == 10)
assert((function() local t = {6, 9, 7} t[4.5] = 10 return t[4.5] end)() == 10)
assert((function() local t = {6, 9, 7} t['a'] = 11 return t['a'] end)() == 11)
assert((function() local t = {6, 9, 7} setmetatable(t, { __newindex = function(t,i,v) rawset(t, i * 10, v) end }) t[1] = 17 t[5] = 1 return concat(t[1],t[5],t[50]) end)() == "17,nil,1")

-- userdata access
assert((function() local ud = newproxy(true) getmetatable(ud).__index = function(ud,i) return i * 10 end return ud[2] end)() == 20)
assert((function() local ud = newproxy(true) getmetatable(ud).__index = function() return function(self, i) return i * 10 end end return ud:meow(2) end)() == 20)

-- and/or
-- rhs is a constant
assert((function() local a = 1 a = a and 2 return a end)() == 2)
assert((function() local a = nil a = a and 2 return a end)() == nil)
assert((function() local a = 1 a = a or 2 return a end)() == 1)
assert((function() local a = nil a = a or 2 return a end)() == 2)

-- rhs is a local
assert((function() local a = 1 local b = 2 a = a and b return a end)() == 2)
assert((function() local a = nil local b = 2 a = a and b return a end)() == nil)
assert((function() local a = 1 local b = 2 a = a or b return a end)() == 1)
assert((function() local a = nil local b = 2 a = a or b return a end)() == 2)

-- rhs is a global (prevents optimizations)
assert((function() local a = 1 b = 2 a = a and b return a end)() == 2)
assert((function() local a = nil b = 2 a = a and b return a end)() == nil)
assert((function() local a = 1 b = 2 a = a or b return a end)() == 1)
assert((function() local a = nil b = 2 a = a or b return a end)() == 2)

-- table access: method calls + fake oop via mt
assert((function()
    local Class = {}
    Class.__index = Class

    function Class.new()
        local self = {}
        setmetatable(self, Class)

        self.field = 5

        return self
    end

    function Class:GetField()
        return self.field
    end

    local object = Class.new()
    return object:GetField()
end)() == 5)

-- table access: evil indexer
assert((function()
    local a = {5}
    local b = {6}
    local mt = { __index = function() return b[1] end }
    setmetatable(a, mt)
    b = a.hi
    return b
end)() == 6)

-- table access:  fast-path tests for array lookup
-- in-bounds array lookup shouldn't call into Lua, but if the element isn't there we'll still call the metatable
assert((function() local a = {9, [1.5] = 7} return a[1], a[2], a[1.5] end)() == 9,nil,7)
assert((function() local a = {9, [1.5] = 7} setmetatable(a, { __index = function() return 5 end }) return concat(a[1],a[2],a[1.5]) end)() == "9,5,7")
assert((function() local a = {9, nil, 11} setmetatable(a, { __index = function() return 5 end }) return concat(a[1],a[2],a[3],a[4]) end)() == "9,5,11,5")

-- namecall for userdata: technically not officially supported but hard to test in a different way!
-- warning: this test may break at any time as we may decide that we'll only use userdata-namecall on tagged user data objects
assert((function()
    local obj = newproxy(true)
    getmetatable(obj).__namecall = function(self, arg) return 42 + arg end
    return obj:Foo(10)
end)() == 52)
assert((function()
    local obj = newproxy(true)
    local t = {}
    setmetatable(t, { __call = function(self1, self2, arg) return 42 + arg end })
    getmetatable(obj).__namecall = t
    return obj:Foo(10)
end)() == 52)

-- namecall for oop to test fast paths
assert((function()
    local Class = {}
    Class.__index = Class

    function Class:new(klass, v) -- note, this isn't necessarily common but it exercises additional namecall paths
        local self = {value = v}
        setmetatable(self, Class)
        return self
    end

    function Class:get()
        return self.value
    end

    function Class:set(v)
        self.value = v
    end

    local n = Class:new(32)
    n:set(42)
    return n:get()
end)() == 42)

-- comparison
-- basic types
assert((function() a = nil return concat(a == nil, a ~= nil) end)() == "true,false")
assert((function() a = nil return concat(a == 1, a ~= 1) end)() == "false,true")
assert((function() a = 1 return concat(a == 1, a ~= 1) end)() == "true,false")
assert((function() a = 1 return concat(a == 2, a ~= 2) end)() == "false,true")
assert((function() a = true return concat(a == true, a ~= true) end)() == "true,false")
assert((function() a = true return concat(a == false, a ~= false) end)() == "false,true")
assert((function() a = 'a' return concat(a == 'a', a ~= 'a') end)() == "true,false")
assert((function() a = 'a' return concat(a == 'b', a ~= 'b') end)() == "false,true")

-- tables, reference equality (no mt)
assert((function() a = {} return concat(a == a, a ~= a) end)() == "true,false")
assert((function() a = {} b = {} return concat(a == b, a ~= b) end)() == "false,true")

-- tables, reference equality (mt without __eq)
assert((function() a = {} setmetatable(a, {}) return concat(a == a, a ~= a) end)() == "true,false")
assert((function() a = {} b = {} mt = {} setmetatable(a, mt) setmetatable(b, mt) return concat(a == b, a ~= b) end)() == "false,true")

-- tables, __eq with same mt/different mt but same function/different function
assert((function() a = {} b = {} mt = { __eq = function(l, r) return #l == #r end } setmetatable(a, mt) setmetatable(b, mt) return concat(a == b, a ~= b) end)() == "true,false")
assert((function() a = {} b = {} function eq(l, r) return #l == #r end setmetatable(a, {__eq = eq}) setmetatable(b, {__eq = eq}) return concat(a == b, a ~= b) end)() == "true,false")
assert((function() a = {} b = {} setmetatable(a, {__eq = function(l, r) return #l == #r end}) setmetatable(b, {__eq = function(l, r) return #l == #r end}) return concat(a == b, a ~= b) end)() == "false,true")

-- userdata, reference equality (no mt or mt.__eq)
assert((function() a = newproxy() return concat(a == newproxy(),a ~= newproxy()) end)() == "false,true")
assert((function() a = newproxy(true) return concat(a == newproxy(true),a ~= newproxy(true)) end)() == "false,true")

-- rawequal
assert(rawequal(true, 5) == false)
assert(rawequal(nil, nil) == true)
assert(rawequal(true, false) == false)
assert(rawequal(true, true) == true)
assert(rawequal(0, -0) == true)
assert(rawequal(1, 2) == false)
assert(rawequal("a", "a") == true)
assert(rawequal("a", "b") == false)
assert((function() a = {} b = {} mt = { __eq = function(l, r) return #l == #r end } setmetatable(a, mt) setmetatable(b, mt) return concat(a == b, rawequal(a, b)) end)() == "true,false")

-- metatable ops
local function vec3t(x, y, z)
    return setmetatable({x=x, y=y, z=z}, {
        __add = function(l, r) return vec3t(l.x + r.x, l.y + r.y, l.z + r.z) end,
        __sub = function(l, r) return vec3t(l.x - r.x, l.y - r.y, l.z - r.z) end,
        __mul = function(l, r) return type(r) == "number" and vec3t(l.x * r, l.y * r, l.z * r) or vec3t(l.x * r.x, l.y * r.y, l.z * r.z) end,
        __div = function(l, r) return type(r) == "number" and vec3t(l.x / r, l.y / r, l.z / r) or vec3t(l.x / r.x, l.y / r.y, l.z / r.z) end,
        __unm = function(v) return vec3t(-v.x, -v.y, -v.z) end,
        __tostring = function(v) return string.format("%g, %g, %g", v.x, v.y, v.z) end
    })
end

-- reg vs reg
assert((function() return tostring(vec3t(1,2,3) + vec3t(4,5,6)) end)() == "5, 7, 9")
assert((function() return tostring(vec3t(1,2,3) - vec3t(4,5,6)) end)() == "-3, -3, -3")
assert((function() return tostring(vec3t(1,2,3) * vec3t(4,5,6)) end)() == "4, 10, 18")
assert((function() return tostring(vec3t(1,2,3) / vec3t(2,4,8)) end)() == "0.5, 0.5, 0.375")

-- reg vs constant
assert((function() return tostring(vec3t(1,2,3) * 2) end)() == "2, 4, 6")
assert((function() return tostring(vec3t(1,2,3) / 2) end)() == "0.5, 1, 1.5")

-- unary
assert((function() return tostring(-vec3t(1,2,3)) end)() == "-1, -2, -3")

-- string comparison
assert((function() function cmp(a,b) return a<b,a<=b,a>b,a>=b end return concat(cmp('a', 'b')) end)() == "true,true,false,false")
assert((function() function cmp(a,b) return a<b,a<=b,a>b,a>=b end return concat(cmp('a', 'a')) end)() == "false,true,false,true")
assert((function() function cmp(a,b) return a<b,a<=b,a>b,a>=b end return concat(cmp('a', '')) end)() == "false,false,true,true")
assert((function() function cmp(a,b) return a<b,a<=b,a>b,a>=b end return concat(cmp('', '\\0')) end)() == "true,true,false,false")
assert((function() function cmp(a,b) return a<b,a<=b,a>b,a>=b end return concat(cmp('abc', 'abd')) end)() == "true,true,false,false")
assert((function() function cmp(a,b) return a<b,a<=b,a>b,a>=b end return concat(cmp('ab\\0c', 'ab\\0d')) end)() == "true,true,false,false")
assert((function() function cmp(a,b) return a<b,a<=b,a>b,a>=b end return concat(cmp('ab\\0c', 'ab\\0')) end)() == "false,false,true,true")

-- array access
assert((function() local a = {4,5,6} return a[3] end)() == 6)
assert((function() local a = {4,5,nil,6} return a[3] end)() == nil)
assert((function() local a = {4,5,nil,6} setmetatable(a, { __index = function() return 42 end }) return a[4] end)() == 6)
assert((function() local a = {4,5,nil,6} setmetatable(a, { __index = function() return 42 end }) return a[3] end)() == 42)
assert((function() local a = {4,5,6} a[3] = 8 return a[3] end)() == 8)
assert((function() local a = {4,5,nil,6} a[3] = 8 return a[3] end)() == 8)
assert((function() local a = {4,5,nil,6} setmetatable(a, { __newindex = function(t,i) rawset(t,i,42) end }) a[4] = 0 return a[4] end)() == 0)
assert((function() local a = {4,5,nil,6} setmetatable(a, { __newindex = function(t,i) rawset(t,i,42) end }) a[3] = 0 return a[3] end)() == 42)

-- array index for literal
assert((function() local a = {4, 5, nil, 6} return concat(a[1], a[3], a[4], a[100]) end)() == "4,nil,6,nil")
assert((function() local a = {4, 5, nil, 6} a[1] = 42 a[3] = 0 a[100] = 75 return concat(a[1], a[3], a[75], a[100]) end)() == "42,0,nil,75")

-- load error
assert((function() return concat(loadstring('hello world')) end)() == "nil,[string \"hello world\"]:1: Incomplete statement: expected assignment or a function call")

-- many arguments & locals
function f(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10,
    p11, p12, p13, p14, p15, p16, p17, p18, p19, p20,
    p21, p22, p23, p24, p25, p26, p27, p28, p29, p30,
    p31, p32, p33, p34, p35, p36, p37, p38, p39, p40,
    p41, p42, p43, p44, p45, p46, p48, p49, p50, ...)
    local a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14
end

assert(f() == nil)

-- upvalues & loops (validates timely closing)
assert((function()
    local res = {}

    for i=1,5 do
        res[#res+1] = (function() return i end)
    end

    local sum = 0
    for i,f in pairs(res) do sum = sum + f() end

    return sum
end)() == 15)

assert((function()
    local res = {}

    for i in ipairs({1,2,3,4,5}) do
        res[#res+1] =(function() return i end)
    end

    local sum = 0
    for i,f in pairs(res) do sum = sum + f() end

    return sum
end)() == 15)

assert((function()
    local res = {}

    local i = 0
    while i <= 5 do
        local j = i
        res[#res+1] = (function() return j end)
        i = i + 1
    end

    local sum = 0
    for i,f in pairs(res) do sum = sum + f() end

    return sum
end)() == 15)

assert((function()
    local res = {}

    local i = 0
    repeat
        local j = i
        res[#res+1] = (function() return j end)
        i = i + 1
    until i > 5

    local sum = 0
    for i,f in pairs(res) do sum = sum + f() end

    return sum
end)() == 15)

-- upvalues & loops & break!
assert((function()
    local res = {}

    for i=1,10 do
        res[#res+1] = (function() return i end)
        if i == 5 then
            break
        end
    end

    local sum = 0
    for i,f in pairs(res) do sum = sum + f() end

    return sum
end)() == 15)

assert((function()
    local res = {}

    for i in ipairs({1,2,3,4,5,6,7,8,9,10}) do
        res[#res+1] =(function() return i end)
        if i == 5 then
            break
        end
    end

    local sum = 0
    for i,f in pairs(res) do sum = sum + f() end

    return sum
end)() == 15)

assert((function()
    local res = {}

    local i = 0
    while i < 10 do
        local j = i
        res[#res+1] = (function() return j end)
        if i == 5 then
            break
        end
        i = i + 1
    end

    local sum = 0
    for i,f in pairs(res) do sum = sum + f() end

    return sum
end)() == 15)

assert((function()
    local res = {}

    local i = 0
    repeat
        local j = i
        res[#res+1] = (function() return j end)
        if i == 5 then
            break
        end
        i = i + 1
    until i >= 10

    local sum = 0
    for i,f in pairs(res) do sum = sum + f() end

    return sum
end)() == 15)

-- the reason why this test is interesting is that the table created here has arraysize=0 and a single hash element with key = 1.0
-- ipairs must iterate through that
assert((function()
    local arr = { [1] = 42 }
    local sum = 0
    for i,v in ipairs(arr) do
    sum = sum + v
    end
    return sum
end)() == 42)

-- the reason why this test is interesting is it ensures we do correct mutability analysis for locals
local function chainTest(n)
     local first = nil
     local last = nil

     -- Build chain of n equality constraints
     for i = 0, n do
            local name = "v" .. i;
            if i == 0 then first = name end
            if i == n then last = name end
     end

     return concat(first, last)
end

assert(chainTest(100) == "v0,v100")

-- this validates import fallbacks
assert(pcall(function() return idontexist.a end) == false)

-- make sure that NaN is preserved by the bytecode compiler
local realnan = tostring(math.abs(0)/math.abs(0))
assert(tostring(0/0*0) == realnan)
assert(tostring((-1)^(1/2)) == realnan)

-- make sure that negative zero is preserved by bytecode compiler
assert(tostring(0) == "0")
assert(tostring(-0) == "-0")

-- test newline handling in long strings
assert((function() 
    local s1 = [[
]]
    local s2 = [[

]]
     local s3 = [[
foo
bar]]
     local s4 = [[
foo
bar
]]
    return concat(s1,s2,s3,s4)
end)() == ",\n,foo\nbar,foo\nbar\n")

-- fastcall
-- positive tests for all simple examples; note that in this case the call is a multret call (nresults=LUA_MULTRET)
assert((function() return math.abs(-5) end)() == 5)
assert((function() local abs = math.abs return abs(-5) end)() == 5)
assert((function() local abs = math.abs function foo() return abs(-5) end return foo() end)() == 5)

-- vararg testing - in this case nparams = LUA_MULTRET and it gets adjusted before execution
assert((function() function foo(...) return math.abs(...) end return foo(-5) end)() == 5)
assert((function() function foo(...) local abs = math.abs return abs(...) end return foo(-5) end)() == 5)
assert((function() local abs = math.abs function foo(...) return abs(...) end return foo(-5) end)() == 5)

-- NOTE: getfenv breaks fastcalls for the remainder of the source! hence why this is delayed until the end
function testgetfenv()
    getfenv()

    -- declare constant so that at O2 this test doesn't interfere with constant folding which we can't deoptimize
    local negfive negfive = -5

    -- getfenv breaks fastcalls (we assume we can't rely on knowing the semantics), but behavior shouldn't change
    assert((function() return math.abs(negfive) end)() == 5)
    assert((function() local abs = math.abs return abs(negfive) end)() == 5)
    assert((function() local abs = math.abs function foo() return abs(negfive) end return foo() end)() == 5)

    -- ... unless you actually reassign the function :D
    getfenv().math = { abs = function(n) return n*n end }
    assert((function() return math.abs(negfive) end)() == 25)
    assert((function() local abs = math.abs return abs(negfive) end)() == 25)
    assert((function() local abs = math.abs function foo() return abs(negfive) end return foo() end)() == 25)
end

-- you need to have enough arguments and arguments of the right type; if you don't, we'll fallback to the regular code. This checks coercions
-- first to make sure all fallback paths work
assert((function() return math.abs('-5') end)() == 5)
assert((function() local abs = math.abs return abs('-5') end)() == 5)
assert((function() local abs = math.abs function foo() return abs('-5') end return foo() end)() == 5)

-- if you don't have enough arguments or types are wrong, we fall back to the regular execution; this checks that the error generated is actually correct
assert(concat(pcall(function() return math.abs() end)):match("missing argument #1 to 'abs'"))
assert(concat(pcall(function() return math.abs(nil) end)):match("invalid argument #1 to 'abs'"))
assert(concat(pcall(function() return math.abs({}) end)):match("invalid argument #1 to 'abs'"))

-- very large unpack
assert(select('#', table.unpack({1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1})) == 263)

-- basic continue in for/while/repeat loops
assert((function() local a = 1 for i=1,8 do a = a + 1 if a < 5 then continue end a = a * 2 end return a end)() == 190)
assert((function() local a = 1 while a < 100 do a = a + 1 if a < 5 then continue end a = a * 2 end return a end)() == 190)
assert((function() local a = 1 repeat a = a + 1 if a < 5 then continue end a = a * 2 until a > 100 return a end)() == 190)

-- upvalues, loops, continue
assert((function()
    local res = {}

    for i=1,10 do
        res[#res+1] = (function() return i end)
        if i == 5 then
            continue
        end
        i = i * 2
    end

    local sum = 0
    for i,f in pairs(res) do sum = sum + f() end

    return sum
end)() == 105)

assert((function()
    local res = {}

    for i in ipairs({1,2,3,4,5,6,7,8,9,10}) do
        res[#res+1] =(function() return i end)
        if i == 5 then
            continue
        end
        i = i * 2
    end

    local sum = 0
    for i,f in pairs(res) do sum = sum + f() end

    return sum
end)() == 105)

assert((function()
    local res = {}

    local i = 1
    while i <= 10 do
        local j = i
        res[#res+1] = (function() return j end)
        if i == 5 then
            i = i + 1
            continue
        end
        i = i + 1
        j = j * 2
    end

    local sum = 0
    for i,f in pairs(res) do sum = sum + f() end

    return sum
end)() == 105)

assert((function()
    local res = {}

    local i = 1
    repeat
        local j = i
        res[#res+1] = (function() return j end)
        if i == 5 then
            i = i + 1
            continue
        end
        i = i + 1
        j = j * 2
    until i > 10

    local sum = 0
    for i,f in pairs(res) do sum = sum + f() end

    return sum
end)() == 105)

-- shrinking array part
assert((function()
    local t = table.create(100, 42)
    for i=1,90 do t[i] = nil end
    t[101] = 42
    local sum = 0
    for _,v in ipairs(t) do sum += v end
    for _,v in pairs(t) do sum += v end
    return sum
end)() == 462)

-- upvalues: recursive capture
assert((function() local function fact(n) return n < 1 and 1 or n * fact(n-1) end return fact(5) end)() == 120)

-- basic compound assignment
assert((function()
    local a = 1
    b = 2
    local c = { value = 3 }
    local d = { 4 }
    local e = 3
    local f = 2

    a += 5
    b -= a
    c.value *= 3
    d[1] /= b
    e %= 2
    f ^= 4

    return concat(a,b,c.value,d[1],e,f)
end)() == "6,-4,9,-1,1,16")

-- compound concat
assert((function()
    local a = 'a'

    a ..= 'b'
    a ..= 'c' .. 'd'
    a ..= 'e' .. 'f' .. a

    return a
end)() == "abcdefabcd")

-- compound assignment with side effects validates lhs is evaluated once
assert((function()
    local res = { 1, 2, 3 }
    local count = 0

    res[(function() count += 1 return count end)()] += 5
    res[(function() count += 1 return count end)()] += 6
    res[(function() count += 1 return count end)()] += 7

    return table.concat(res, ',')
end)() == "6,8,10")

-- typeof and type require an argument
assert(pcall(typeof) == false)
assert(pcall(type) == false)

-- typeof == type in absence of custom userdata
assert(concat(typeof(5), typeof(nil), typeof({}), typeof(newproxy())) == "number,nil,table,userdata")

-- type/typeof/newproxy interaction with metatables: __type doesn't work intentionally to avoid spoofing
assert((function()
    local ud = newproxy(true)
    getmetatable(ud).__type = "number"

    return concat(type(ud),typeof(ud))
end)() == "userdata,userdata")

testgetfenv() -- DONT MOVE THIS LINE

return 'OK'
