---@class Emmy2
local e = { name = 'Emmy', age = 7, doit = function () end }

-- This should create a local variable
local f = {1, 2, 3, 4}

-- This should add sayHello() to the Emmy class
function e:sayHello()
    print('hi')
end

-- Another local
local a = 3

-- Global func
function globalFunc() end

-- Global func
function globalFuncWithArgs(a, b, c) end

-- local func
local function localFunc() end

-- local func
local function localFunc2()
    -- local func as a child of localFunc2
    local function localFuncNestedInLocalFunc2() end

    -- local var as a child of localFunc2
    local bobby = 6

    for i in pairs(bobby) do
        -- we don't display locals that aren't either function- or file-scope
        local jack = 6
    end

    -- Emmy class gets a new function
    e.baz = function () end

    doSomething({
        -- we don't handle function call arguments (yet)
        x = function () end
    })
end

-- new local
local d = {
    -- with new functions
    foo = function () end,
    bar = function (a, b) end
}

-- local var
local g = {}

-- new non-class A.B.C w/ function doit()
function A.B.C.doit() end

-- add doitItAgain() to A.B.C
A.B.C.doitAgain = function () end

-- look, mom.  non-fields (in non-classes).
A.B.C.var = 3

globalVar = 3
globalFunc2 = function ()
    local j = 3
end

globalTable = {
    a = function () end
}

---@class GlobalClass2
globalC2 = {}

-- Assignment test
local xyz

xyz = 3

e:doit(function ()
    local a = 3;

    local F.A.x = function () local x = 3 end
end)
