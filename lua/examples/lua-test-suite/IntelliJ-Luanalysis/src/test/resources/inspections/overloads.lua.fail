---@class ClassWithOverloads
local ClassWithOverloads = {}

--- Returns true if `tab` only contains sequential positive integer keys.
---@overload fun(tab: any[]): true
---@overload fun(tab: table): false
---@param tab table
---@return boolean
function ClassWithOverloads.isArray(tab)
    local i = 0
    for _ in pairs(tab) do
        i = i + 1
        if tab[i] == nil then return false end
    end
    return true
end

--- Returns true if `tab` only contains sequential positive integer keys.
---@overload fun(tab: any[]): true
---@overload fun(tab: table): false
---@param tab table
---@return boolean
ClassWithOverloads.isArrayFromClosure = function(tab)
    local i = 0
    for _ in pairs(tab) do
        i = i + 1
        if tab[i] == nil then return false end
    end
    return true
end

--- Returns true if `tab` only contains sequential positive integer keys.
---@overload fun(tab: any[]): true
---@overload fun(tab: table): false
---@param tab table
---@return boolean
local function isArray(tab)
    local i = 0
    for _ in pairs(tab) do
        i = i + 1
        if tab[i] == nil then return false end
    end
    return true
end

--- Returns true if `tab` only contains sequential positive integer keys.
---@overload fun(tab: any[]): true
---@overload fun(tab: table): false
---@param tab table
---@return boolean
local isArrayFromClosure = function(tab)
    local i = 0
    for _ in pairs(tab) do
        i = i + 1
        if tab[i] == nil then return false end
    end
    return true
end

---@type true
local t

---@type false
local f

t = ClassWithOverloads.isArray({1, 2, 3})
t = ClassWithOverloads.isArrayFromClosure({1, 2, 3})
t = isArray({1, 2, 3})
t = isArrayFromClosure({1, 2, 3})

f = <error descr="Type mismatch. Required: 'false' Found: 'true'">ClassWithOverloads.isArray({1, 2, 3})</error>
f = <error descr="Type mismatch. Required: 'false' Found: 'true'">ClassWithOverloads.isArrayFromClosure({1, 2, 3})</error>
f = <error descr="Type mismatch. Required: 'false' Found: 'true'">isArray({1, 2, 3})</error>
f = <error descr="Type mismatch. Required: 'false' Found: 'true'">isArrayFromClosure({1, 2, 3})</error>

f = ClassWithOverloads.isArray({one = 1, two = 2, three = 3})
f = ClassWithOverloads.isArrayFromClosure({one = 1, two = 2, three = 3})
f = isArray({one = 1, two = 2, three = 3})
f = isArrayFromClosure({one = 1, two = 2, three = 3})

t = <error descr="Type mismatch. Required: 'true' Found: 'false'">ClassWithOverloads.isArray({one = 1, two = 2, three = 3})</error>
t = <error descr="Type mismatch. Required: 'true' Found: 'false'">ClassWithOverloads.isArrayFromClosure({one = 1, two = 2, three = 3})</error>
t = <error descr="Type mismatch. Required: 'true' Found: 'false'">isArray({one = 1, two = 2, three = 3})</error>
t = <error descr="Type mismatch. Required: 'true' Found: 'false'">isArrayFromClosure({one = 1, two = 2, three = 3})</error>
