---@param returnString boolean
---@return string|number
local function stringOrNumber(returnString)
    return returnString and "someString" or 1
end

---@param n number
function wantsNumber(n)
end

wantsNumber(<error descr="Type mismatch. Required: 'number' Found: 'number | string'">stringOrNumber(false)</error>)
wantsNumber(--[[---@type number]] stringOrNumber(false))
wantsNumber(--[[---@not string]] stringOrNumber(false))
wantsNumber(<error descr="Type mismatch. Required: 'number' Found: 'string'">--[[---@not number]] stringOrNumber(false)</error>)

wantsNumber(
        ---@type number @Single line doc comments also work as type casts
        stringOrNumber(false)
)

wantsNumber(<error descr="Type mismatch. Required: 'number' Found: 'fun(): any'">--[[---@type fun(): any]] 1</error>)

---@param arr any[]
function wantsArray(arr)
end

local aString = "aString"

wantsArray(<error descr="Type mismatch. Required: 'any[]' Found: 'string'">aString</error>)
wantsArray(--[[--- @type any[] ]] aString) -- Trailing space used to separate array ']' from the block comment ']]'.


local aNumber = 1

---@return number, string
local function multiReturn()
return 1, "a string"
end

aNumber, aString = multiReturn()
<error descr="Type mismatch. Required: 'string' Found: 'number'">aString</error>, <error descr="Type mismatch. Required: 'number' Found: 'string'">aNumber</error> = <error descr="Result 1, type mismatch. Required: 'string' Found: 'number'"><error descr="Result 2, type mismatch. Required: 'number' Found: 'string'">multiReturn()</error></error>
aString, aNumber = --[[---@type string, number]] multiReturn()
aString, <error descr="Type mismatch. Required: 'number' Found: 'string'">aNumber</error> = <error descr="Result 2, type mismatch. Required: 'number' Found: 'string'">--[[---@type string, string]] multiReturn()</error>


---@type number|nil
local numberOrNil

wantsNumber(<error descr="Type mismatch. Required: 'number' Found: 'nil | number'">numberOrNil</error>)
wantsNumber(--[[---@not nil]] numberOrNil)

---@param returnNumbers boolean
---@return number|string, number|string
local function multiReturn2(returnNumbers)
return stringOrNumber(returnNumbers), stringOrNumber(returnNumbers)
end


<error descr="Type mismatch. Required: 'number' Found: 'number | string'">aNumber</error>, <error descr="Type mismatch. Required: 'number' Found: 'number | string'">aNumber</error> = <error descr="Result 1, type mismatch. Required: 'number' Found: 'number | string'"><error descr="Result 2, type mismatch. Required: 'number' Found: 'number | string'">multiReturn2(true)</error></error>
aNumber, aNumber = --[[---@not string, string]] multiReturn2(true)
aNumber, aString = --[[---@not string, number]] multiReturn2(true)
aNumber = <error descr="Type mismatch. Required: 'number' Found: 'number | string'">multiReturn2(true)</error>
aNumber = --[[---@not string]] multiReturn2(true)

---@type number[]
local numberArray

local a = --[[---@not nil]] table.unpack(numberArray, 1, 1)
local a2, b2, c2 = --[[---@not nil, nil, nil]] table.unpack(numberArray, 1, 3)

aNumber = a
aNumber = a2
aNumber = b2
aNumber = c2

aString = <error descr="Type mismatch. Required: 'string' Found: 'number'">a</error>
aString = <error descr="Type mismatch. Required: 'string' Found: 'number'">a2</error>
aString = <error descr="Type mismatch. Required: 'string' Found: 'number'">b2</error>
aString = <error descr="Type mismatch. Required: 'string' Found: 'number'">c2</error>

---@type (number | string)[]
local stringOrNumberArray

local d, e, f = --[[---@not nil | number, nil | number, nil | number]] table.unpack(stringOrNumberArray, 1, 3)

aNumber = <error descr="Type mismatch. Required: 'number' Found: 'string'">d</error>
aNumber = <error descr="Type mismatch. Required: 'number' Found: 'string'">e</error>
aNumber = <error descr="Type mismatch. Required: 'number' Found: 'string'">f</error>

aString = d
aString = e
aString = f

---@type nil | number
local nilOrNumber

local g, h, i = --[[---@not string...]] table.unpack(stringOrNumberArray, 1, 3)

nilOrNumber = g
nilOrNumber = h
nilOrNumber = i

aNumber = <error descr="Type mismatch. Required: 'number' Found: 'nil | number'">g</error>
aNumber = <error descr="Type mismatch. Required: 'number' Found: 'nil | number'">h</error>
aNumber = <error descr="Type mismatch. Required: 'number' Found: 'nil | number'">i</error>

---@type nil | string
local nilOrString

local j, k, l = --[[---@type string...]] table.unpack(stringOrNumberArray, 1, 3)

nilOrNumber = <error descr="Type mismatch. Required: 'nil | number' Found: 'nil | string'">j</error>
nilOrNumber = <error descr="Type mismatch. Required: 'nil | number' Found: 'nil | string'">k</error>
nilOrNumber = <error descr="Type mismatch. Required: 'nil | number' Found: 'nil | string'">l</error>

nilOrString = j
nilOrString = k
nilOrString = l

---@return any
local function notNilAny()
    ---@type any[]
    local anyArray

    return --[[---@not nil]] table.unpack(anyArray, 1)
end

---@alias ANestedUnion 1 | 2
---@alias AUnionAlias ANestedUnion | 'a' | 'b' | 'c'

---@type AUnionAlias
local union

local filteredUnion = --[[---@not 'a' | 'c']] union
filteredUnion = <error descr="Type mismatch. Required: '\"b\" | ANestedUnion' Found: '\"a\"'">'a'</error>
filteredUnion = 'b'
filteredUnion = <error descr="Type mismatch. Required: '\"b\" | ANestedUnion' Found: '\"c\"'">'c'</error>
filteredUnion = 1
