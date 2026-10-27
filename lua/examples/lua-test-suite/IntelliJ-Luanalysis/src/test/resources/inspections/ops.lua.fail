---@type number
local aNumber

---@type string
local aString

---@type boolean
local aBoolean

---@type 1
local theNumberOne

---@type string | number
local aStringOrNumber

---@type true | 1
local trueOrOne

---@type true
local trueLiteral

---@type false
local falseLiteral

---@type number[]
local numberArray

---@type {x: number}
local xShape

---@type table<number, number>
local numberNumberTable

-- or
theNumberOne = 1 or 1
theNumberOne = 1 or nil
theNumberOne = 1 or false
theNumberOne = 1 or true
theNumberOne = 1 or aBoolean
theNumberOne = nil or 1
theNumberOne = false or 1
theNumberOne = <error descr="Type mismatch. Required: '1' Found: 'true'">true or 1</error>
theNumberOne = <error descr="Type mismatch. Required: '1' Found: '1 | true'">aBoolean or 1</error>

-- and
theNumberOne = 1 and 1
theNumberOne = <error descr="Type mismatch. Required: '1' Found: 'nil'">1 and nil</error>
theNumberOne = <error descr="Type mismatch. Required: '1' Found: 'false'">1 and false</error>
theNumberOne = <error descr="Type mismatch. Required: '1' Found: 'true'">1 and true</error>
theNumberOne = <error descr="Type mismatch. Required: '1' Found: 'boolean'">1 and aBoolean</error>
theNumberOne = <error descr="Type mismatch. Required: '1' Found: 'nil'">nil and 1</error>
theNumberOne = <error descr="Type mismatch. Required: '1' Found: 'false'">false and 1</error>
theNumberOne = true and 1
theNumberOne = <error descr="Type mismatch. Required: '1' Found: '1 | false'">aBoolean and 1</error>

-- combinatorial
aStringOrNumber = aBoolean and "someString" or 1
aStringOrNumber = "someString" and aBoolean and "someString" or 1
aStringOrNumber = <error descr="Type mismatch. Required: 'number | string' Found: '1 | true'">aBoolean or "someString" and 1</error>
aString = 1 and "someString" or aBoolean
trueOrOne = aBoolean or "someString" and 1

-- arithmetic
aNumber = theNumberOne + 1
theNumberOne = <error descr="Type mismatch. Required: '1' Found: 'number'">theNumberOne + 1</error>
aNumber = <error descr="Type mismatch. Required: 'number' Found: 'number | string'">aStringOrNumber + 1</error>

aNumber = theNumberOne - 1
theNumberOne = <error descr="Type mismatch. Required: '1' Found: 'number'">theNumberOne - 1</error>
aNumber = <error descr="Type mismatch. Required: 'number' Found: 'number | string'">aStringOrNumber + 1</error>

-- getn
aNumber = #{}
aString = <error descr="Type mismatch. Required: 'string' Found: 'number'">#{}</error>

-- not
trueLiteral = not false
trueLiteral = <error descr="Type mismatch. Required: 'true' Found: 'false'">not true</error>
trueLiteral = <error descr="Type mismatch. Required: 'true' Found: 'boolean'">not aBoolean</error>
falseLiteral = not true
falseLiteral = <error descr="Type mismatch. Required: 'false' Found: 'true'">not false</error>
falseLiteral = <error descr="Type mismatch. Required: 'false' Found: 'boolean'">not aBoolean</error>

-- minus
aNumber = -theNumberOne
theNumberOne = <error descr="Type mismatch. Required: '1' Found: '-1'">-theNumberOne</error>

-- ensure table literals variance detection and inspections traverse binary and parenthetical expressions
numberArray = false or {1, 2}
numberArray = {1, 2} or false
numberArray = <error descr="Type mismatch. Required: 'number[]' Found: 'true'">true</error> or {1, 2}
numberArray = {1, 2} or true
numberArray = <error descr="Type mismatch. Required: 'false | nil | number[]' Found: 'boolean'">aBoolean</error> or {1, 2}
numberArray = {1, 2} or aBoolean

numberArray = <error descr="Type mismatch. Required: 'number[]' Found: 'false'">false</error> and {1, 2}
numberArray = {1, 2} and <error descr="Type mismatch. Required: 'number[]' Found: 'false'">false</error>
numberArray = true and {1, 2}
numberArray = {1, 2} and <error descr="Type mismatch. Required: 'number[]' Found: 'true'">true</error>
numberArray = <error descr="Type mismatch. Required: 'number[]' Found: 'false'">aBoolean</error> and {1, 2}
numberArray = {1, 2} and <error descr="Type mismatch. Required: 'number[]' Found: 'boolean'">aBoolean</error>

xShape = false or {x = 1}
xShape = {x = 1} or false
xShape = <error descr="Type mismatch. Required: '{ x: number }' Found: 'true'">true</error> or {x = 1}
xShape = {x = 1} or true
xShape = <error descr="Type mismatch. Required: 'false | nil | { x: number }' Found: 'boolean'">aBoolean</error> or {x = 1}
xShape = {x = 1} or aBoolean

xShape = <error descr="Type mismatch. Required: '{ x: number }' Found: 'false'">false</error> and {x = 1}
xShape = {x = 1} and <error descr="Type mismatch. Required: '{ x: number }' Found: 'false'">false</error>
xShape = true and {x = 1}
xShape = {x = 1} and <error descr="Type mismatch. Required: '{ x: number }' Found: 'true'">true</error>
xShape = <error descr="Type mismatch. Required: '{ x: number }' Found: 'false'">aBoolean</error> and {x = 1}
xShape = {x = 1} and <error descr="Type mismatch. Required: '{ x: number }' Found: 'boolean'">aBoolean</error>

xShape = false or <error descr="Type mismatch. Missing member: 'x' of: '{ x: number }'">{invalid = 1}</error>
xShape = <error descr="Type mismatch. Missing member: 'x' of: '{ x: number }'">{invalid = 1}</error> or false
xShape = <error descr="Type mismatch. Required: '{ x: number }' Found: 'true'">true</error> or {invalid = 1}
xShape = <error descr="Type mismatch. Missing member: 'x' of: '{ x: number }'">{invalid = 1}</error> or true
xShape = <error descr="Type mismatch. Required: 'false | nil | { x: number }' Found: 'boolean'">aBoolean</error> or <error descr="Type mismatch. Missing member: 'x' of: '{ x: number }'">{invalid = 1}</error>
xShape = <error descr="Type mismatch. Missing member: 'x' of: '{ x: number }'">{invalid = 1}</error> or aBoolean

xShape = <error descr="Type mismatch. Required: '{ x: number }' Found: 'false'">false</error> and {invalid = 1}
xShape = {invalid = 1} and <error descr="Type mismatch. Required: '{ x: number }' Found: 'false'">false</error>
xShape = true and <error descr="Type mismatch. Missing member: 'x' of: '{ x: number }'">{invalid = 1}</error>
xShape = {invalid = 1} and <error descr="Type mismatch. Required: '{ x: number }' Found: 'true'">true</error>
xShape = <error descr="Type mismatch. Required: '{ x: number }' Found: 'false'">aBoolean</error> and <error descr="Type mismatch. Missing member: 'x' of: '{ x: number }'">{invalid = 1}</error>
xShape = {invalid = 1} and <error descr="Type mismatch. Required: '{ x: number }' Found: 'boolean'">aBoolean</error>


numberArray = false and {invalid = 1} or {1, 2}
numberArray = false and {1, 2} or {<error descr="Type mismatch. Required: 'number[]' Found non-array field 'invalid'">invalid = 1</error>}
numberArray = {1, 2} and false or {<error descr="Type mismatch. Required: 'number[]' Found non-array field 'invalid'">invalid = 1</error>}
numberArray = {invalid = 1} and false or {1, 2}
numberArray = {1, 2} and {<error descr="Type mismatch. Required: 'number[]' Found non-array field 'invalid'">invalid = 1</error>} or false
numberArray = {invalid = 1} and {1, 2} or false

numberArray = true and {<error descr="Type mismatch. Required: 'number[]' Found non-array field 'invalid'">invalid = 1</error>} or {1, 2}
numberArray = true and {1, 2} or {invalid = 1}
numberArray = {1, 2} and <error descr="Type mismatch. Required: 'number[]' Found: 'true'">true</error> or {invalid = 1}
numberArray = {invalid = 1} and <error descr="Type mismatch. Required: 'number[]' Found: 'true'">true</error> or {1, 2}
numberArray = {1, 2} and {<error descr="Type mismatch. Required: 'number[]' Found non-array field 'invalid'">invalid = 1</error>} or true
numberArray = {invalid = 1} and {1, 2} or true

numberArray = nil and {invalid = 1} or {1, 2}
numberArray = nil and {1, 2} or {<error descr="Type mismatch. Required: 'number[]' Found non-array field 'invalid'">invalid = 1</error>}
numberArray = {1, 2} and nil or {<error descr="Type mismatch. Required: 'number[]' Found non-array field 'invalid'">invalid = 1</error>}
numberArray = {invalid = 1} and nil or {1, 2}
numberArray = {1, 2} and {<error descr="Type mismatch. Required: 'number[]' Found non-array field 'invalid'">invalid = 1</error>} or nil
numberArray = {invalid = 1} and {1, 2} or nil

numberArray = aBoolean and {<error descr="Type mismatch. Required: 'number[]' Found non-array field 'invalid'">invalid = 1</error>} or {1, 2}
numberArray = aBoolean and {1, 2} or {<error descr="Type mismatch. Required: 'number[]' Found non-array field 'invalid'">invalid = 1</error>}
numberArray = {1, 2} and <error descr="Type mismatch. Required: 'false | nil | number[]' Found: 'boolean'">aBoolean</error> or {<error descr="Type mismatch. Required: 'number[]' Found non-array field 'invalid'">invalid = 1</error>}
numberArray = {invalid = 1} and <error descr="Type mismatch. Required: 'false | nil | number[]' Found: 'boolean'">aBoolean</error> or {1, 2}
numberArray = {1, 2} and {<error descr="Type mismatch. Required: 'number[]' Found non-array field 'invalid'">invalid = 1</error>} or aBoolean
numberArray = {invalid = 1} and {1, 2} or aBoolean


numberArray = false or {invalid = 1} and {1, 2}
numberArray = false or {1, 2} and {<error descr="Type mismatch. Required: 'number[]' Found non-array field 'invalid'">invalid = 1</error>}
numberArray = {1, 2} or false and {invalid = 1}
numberArray = {<error descr="Type mismatch. Required: 'number[]' Found non-array field 'invalid'">invalid = 1</error>} or false and {1, 2}
numberArray = {1, 2} or {invalid = 1} and false
numberArray = {<error descr="Type mismatch. Required: 'number[]' Found non-array field 'invalid'">invalid = 1</error>} or {1, 2} and false

numberArray = <error descr="Type mismatch. Required: 'number[]' Found: 'true'">true</error> or {invalid = 1} and {1, 2}
numberArray = <error descr="Type mismatch. Required: 'number[]' Found: 'true'">true</error> or {1, 2} and {invalid = 1}
numberArray = {1, 2} or true and {invalid = 1}
numberArray = {<error descr="Type mismatch. Required: 'number[]' Found non-array field 'invalid'">invalid = 1</error>} or true and {1, 2}
numberArray = {1, 2} or {invalid = 1} and true
numberArray = {<error descr="Type mismatch. Required: 'number[]' Found non-array field 'invalid'">invalid = 1</error>} or {1, 2} and true

numberArray = nil or {invalid = 1} and {1, 2}
numberArray = nil or {1, 2} and {<error descr="Type mismatch. Required: 'number[]' Found non-array field 'invalid'">invalid = 1</error>}
numberArray = {1, 2} or nil and {invalid = 1}
numberArray = {<error descr="Type mismatch. Required: 'number[]' Found non-array field 'invalid'">invalid = 1</error>} or nil and {1, 2}
numberArray = {1, 2} or {invalid = 1} and nil
numberArray = {<error descr="Type mismatch. Required: 'number[]' Found non-array field 'invalid'">invalid = 1</error>} or {1, 2} and nil

numberArray = <error descr="Type mismatch. Required: 'false | nil | number[]' Found: 'boolean'">aBoolean</error> or {invalid = 1} and {1, 2}
numberArray = <error descr="Type mismatch. Required: 'false | nil | number[]' Found: 'boolean'">aBoolean</error> or {1, 2} and {<error descr="Type mismatch. Required: 'number[]' Found non-array field 'invalid'">invalid = 1</error>}
numberArray = {1, 2} or aBoolean and {invalid = 1}
numberArray = {<error descr="Type mismatch. Required: 'number[]' Found non-array field 'invalid'">invalid = 1</error>} or aBoolean and {1, 2}
numberArray = {1, 2} or {invalid = 1} and aBoolean
numberArray = {<error descr="Type mismatch. Required: 'number[]' Found non-array field 'invalid'">invalid = 1</error>} or {1, 2} and aBoolean


numberArray = <error descr="Type mismatch. Required: 'number[]' Found: 'false'">aBoolean</error> and ({<error descr="Type mismatch. Required: 'number[]' Found non-array field 'invalid'">invalid = 1</error>} or {1, 2})
numberArray = <error descr="Type mismatch. Required: 'number[]' Found: 'false'">aBoolean</error> and ({1, 2} or {invalid = 1})
numberArray = {1, 2} and (<error descr="Type mismatch. Required: 'false | nil | number[]' Found: 'boolean'">aBoolean</error> or {<error descr="Type mismatch. Required: 'number[]' Found non-array field 'invalid'">invalid = 1</error>})
numberArray = {invalid = 1} and (<error descr="Type mismatch. Required: 'false | nil | number[]' Found: 'boolean'">aBoolean</error> or {1, 2})
numberArray = {1, 2} and ({<error descr="Type mismatch. Required: 'number[]' Found non-array field 'invalid'">invalid = 1</error>} or aBoolean)
numberArray = {invalid = 1} and ({1, 2} or aBoolean)

-- Redundant parenthesis, but we want to ensure they're handled.
numberArray = <error descr="Type mismatch. Required: 'false | nil | number[]' Found: 'boolean'">aBoolean</error> or ({invalid = 1} and {1, 2})
numberArray = <error descr="Type mismatch. Required: 'false | nil | number[]' Found: 'boolean'">aBoolean</error> or ({1, 2} and {<error descr="Type mismatch. Required: 'number[]' Found non-array field 'invalid'">invalid = 1</error>})
numberArray = {1, 2} or (aBoolean and {invalid = 1})
numberArray = {<error descr="Type mismatch. Required: 'number[]' Found non-array field 'invalid'">invalid = 1</error>} or (aBoolean and {1, 2})
numberArray = {1, 2} or ({invalid = 1} and aBoolean)
numberArray = {<error descr="Type mismatch. Required: 'number[]' Found non-array field 'invalid'">invalid = 1</error>} or ({1, 2} and aBoolean)

numberNumberTable[1] = false or nil
numberNumberTable[1] = <error descr="Type mismatch. Required: 'nil | number' Found: 'false'">nil or false</error>

---@return number[]
local function returnTableLiteralThroughBinaryOp()
    return aBoolean and {1, 2} or {<error descr="Type mismatch. Required: 'number[]' Found non-array field 'invalid'">invalid = 1</error>}
end
