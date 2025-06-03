---@type fun(numberParam: number, vararg boolean)
local varargFunction

varargFunction(1, true, true)
varargFunction(1, true, true, <error descr="Type mismatch. Required: 'boolean' Found: '4'">4</error>, true)

---@type fun(numberParam: number, ...: boolean)
local varargFunction2

varargFunction2(1, true, true)
varargFunction2(1, true, true, <error descr="Type mismatch. Required: 'boolean' Found: '4'">4</error>, true)

---@param numberParam number
---@vararg boolean
local function varargFunction3(numberParam, ...)
end

varargFunction3(1, true, true)
varargFunction3(1, true, true, <error descr="Type mismatch. Required: 'boolean' Found: '4'">4</error>, true)

---@type fun(vararg boolean)
local varargFunction4

varargFunction4(true, true)
varargFunction4(true, true, <error descr="Type mismatch. Required: 'boolean' Found: '4'">4</error>, true)
varargFunction4(<error descr="Type mismatch. Required: 'boolean' Found: '1'">1</error>, true)

---@type fun(...: boolean)
local varargFunction5

varargFunction5(true, true)
varargFunction5(true, true, <error descr="Type mismatch. Required: 'boolean' Found: '4'">4</error>, true)
varargFunction5(<error descr="Type mismatch. Required: 'boolean' Found: '1'">1</error>, true)

---@vararg boolean
local function varargFunction6(...)
end

varargFunction6(true, true)
varargFunction6(true, true, <error descr="Type mismatch. Required: 'boolean' Found: '4'">4</error>, true)
varargFunction6(<error descr="Type mismatch. Required: 'boolean' Found: '1'">1</error>, true)

---@type number
local aNumber

---@type boolean
local aBoolean

---@type nil | number
local nilOrNumber

---@type nil | boolean
local nilOrBoolean

---@type fun(value: nil | number): void
local wantsNilOrNumber

---@type fun(value: nil | number): void
local wantsNilOrNumber

---@shape WantsNilOrNumberAndOptionallyNilOrBoolean
---@overload fun(value: nil | number): void
---@overload fun(value: nil | number, value: nil | boolean): void

---@type WantsNilOrNumberAndOptionallyNilOrBoolean
local wantsNilOrNumberAndOptionallyNilOrBoolean

---@vararg number
local function varargFunction7(...)
    nilOrNumber = ...
    nilOrNumber, nilOrNumber = ...
    <error descr="Type mismatch. Required: 'boolean | nil' Found: 'nil | number'">nilOrBoolean</error>, <error descr="Type mismatch. Required: 'boolean | nil' Found: 'nil | number'">nilOrBoolean</error> = <error descr="Result 2, type mismatch. Required: 'boolean | nil' Found: 'nil | number'"><error descr="Type mismatch. Required: 'boolean | nil' Found: 'nil | number'">...</error></error>
    <error descr="Type mismatch. Required: 'boolean | nil' Found: 'nil | number'">nilOrBoolean</error>, <error descr="Type mismatch. Required: 'boolean | nil' Found: 'nil | number'">nilOrBoolean</error> = <error descr="Result 2, type mismatch. Required: 'boolean | nil' Found: 'nil | number'"><error descr="Type mismatch. Required: 'boolean | nil' Found: 'nil | number'">...</error></error>

    local implicitNilOrNumber = ...
    wantsNilOrNumberAndOptionallyNilOrBoolean(implicitNilOrNumber)
    wantsNilOrNumberAndOptionallyNilOrBoolean(<error descr="Type mismatch. Required: 'boolean | nil' Found: 'number'. In: fun(value: nil | number, value: boolean | nil): void">...</error><error descr="Missing argument: value: boolean | nil. In: fun(value: nil | number, value: boolean | nil): void"><error descr="Missing argument: value: nil | number. In: fun(value: nil | number): void"><error descr="Missing argument: value: nil | number. In: fun(value: nil | number, value: boolean | nil): void">)</error></error></error>
    wantsNilOrNumberAndOptionallyNilOrBoolean((...))
    wantsNilOrNumberAndOptionallyNilOrBoolean(..., nilOrBoolean)
end

---@type fun<T>(index: number, vararg T): T
local genericVarargFunction

aNumber = genericVarargFunction(1, 1, 2, 3)
aNumber = <error descr="Type mismatch. Required: 'number' Found: 'boolean'">genericVarargFunction(1, true, false, true)</error>
aBoolean = <error descr="Type mismatch. Required: 'boolean' Found: '1 | 2 | 3'">genericVarargFunction(1, 1, 2, 3)</error>
aBoolean = genericVarargFunction(1, true, false, true)

----@type fun<T>(index: number, ...: T): T
local genericVarargFunction2

aNumber = genericVarargFunction2(1, 1, 2, 3)
aNumber = <error descr="Type mismatch. Required: 'number' Found: 'boolean'">genericVarargFunction2(1, true, false, true)</error>
aBoolean = <error descr="Type mismatch. Required: 'boolean' Found: '1 | 2 | 3'">genericVarargFunction2(1, 1, 2, 3)</error>
aBoolean = genericVarargFunction2(1, true, false, true)

---@generic T
---@param index number
---@vararg T
---@return T
local function genericVarargFunction3(index, ...)
    return --[[---@type T]] table.unpack({...}, index, 1)
end

aNumber = genericVarargFunction3(1, 1, 2, 3)
aNumber = <error descr="Type mismatch. Required: 'number' Found: 'boolean'">genericVarargFunction3(1, true, false, true)</error>
aBoolean = <error descr="Type mismatch. Required: 'boolean' Found: '1 | 2 | 3'">genericVarargFunction3(1, 1, 2, 3)</error>
aBoolean = genericVarargFunction3(1, true, false, true)


---@type 1
local one

---@type 3
local three

---@type table<string, string>
local stringStringTable

---@generic K, V
---@vararg table<K, V>
---@return table<K, V>
local function merge(...)
    ---@type table<K, V>
    local res

    return res
end

local mergedLiteralArr = merge({1, 2}, {3, 4})
local mergedLiteralMap = merge({a = 1, b = 2}, {c = 3, d = 4})

mergedLiteralArr[1] = one
mergedLiteralArr[1] = three
mergedLiteralArr[1] = <error descr="Type mismatch. Required: '1 | 2 | 3 | 4 | nil' Found: '5'">5</error>

mergedLiteralMap.a = one
mergedLiteralMap.a = three
mergedLiteralMap.a = <error descr="Type mismatch. Required: '1 | 2 | 3 | 4 | nil' Found: '5'">5</error>

<error descr="No such member 'e' found on type 'table<\"a\" | \"b\" | \"c\" | \"d\", 1 | 2 | 3 | 4>'">mergedLiteralMap.e</error> = one

local mergedStringStringMap = merge(stringStringTable, stringStringTable)

mergedStringStringMap.a = "a string"
mergedStringStringMap.a = <error descr="Type mismatch. Required: 'nil | string' Found: '1'">1</error>
mergedStringStringMap['a'] = "a string"
mergedStringStringMap['a'] = <error descr="Type mismatch. Required: 'nil | string' Found: '1'">1</error>


---@type fun<K, V>(...: table<K, V>): table<K, V>
local typeMerge

local typeMergedLiteralArr = typeMerge({1, 2}, {3, 4})
local typeMergedLiteralMap = typeMerge({a = 1, b = 2}, {c = 3, d = 4})

typeMergedLiteralArr[1] = one
typeMergedLiteralArr[1] = three
typeMergedLiteralArr[1] = <error descr="Type mismatch. Required: '1 | 2 | 3 | 4 | nil' Found: '5'">5</error>

typeMergedLiteralMap.a = one
typeMergedLiteralMap.a = three
typeMergedLiteralMap.a = <error descr="Type mismatch. Required: '1 | 2 | 3 | 4 | nil' Found: '5'">5</error>

<error descr="No such member 'e' found on type 'table<\"a\" | \"b\" | \"c\" | \"d\", 1 | 2 | 3 | 4>'">typeMergedLiteralMap.e</error> = one

local typeMergedStringStringMap = typeMerge(stringStringTable, stringStringTable)

typeMergedStringStringMap.a = "a string"
typeMergedStringStringMap.a = <error descr="Type mismatch. Required: 'nil | string' Found: '1'">1</error>
typeMergedStringStringMap['a'] = "a string"
typeMergedStringStringMap['a'] = <error descr="Type mismatch. Required: 'nil | string' Found: '1'">1</error>

---@overload fun<K, V>(...: table<K, V>): table<K, V>
local function overloadMerge(...)
end

local overloadMergedLiteralArr = overloadMerge({1, 2}, {3, 4})
local overloadMergedLiteralMap = overloadMerge({a = 1, b = 2}, {c = 3, d = 4})

overloadMergedLiteralArr[1 ] = one
overloadMergedLiteralArr[1] = three
overloadMergedLiteralArr[1] = <error descr="Type mismatch. Required: '1 | 2 | 3 | 4 | nil' Found: '5'">5</error>

overloadMergedLiteralMap.a = one
overloadMergedLiteralMap.a = three
overloadMergedLiteralMap.a = <error descr="Type mismatch. Required: '1 | 2 | 3 | 4 | nil' Found: '5'">5</error>

<error descr="No such member 'e' found on type 'table<\"a\" | \"b\" | \"c\" | \"d\", 1 | 2 | 3 | 4>'">overloadMergedLiteralMap.e</error> = one

local overloadMergedStringStringMap = overloadMerge(stringStringTable, stringStringTable)

overloadMergedStringStringMap.a = "a string"
overloadMergedStringStringMap.a = <error descr="Type mismatch. Required: 'nil | string' Found: '1'">1</error>
overloadMergedStringStringMap['a'] = "a string"
overloadMergedStringStringMap['a'] = <error descr="Type mismatch. Required: 'nil | string' Found: '1'">1</error>


---@generic T
---@vararg T
---@return T[]
local function varargsToArray(...)
    return {...}
end

---@class SomeClassWantingArrays
local SomeClassWantingArrays = {}

---@param a string[]
function SomeClassWantingArrays.acceptsStringArray(a)
end

---@param a number[]
function SomeClassWantingArrays.acceptsNumberArray(a)
end

---@param a string[]
---@overload fun(): boolean
local function acceptsStringArray(a)
end

---@param a number[]
---@overload fun(): boolean
local function acceptsNumberArray(a)
end

---@vararg string
local function acceptsVarString(...)
    SomeClassWantingArrays.acceptsStringArray({...})
    acceptsStringArray(varargsToArray(...))
    SomeClassWantingArrays.acceptsNumberArray({<error descr="Type mismatch. Required: 'number' Found: 'string'">...</error>})
    acceptsNumberArray(<error descr="Type mismatch. Required: 'number[]' Found: 'string[]'">varargsToArray(...)</error>)
end

---@vararg fun(a: string, b: number): number
local function acceptsFun(...)
    local a = {...}
    a[1]("a string", 1)
    a[1]("a string", <error descr="Type mismatch. Required: 'number' Found: '\"a string\"'">"a string"</error>)
end
