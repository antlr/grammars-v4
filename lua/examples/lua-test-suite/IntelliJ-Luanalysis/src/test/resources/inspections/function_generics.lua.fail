---@type any
local anything

---@type number
local anyNumber

---@type string
local anyString

---@type boolean
local anyBoolean

---@type 1
local number1

---@type 2
local number2

---@type "string1"
local string1

---@type 1|"string1"
local number1OrString1

---@generic T
---@param arg T
local function fn(arg)
    -- T = anonymous type : void (inherits from void because we can make no assumptions about it)
    local var = arg

    anyNumber = <error descr="Type mismatch. Required: 'number' Found: 'T'">var</error>
    anyString = <error descr="Type mismatch. Required: 'string' Found: 'T'">var</error>
    anything = var
end

-- T = any
fn(anything)
fn(anyNumber)
fn(number1)
fn(anyString)
fn(string1)


---@generic T : number
---@param arg T
local function fn2(arg)
    -- T = anonymous type : number
    local var = arg

    anyNumber = var
    number1 = <error descr="Type mismatch. Required: '1' Found: 'T : number'">var</error>
    anyString = <error descr="Type mismatch. Required: 'string' Found: 'T : number'">var</error>
end

-- T = number
fn2(anything)
fn2(anyNumber)
fn2(number1)
fn2(<error descr="Type mismatch. Required: 'T : number' Found: 'string'">anyString</error>)
fn2(<error descr="Type mismatch. Required: 'T : number' Found: '\"string1\"'">string1</error>)




---@generic T
---@param arg1 T
---@param arg2 T
local function fn3(arg1, arg2) end

-- T = 1|2
fn3(number1, number2)

-- T = 1|"string1"
fn3(number1, string1)



---@generic T : number
---@param arg1 T
---@param arg2 T
local function fn4(arg1, arg2)
    -- T = anonymous type : number

    anyNumber = arg1
    anyNumber = arg2
    number1 = <error descr="Type mismatch. Required: '1' Found: 'T : number'">arg1</error>
    number1 = <error descr="Type mismatch. Required: '1' Found: 'T : number'">arg2</error>

    ---@type T
    local t

    t = arg1
    t = arg2

    arg1 = arg2
    arg2 = arg1
end

-- T = 1|2
fn4(number1, number2)

-- T = number
fn4(<error descr="Type mismatch. Required: 'T : number' Found: '1'">number1</error>, <error descr="Type mismatch. Required: 'T : number' Found: '\"string1\"'">string1</error>)



---@generic T
---@param arg T
---@return T
local function fn5(arg)
    return arg
end

-- T = typeof(arg)
string1 = fn5(string1)
anyString = fn5(string1)
number1 = fn5(number1)
number2 = fn5(number2)
number1 = <error descr="Type mismatch. Required: '1' Found: '2'">fn5(number2)</error>
anyNumber = fn5(number1)



---@generic T
---@param arg1 T
---@param arg2 T
------@return T
local function fn6(arg1, arg2)
    return arg1
end

-- T = 1|2
anything = fn6(number1, number2)
anyNumber = fn6(number1, number2)
anyString = <error descr="Type mismatch. Required: 'string' Found: '1 | 2'">fn6(number1, number2)</error>
number1 = <error descr="Type mismatch. Required: '1' Found: '1 | 2'">fn6(number1, number2)</error>
number2 = <error descr="Type mismatch. Required: '2' Found: '1 | 2'">fn6(number1, number2)</error>

-- T = 1|"string1"
anyNumber = <error descr="Type mismatch. Required: 'number' Found: '\"string1\" | 1'">fn6(number1, string1)</error>
anyString = <error descr="Type mismatch. Required: 'string' Found: '\"string1\" | 1'">fn6(number1, string1)</error>
number1OrString1 = fn6(number1, string1)

-- T = number
anything = fn6(number1, anyNumber)
anyNumber = fn6(number1, anyNumber)
anyString = <error descr="Type mismatch. Required: 'string' Found: 'number'">fn6(number1, anyNumber)</error>
number1 = <error descr="Type mismatch. Required: '1' Found: 'number'">fn6(number1, anyNumber)</error>
number2 = <error descr="Type mismatch. Required: '2' Found: 'number'">fn6(number1, anyNumber)</error>



---@generic T
---@param arg1 T
---@param arg2 T
---@param arg3 table<T, T>
------@return T
local function fn7(arg1, arg2, arg3)
    return arg1
end

---@type table<number, number>
local numberNumberTable

---@type table<1, 1>
local number1Number1Table

-- T = 1
anything = fn7(number1, number1, number1Number1Table)
anyNumber = fn7(number1, number1, number1Number1Table)
anyString = <error descr="Type mismatch. Required: 'string' Found: '1'">fn7(number1, number1, number1Number1Table)</error>
number1 = fn7(number1, number1, number1Number1Table)
number2 = <error descr="Type mismatch. Required: '2' Found: '1'">fn7(number1, number1, number1Number1Table)</error>

-- T = number
anything = fn7(number1, number2, numberNumberTable)
anyNumber = fn7(number1, number2, numberNumberTable)
anyString = <error descr="Type mismatch. Required: 'string' Found: 'number'">fn7(number1, number2, numberNumberTable)</error>
number1 = <error descr="Type mismatch. Required: '1' Found: 'number'">fn7(number1, number2, numberNumberTable)</error>
number2 = <error descr="Type mismatch. Required: '2' Found: 'number'">fn7(number1, number2, numberNumberTable)</error>

---@type number[]
local numberArray

-- T = number
anything = fn7(number1, number2, numberArray)
anyNumber = fn7(number1, number2, numberArray)
anyString = <error descr="Type mismatch. Required: 'string' Found: 'number'">fn7(number1, number2, numberArray)</error>
number1 = <error descr="Type mismatch. Required: '1' Found: 'number'">fn7(number1, number2, numberArray)</error>
number2 = <error descr="Type mismatch. Required: '2' Found: 'number'">fn7(number1, number2, numberArray)</error>
anyNumber = fn7(number1, number2, {3})
number1 = <error descr="Type mismatch. Required: '1' Found: 'number'">fn7(number1, number2, {3})</error>


---@type table<string|number, string|number>
local stringOrNumberStringOrNumberTable

---@type string|number
local stringOrNumber

-- T = commonAncestor(arg1, arg2) = number|string
anything = fn7(number1, string1, stringOrNumberStringOrNumberTable)
stringOrNumber = fn7(number1, string1, stringOrNumberStringOrNumberTable)
anyNumber = <error descr="Type mismatch. Required: 'number' Found: 'number | string'">fn7(number1, number2, stringOrNumberStringOrNumberTable)</error>
anyString = <error descr="Type mismatch. Required: 'string' Found: 'number | string'">fn7(number1, number2, stringOrNumberStringOrNumberTable)</error>


---@generic K, V
---@param arg1 K
---@param arg2 V
---@param arg3 table<K, V>
---@return table<K, V>
local function fn8(arg1, arg2, arg3)
    return arg3
end

---@type table<string, number>
local stringNumberTable

-- K = string, V = number
stringOrNumber = <error descr="Type mismatch. Required: 'number | string' Found: 'table<string, number>'">fn8(anyString, number1, stringNumberTable)</error>
stringNumberTable = fn8(anyString, number1, stringNumberTable)
stringNumberTable = fn8(string1, number1, stringNumberTable)
stringNumberTable = <error descr="Type mismatch. Required: 'table<string, number>' Found: 'table<number | string, number>'">fn8(anyString, number1, <error descr="Type mismatch. Required: 'table<number | string, number>' Found: 'number[]'">numberArray</error>)</error>
stringNumberTable = <error descr="Type mismatch. Required: 'table<string, number>' Found: 'table<number, number>'">fn8(anyNumber, number1, numberArray)</error>


---@generic K : string, T : table<K, string>
---@param arg1 K
---@param arg2 T
---@return T
local function fn9(arg1, arg2)
    return arg2
end

---@type table<string, string>
local stringStringTable

-- K = string, T = table<string, string>
anyString = <error descr="Type mismatch. Required: 'string' Found: 'table<string, string>'">fn9(anyString, stringStringTable)</error>
stringStringTable = fn9(anyString, stringStringTable)
stringStringTable = <error descr="Type mismatch. Required: 'table<string, string>' Found: 'T : table<K : string, string>'">fn9(anyString, <error descr="Type mismatch. Required: 'T : table<K : string, string>' Found: 'table<string, number>'">stringNumberTable</error>)</error>
stringStringTable = fn9(string1, stringStringTable)

---@type table<"string1", string>
local string1StringTable

-- K = "string1", T = table<"string1", string>
string1StringTable = fn9(string1, string1StringTable)
string1StringTable = <error descr="Type mismatch. Required: 'table<\"string1\", string>' Found: 'table<string, string>'">fn9(string1, stringStringTable)</error>
stringStringTable = <error descr="Type mismatch. Required: 'table<string, string>' Found: 'table<\"string1\", string>'">fn9(string1, string1StringTable)</error>

---@generic T : boolean
local function fn10()
    ---@generic <error descr="Generic parameters cannot be shadowed, 'T' was previously defined on line 253">T : string</error>
    local function fn10Nested(arg)
    end
end


---@type fun<K, V>(tab: table<K, V>, func: fun(key: K, value: V))
local fn11

-- K = 1|2,
fn11({a = 1, b = 2}, function(key, value)
    anyString = key
    anyNumber = <error descr="Type mismatch. Required: 'number' Found: '\"a\" | \"b\"'">key</error>
    anyString = <error descr="Type mismatch. Required: 'string' Found: '1 | 2'">value</error>
    anyNumber = value
end)

fn11(stringNumberTable, function(key, value)
    anyString = key
    anyNumber = <error descr="Type mismatch. Required: 'number' Found: 'string'">key</error>
    anyString = <error descr="Type mismatch. Required: 'string' Found: 'number'">value</error>
    anyNumber = value
end)

fn11({a = "a", b = "b"}, function(key, value)
    anyString = key
    anyNumber = <error descr="Type mismatch. Required: 'number' Found: '\"a\" | \"b\"'">key</error>
    anyString = value
    anyNumber = <error descr="Type mismatch. Required: 'number' Found: '\"a\" | \"b\"'">value</error>
end)

fn11(stringStringTable, function(key, value)
    anyString = key
    anyNumber = <error descr="Type mismatch. Required: 'number' Found: 'string'">key</error>
    anyString = value
    anyNumber = <error descr="Type mismatch. Required: 'number' Found: 'string'">value</error>
end)

fn11({[1] = "a", [2] = "b"}, function(key, value)
    anyString = <error descr="Type mismatch. Required: 'string' Found: '1 | 2'">key</error>
    anyNumber = key
    anyString = value
    anyNumber = <error descr="Type mismatch. Required: 'number' Found: '\"a\" | \"b\"'">value</error>
end)

fn11({"a", "b"}, function(key, value)
    anyString = <error descr="Type mismatch. Required: 'string' Found: 'number'">key</error>
    anyNumber = key
    anyString = value
    anyNumber = <error descr="Type mismatch. Required: 'number' Found: '\"a\" | \"b\"'">value</error>
end)

fn11(numberArray, function(key, value)
    anyString = <error descr="Type mismatch. Required: 'string' Found: 'number'">key</error>
    anyNumber = key
    anyString = <error descr="Type mismatch. Required: 'string' Found: 'number'">value</error>
    anyNumber = value
end)


---@type 1
local one

---@type 3
local three


---@generic T
---@vararg T
---@return T
local function merge(...)
    ---@type T
    local fakeResult

    return fakeResult
end

---@type {a: string}
local looseAnonymousShape
local strictAnonymousShape = {a = 'specific string'}

looseAnonymousShape = merge(looseAnonymousShape, strictAnonymousShape)
looseAnonymousShape = merge(strictAnonymousShape, looseAnonymousShape)
strictAnonymousShape = <error descr="Type mismatch. Required: 'table' Found: '{ a: string }'">merge(looseAnonymousShape, strictAnonymousShape)</error>

stringStringTable = merge(stringStringTable, {a = 'specific string'})
stringStringTable = <error descr="Type mismatch. Required: 'table<string, string>' Found: 'table | table<string, string>'">merge(stringStringTable, strictAnonymousShape)</error>

---@generic K, V
---@param a table<K, V>
---@param b table<K, V>
---@return table<K, V>
local function mergeGenericTables(a, b)
    ---@type table<K, V>
    local fakeResult

    return fakeResult
end


---@generic K, V
---@param a table<K, V>
---@param b table<K, V>
---@return table<K, V>
local function mergeGenericTables(a, b)
    ---@type table<K, V>
    local res

    return res
end

local mergedLiteralArr = mergeGenericTables({1, 2}, {3, 4})
local mergedLiteralMap = mergeGenericTables({a = 1, b = 2}, {c = 3, d = 4})

mergedLiteralArr[1] = one
mergedLiteralArr[1] = three
mergedLiteralArr[1] = <error descr="Type mismatch. Required: '1 | 2 | 3 | 4 | nil' Found: '5'">5</error>

mergedLiteralMap.a = one
mergedLiteralMap.a = three
mergedLiteralMap.a = <error descr="Type mismatch. Required: '1 | 2 | 3 | 4 | nil' Found: '5'">5</error>

<error descr="No such member 'e' found on type 'table<\"a\" | \"b\" | \"c\" | \"d\", 1 | 2 | 3 | 4>'">mergedLiteralMap.e</error> = one

local mergedStringStringMap = mergeGenericTables(stringStringTable, stringStringTable)

mergedStringStringMap.a = "a string"
mergedStringStringMap.a = <error descr="Type mismatch. Required: 'nil | string' Found: '1'">1</error>
mergedStringStringMap['a'] = "a string"
mergedStringStringMap['a'] = <error descr="Type mismatch. Required: 'nil | string' Found: '1'">1</error>


---@type fun<K, V>(a: table<K, V>, b: table<K, V>): table<K, V>
local typeMergeGenericTables

local typeMergedLiteralArr = typeMergeGenericTables({1, 2}, {3, 4})
local typeMergedLiteralMap = typeMergeGenericTables({a = 1, b = 2}, {c = 3, d = 4})

typeMergedLiteralArr[1] = one
typeMergedLiteralArr[1] = three
typeMergedLiteralArr[1] = <error descr="Type mismatch. Required: '1 | 2 | 3 | 4 | nil' Found: '5'">5</error>

typeMergedLiteralMap.a = one
typeMergedLiteralMap.a = three
typeMergedLiteralMap.a = <error descr="Type mismatch. Required: '1 | 2 | 3 | 4 | nil' Found: '5'">5</error>

<error descr="No such member 'e' found on type 'table<\"a\" | \"b\" | \"c\" | \"d\", 1 | 2 | 3 | 4>'">typeMergedLiteralMap.e</error> = one

local typeMergedStringStringMap = typeMergeGenericTables(stringStringTable, stringStringTable)

typeMergedStringStringMap.a = "a string"
typeMergedStringStringMap.a = <error descr="Type mismatch. Required: 'nil | string' Found: '1'">1</error>
typeMergedStringStringMap['a'] = "a string"
typeMergedStringStringMap['a'] = <error descr="Type mismatch. Required: 'nil | string' Found: '1'">1</error>


---@overload fun<K, V>(a: table<K, V>, b: table<K, V>): table<K, V>
local function overloadMergeGenericTables(a, b)
end

local overloadMergedLiteralArr = overloadMergeGenericTables({1, 2}, {3, 4})
local overloadMergedLiteralMap = overloadMergeGenericTables({a = 1, b = 2}, {c = 3, d = 4})

overloadMergedLiteralArr[1] = one
overloadMergedLiteralArr[1] = three
overloadMergedLiteralArr[1] = <error descr="Type mismatch. Required: '1 | 2 | 3 | 4 | nil' Found: '5'">5</error>

overloadMergedLiteralMap.a = one
overloadMergedLiteralMap.a = three
overloadMergedLiteralMap.a = <error descr="Type mismatch. Required: '1 | 2 | 3 | 4 | nil' Found: '5'">5</error>

<error descr="No such member 'e' found on type 'table<\"a\" | \"b\" | \"c\" | \"d\", 1 | 2 | 3 | 4>'">overloadMergedLiteralMap.e</error> = one

local overloadMergedStringStringMap = overloadMergeGenericTables(stringStringTable, stringStringTable)

overloadMergedStringStringMap.a = "a string"
overloadMergedStringStringMap.a = <error descr="Type mismatch. Required: 'nil | string' Found: '1'">1</error>
overloadMergedStringStringMap['a'] = "a string"
overloadMergedStringStringMap['a'] = <error descr="Type mismatch. Required: 'nil | string' Found: '1'">1</error>


---@overload fun<T : number>(value: T): void
---@generic T : string
---@param optional number
---@param value T
local function overloadedT(value, optional) end

overloadedT(1)
overloadedT(<error descr="Type mismatch. Required: 'T : number' Found: '\"string\"'. In: fun(value: (T : number)): void">"string"</error><error descr="Missing argument: optional: number. In: fun(value: (T : string), optional: number): void">)</error>

overloadedT(<error descr="Type mismatch. Required: 'T : string' Found: '1'">1</error>, 1)
overloadedT("string", 1)


---@generic T
---@param builder fun(): T
---@return T
function build(builder)
    return builder()
end

stringNumberTable = <error descr="Type mismatch. Required: 'table<string, number>' Found: 'table'">build(function()
    return {a = 1}
end)</error>

stringNumberTable = build(function()
    ---@type table<string, number>
    return {a = 1}
end)


---@type nil | boolean
local nilOrBoolean

---@type nil | number
local nilOrNumber

---@type nil | string
local nilOrString

---@type nil | number | string
local nilOrNumberOrString

---@return string, number
local function returnsStringNumber()
    return "one", 1
end

---@return number, boolean...
local function returnsNumberVariadicBoolean()
    return 1, true, false
end

local function chainedMultipleResults()
    return returnsStringNumber(), returnsStringNumber()
end

---@generic T
---@param f fun(): T
---@return boolean, T
function genericParameterMultipleResults(f, ...)
    return true, f()
end

anyBoolean, anyString, anyNumber = genericParameterMultipleResults(returnsStringNumber)
anyBoolean, anyString, <error descr="Type mismatch. Required: 'string' Found: 'number'">anyString</error> = <error descr="Result 3, type mismatch. Required: 'string' Found: 'number'">genericParameterMultipleResults(returnsStringNumber)</error>
anyBoolean, anyBoolean, anyString, anyNumber = genericParameterMultipleResults(returnsStringNumber), genericParameterMultipleResults(returnsStringNumber)

anyBoolean, anyNumber = genericParameterMultipleResults(returnsNumberVariadicBoolean)
anyBoolean, anyNumber, nilOrBoolean, nilOrBoolean = genericParameterMultipleResults(returnsNumberVariadicBoolean)
anyBoolean, anyNumber, <error descr="Type mismatch. Required: 'nil | string' Found: 'boolean | nil'">nilOrString</error> = <error descr="Result 3, type mismatch. Required: 'nil | string' Found: 'boolean | nil'">genericParameterMultipleResults(returnsNumberVariadicBoolean)</error>
anyBoolean, anyBoolean, anyNumber, nilOrBoolean, nilOrBoolean = genericParameterMultipleResults(returnsNumberVariadicBoolean), genericParameterMultipleResults(returnsNumberVariadicBoolean)
anyBoolean, anyBoolean, anyNumber, nilOrBoolean, <error descr="Type mismatch. Required: 'nil | string' Found: 'boolean | nil'">nilOrString</error> = genericParameterMultipleResults(returnsNumberVariadicBoolean), <error descr="Result 4, type mismatch. Required: 'nil | string' Found: 'boolean | nil'">genericParameterMultipleResults(returnsNumberVariadicBoolean)</error>

anyBoolean, anyString, anyString, anyNumber = genericParameterMultipleResults(chainedMultipleResults)
anyBoolean, anyString, anyString, <error descr="Type mismatch. Required: 'string' Found: 'number'">anyString</error> = <error descr="Result 4, type mismatch. Required: 'string' Found: 'number'">genericParameterMultipleResults(chainedMultipleResults)</error>
anyBoolean, anyBoolean, anyString, anyString, anyNumber = genericParameterMultipleResults(chainedMultipleResults), genericParameterMultipleResults(chainedMultipleResults)

---@generic T
---@param f fun(): T
---@return boolean, T...
function variadicGenericParameterMultipleResults(f, ...)
    return true, f(), f(), f()
end

anyBoolean, nilOrString, nilOrNumberOrString, nilOrNumberOrString = variadicGenericParameterMultipleResults(returnsStringNumber)
anyBoolean, nilOrString, <error descr="Type mismatch. Required: 'nil | number' Found: 'nil | number | string'">nilOrNumber</error> = <error descr="Result 3, type mismatch. Required: 'nil | number' Found: 'nil | number | string'">variadicGenericParameterMultipleResults(returnsStringNumber)</error>


---@shape StrictFieldAShape
---@field a 'string1'

---@shape LooseFieldAShape
---@field a string

---@type StrictFieldAShape
local strictfieldAShape

---@type LooseFieldAShape
local looseFieldAShape

---@generic T
---@param thing T
---@param thing2 T
---@return T
local function simpleInlineWidening(thing, thing2)
    return thing
end

looseFieldAShape = simpleInlineWidening({a = anyString}, {a = string1})
strictfieldAShape = <error descr="Type mismatch. Required: 'StrictFieldAShape' Found: 'table'">simpleInlineWidening({a = anyString}, {a = string1})</error>
strictfieldAShape = simpleInlineWidening({a = string1}, {a = string1})

---@generic T : string
---@param thing {a: T}
---@param thing2 {a: T}
---@return T
local function implicitGenericSubstitution(thing, thing2)
    return thing.a
end

---@type 'hi' | 'bye'
local hiOrBye = implicitGenericSubstitution({a = 'hi'}, {a = 'bye'})
string1 = <error descr="Type mismatch. Required: '\"string1\"' Found: '\"bye\" | \"hi\"'">implicitGenericSubstitution({a = 'hi'}, {a = 'bye'})</error>
anyString = implicitGenericSubstitution({a = <error descr="Type mismatch. Required: 'T : string' Found: '\"hi\"'">'hi'</error>}, <error descr="Type mismatch. Missing member: 'a' of: '{ a: T }'">{b = 'nope'}</error>)

---@generic T
---@param a T
---@return T[]
local function genericArray(a)
    return {a, a}
end

local substitutedArray = genericArray( anyNumber)
anyNumber = substitutedArray[1]
numberArray = substitutedArray

---@class BaseClassForGeneric
---@field A number

---@class DerivedClassForGeneric : BaseClassForGeneric
---@field B number

---@generic T : BaseClassForGeneric
---@param t T
---@return T
local function genericPassThrough1(t)
    return t
end

---@generic T : BaseClassForGeneric
---@param t T
local function genericPassThrough2(t)
    ---@type T
    local result = genericPassThrough1(t)
end

---@generic B, D: B
---@param base B
---@param derivedArray D[]
---@return D
local function genericReferencingConstraint(base, derivedArray)
    return derivedArray[1]
end

---@type BaseClassForGeneric
local baseClass

---@type DerivedClassForGeneric
local derivedClass

---@type BaseClassForGeneric[]
local baseClassArray

---@type DerivedClassForGeneric[]
local derivedClassArray

derivedClass = genericReferencingConstraint(baseClass, {[1] = derivedClass})
derivedClass = genericReferencingConstraint(baseClass, derivedClassArray)
baseClass = genericReferencingConstraint(baseClass, {[1] = baseClass})
derivedClass = <error descr="Type mismatch. Required: 'DerivedClassForGeneric' Found: 'BaseClassForGeneric'">genericReferencingConstraint(baseClass, {[1] = baseClass})</error> -- Expect error

---@generic A: B, B: A
---@param a A
---@param b B
local function illegalGenericConstraint(a, b) end

-- Not valid, but shouldn't raise an exception (i.e. no stack overflow).
illegalGenericConstraint(<error descr="Type mismatch. Required: 'A : B' Found: '1'">1</error>, <error descr="Type mismatch. Required: 'B : A : B' Found: '1'">1</error>) -- Expect error

---@type fun<K, V, R>(tab: table<K, V>, initial: R, func: fun(memo: R, value: V, key: K): R): R
local reduce

anyString = reduce({}, anyString, function(str, _, _)
    return str
end)

anyNumber = <error descr="Type mismatch. Required: 'number' Found: 'string'">reduce({}, anyString, function(str, _, _)
    return str
end)</error>


---@type fun<T, RK, RV>(arr: T[], callback: fun(obj: T, i: number): RK, RV): table<RK, RV>
local mapToTable

---@type table<number, string>
local numberStringTable

---@type string[]
local stringArray

numberStringTable = mapToTable(stringArray, function(str, i)
    return i, str
end)

stringNumberTable = <error descr="Type mismatch. Required: 'table<string, number>' Found: 'table<number, string>'">mapToTable(stringArray, function(str, i)
    return i, str
end)</error>


---@type fun<A, B>(func: fun(): A, B): A, B
local returnMultipleGenericParams

local a, b = returnMultipleGenericParams(<error descr="Type mismatch. Required: 'fun(): 1, 1' Found: 'fun(): 1'">function()
    return 1
end</error>)

anyNumber = a
anyString = <error descr="Type mismatch. Required: 'string' Found: 'number'">a</error>
