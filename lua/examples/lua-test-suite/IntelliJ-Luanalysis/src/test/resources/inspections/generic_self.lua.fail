---@type number
local aNumber

---@type string
local aString

---@class GenericSelfA<T : string>
---@field a T
local GenericSelfA = {}

---@return self
function GenericSelfA:colonMethod()
    ---@type self
    local selfTypedVar = self

    ---@type GenericSelfA<string>
    local someGenericSelfA

    someGenericSelfA = <error descr="Type mismatch. Required: 'GenericSelfA<string>' Found: 'GenericSelfA#self'">selfTypedVar</error>
    self = <error descr="Type mismatch. Required: 'GenericSelfA#self' Found: 'GenericSelfA<string>'">someGenericSelfA</error>
    selfTypedVar = <error descr="Type mismatch. Required: 'GenericSelfA#self' Found: 'GenericSelfA<string>'">someGenericSelfA</error>
    aNumber = <error descr="Type mismatch. Required: 'number' Found: 'GenericSelfA#self'">selfTypedVar</error>


    aNumber = <error descr="Type mismatch. Required: 'number' Found: 'T : string'">selfTypedVar.a</error>
    aString = selfTypedVar.a

    return self
end

---@return self
function GenericSelfA.dotMethod()
    ---@type self
    local selfTypedVar = <warning descr="Undeclared variable 'self'.">self</warning>

    ---@type GenericSelfA<string>
    local someGenericSelfA

    someGenericSelfA = <error descr="Type mismatch. Required: 'GenericSelfA<string>' Found: 'GenericSelfA#self'">selfTypedVar</error>
    selfTypedVar = <error descr="Type mismatch. Required: 'GenericSelfA#self' Found: 'GenericSelfA<string>'">someGenericSelfA</error>
    aNumber = <error descr="Type mismatch. Required: 'number' Found: 'GenericSelfA#self'">selfTypedVar</error>

    aNumber = <error descr="Type mismatch. Required: 'number' Found: 'T : string'">selfTypedVar.a</error>
    aString = selfTypedVar.a

    return selfTypedVar
end

---@return self
GenericSelfA.lambdaMethod = function()
    ---@type self
    local selfTypedVar = <warning descr="Undeclared variable 'self'.">self</warning>

    ---@type GenericSelfA<string>
    local someGenericSelfA

    someGenericSelfA = <error descr="Type mismatch. Required: 'GenericSelfA<string>' Found: 'GenericSelfA#self'">selfTypedVar</error>
    selfTypedVar = <error descr="Type mismatch. Required: 'GenericSelfA#self' Found: 'GenericSelfA<string>'">someGenericSelfA</error>
    aNumber = <error descr="Type mismatch. Required: 'number' Found: 'GenericSelfA#self'">selfTypedVar</error>

    aNumber = <error descr="Type mismatch. Required: 'number' Found: 'T : string'">selfTypedVar.a</error>
    aString = selfTypedVar.a

    return selfTypedVar
end

---@type GenericSelfA<string>
local selfAString

---@class GenericSelfB<T : string> : GenericSelfA<T>
---@field b T
local GenericSelfB = {}

---@type GenericSelfB<string>
local selfBString

---@type GenericSelfB<"string literal">
local selfBStringLiteral

selfBString = <error descr="Type mismatch. Required: 'GenericSelfB<string>' Found: 'GenericSelfB<T : string>'">GenericSelfB:colonMethod()</error>
selfBString = <error descr="Type mismatch. Required: 'GenericSelfB<string>' Found: 'GenericSelfB<T : string>'">GenericSelfB.dotMethod()</error>
selfBString = <error descr="Type mismatch. Required: 'GenericSelfB<string>' Found: 'GenericSelfB<T : string>'">GenericSelfB.lambdaMethod()</error>

selfAString = <error descr="Type mismatch. Required: 'GenericSelfA<string>' Found: 'GenericSelfA<T : string>'">GenericSelfA:colonMethod()</error>
selfAString = <error descr="Type mismatch. Required: 'GenericSelfA<string>' Found: 'GenericSelfA<T : string>'">GenericSelfA.dotMethod()</error>
selfAString = <error descr="Type mismatch. Required: 'GenericSelfA<string>' Found: 'GenericSelfA<T : string>'">GenericSelfA.lambdaMethod()</error>

selfBString = selfBString:colonMethod()
selfBString = selfBString.dotMethod()
selfBString = selfBString.lambdaMethod()

selfAString = selfBString:colonMethod()
selfAString = selfBString.dotMethod()
selfAString = selfBString.lambdaMethod()

selfBString = <error descr="Type mismatch. Required: 'GenericSelfB<string>' Found: 'GenericSelfA<string>'">selfAString:colonMethod()</error>
selfBString = <error descr="Type mismatch. Required: 'GenericSelfB<string>' Found: 'GenericSelfA<string>'">selfAString.dotMethod()</error>
selfBString = <error descr="Type mismatch. Required: 'GenericSelfB<string>' Found: 'GenericSelfA<string>'">selfAString.lambdaMethod()</error>

selfBStringLiteral = selfBStringLiteral:colonMethod()
selfBStringLiteral = selfBStringLiteral.dotMethod()
selfBStringLiteral = selfBStringLiteral.lambdaMethod()

selfAString = <error>selfBStringLiteral:colonMethod()</error>
selfAString = <error>selfBStringLiteral.dotMethod()</error>
selfAString = <error>selfBStringLiteral.lambdaMethod()</error>

selfBString = <error>selfBStringLiteral:colonMethod()</error>
selfBString = <error>selfBStringLiteral.dotMethod()</error>
selfBString = <error>selfBStringLiteral.lambdaMethod()</error>
