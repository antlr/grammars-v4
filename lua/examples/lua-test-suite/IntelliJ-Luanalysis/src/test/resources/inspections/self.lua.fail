---@type number
local aNumber

---@class SelfA
---@field a string
---@overload fun(): self
local SelfA = {}

---@return self
function SelfA:colonMethod()
    ---@type self
    local selfTypedVar = self

    ---@type SelfA
    local someSelfA

    someSelfA = self
    self = <error descr="Type mismatch. Required: 'SelfA#self' Found: 'SelfA'">someSelfA</error>
    selfTypedVar = <error descr="Type mismatch. Required: 'SelfA#self' Found: 'SelfA'">someSelfA</error>
    aNumber = <error descr="Type mismatch. Required: 'number' Found: 'SelfA#self'">self</error>

    aNumber = <error descr="Type mismatch. Required: 'number' Found: 'string'">self.a</error>
    aString = self.a

    return self
end

---@return self
function SelfA.dotMethod()
    ---@type self
    local selfTypedVar = <warning descr="Undeclared variable 'self'.">self</warning>

    ---@type SelfA
    local someSelfA

    someSelfA = selfTypedVar
    selfTypedVar = <error descr="Type mismatch. Required: 'SelfA#self' Found: 'SelfA'">someSelfA</error>
    aNumber = <error descr="Type mismatch. Required: 'number' Found: 'SelfA#self'">selfTypedVar</error>

    aNumber = <error descr="Type mismatch. Required: 'number' Found: 'string'">selfTypedVar.a</error>
    aString = selfTypedVar.a

    return selfTypedVar
end

---@return self
SelfA.lambdaMethod = function()
    ---@type self
    local selfTypedVar = <warning descr="Undeclared variable 'self'.">self</warning>

    ---@type SelfA
    local someSelfA

    someSelfA = selfTypedVar
    selfTypedVar = <error descr="Type mismatch. Required: 'SelfA#self' Found: 'SelfA'">someSelfA</error>
    aNumber = <error descr="Type mismatch. Required: 'number' Found: 'SelfA#self'">selfTypedVar</error>

    aNumber = <error descr="Type mismatch. Required: 'number' Found: 'string'">selfTypedVar.a</error>
    aString = selfTypedVar.a

    return selfTypedVar
end

---@type SelfA
local selfA

---@class SelfB : SelfA
---@overload fun(): self
---@field bb string
local SelfB = {}

---@type SelfB
local selfB

selfB = SelfB()
selfB = SelfB:colonMethod()
selfB = SelfB:dotMethod()
selfB = SelfB:lambdaMethod()

selfA = SelfA()
selfA = SelfB:colonMethod()
selfA = SelfB:dotMethod()
selfA = SelfB:lambdaMethod()

selfB = <error descr="Type mismatch. Required: 'SelfB' Found: 'SelfA'">SelfA()</error>
selfB = <error descr="Type mismatch. Required: 'SelfB' Found: 'SelfA'">SelfA:colonMethod()</error>
selfB = <error descr="Type mismatch. Required: 'SelfB' Found: 'SelfA'">SelfA:dotMethod()</error>
selfB = <error descr="Type mismatch. Required: 'SelfB' Found: 'SelfA'">SelfA:lambdaMethod()</error>

selfB = selfB()
selfB = selfB:colonMethod()
selfB = selfB:dotMethod()
selfB = selfB:lambdaMethod()

selfA = selfA()
selfA = selfB:colonMethod()
selfA = selfB:dotMethod()
selfA = selfB:lambdaMethod()

selfB = <error descr="Type mismatch. Required: 'SelfB' Found: 'SelfA'">selfA()</error>
selfB = <error descr="Type mismatch. Required: 'SelfB' Found: 'SelfA'">selfA:colonMethod()</error>
selfB = <error descr="Type mismatch. Required: 'SelfB' Found: 'SelfA'">selfA:dotMethod()</error>
selfB = <error descr="Type mismatch. Required: 'SelfB' Found: 'SelfA'">selfA:lambdaMethod()</error>


---@class ClassWithInstanceMethods
local ClassWithInstanceMethods = {}

function ClassWithInstanceMethods:withoutParam()
end

---@param a string
function ClassWithInstanceMethods:withParam(a)
end

---@type ClassWithInstanceMethods
local classWithInstanceMethods

classWithInstanceMethods.withoutParam(<error descr="Missing self argument.

Did you mean to call the method with a colon?">)</error>
classWithInstanceMethods.withParam(<error descr="Type mismatch. Required: 'ClassWithInstanceMethods' Found: '\"abc\"'.

Did you mean to call the method with a colon?">"abc"</error><error descr="Missing argument: a: string">)</error>
classWithInstanceMethods.withoutParam(classWithInstanceMethods)
classWithInstanceMethods.withParam(classWithInstanceMethods, "abc")

---@class SetmetatableSelf
---@overload fun(): self
local SetmetatableSelf = {}

setmetatable(SetmetatableSelf,  {
    ---@return SetmetatableSelf @In the presence of setmetatable, self is given the return type of __call
    __call = function(_)
        ---@type self
        local self

        ---@return self
        function self.returnsSelf()
            return self
        end

        return self
    end
})

---@type SetmetatableSelf
local setmetatableSelf

setmetatableSelf = SetmetatableSelf()
setmetatableSelf = setmetatableSelf.returnsSelf()
aNumber = <error descr="Type mismatch. Required: 'number' Found: 'SetmetatableSelf'">SetmetatableSelf()</error>
aNumber = <error descr="Type mismatch. Required: 'number' Found: 'SetmetatableSelf'">setmetatableSelf.returnsSelf()</error>
