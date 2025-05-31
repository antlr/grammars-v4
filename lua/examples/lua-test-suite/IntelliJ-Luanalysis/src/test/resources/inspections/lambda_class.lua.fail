---@class LambdaClass
---@overload fun(a: number): LambdaClass
local LambdaClass = {}

setmetatable(LambdaClass,  {
    ---@param a number
    __call = function(_, a)
        local self = --[[---@type LambdaClass]] {}

        ---@return number
        function self.getNumber()
            return a
        end

        return self
    end
})

local missingArg = LambdaClass(<error descr="Missing argument: a: number">)</error>
local wrongArg = LambdaClass(<error descr="Type mismatch. Required: 'number' Found: '\"one\"'">"one"</error>)
local lambdaClass = LambdaClass(1)

---@type number
local aNumber = lambdaClass.getNumber()

---@type string
local aString = <error descr="Type mismatch. Required: 'string' Found: 'number'">lambdaClass.getNumber()</error>
