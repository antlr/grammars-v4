---@class EmptyClass
local EmptyClass = {}

---@class InvalidClassAssignment
local InvalidClassAssignment = <error descr="Type mismatch. Required: 'table' Found: 'nil'">nil</error>

---@type EmptyClass
local emptyClass

---@class ClassWithFields
---@field a string
---@field b number
local ClassWithFields = {}

---@type ClassWithFields
local classWithFields

classWithFields.a = <error descr="Type mismatch. Required: 'string' Found: '1'">1</error>
classWithFields.b = <error descr="Type mismatch. Required: 'number' Found: '\"someString\"'">"someString"</error>

classWithFields.a = "someString"
classWithFields.b = 1

classWithFields.a = "someOtherString"
classWithFields.b = 2

classWithFields = <error descr="Type mismatch. Required: 'ClassWithFields' Found: 'table'">{
    a = "someString",
    b = 1
}</error>

---@class Callable
---@overload fun(a: number, b: number): ClassWithFields
local Callable = {}

---@type Callable
local callable

classWithFields = callable(1, 2)
emptyClass = <error descr="Type mismatch. Required: 'EmptyClass' Found: 'ClassWithFields'">callable(1, 2)</error>

---@class DeclarationlessCallable
---@overload fun(): DeclarationlessCallable

---@type DeclarationlessCallable
local DeclarationlessCallable

---@type DeclarationlessCallable
local declarationlessCallable

declarationlessCallable = DeclarationlessCallable()
callable = <error descr="Type mismatch. Required: 'Callable' Found: 'DeclarationlessCallable'">DeclarationlessCallable()</error>
