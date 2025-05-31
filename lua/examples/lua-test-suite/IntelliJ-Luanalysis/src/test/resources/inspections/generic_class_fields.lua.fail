---@class GenericFieldsA<A>
---@field a A
local GenericFieldsA

---@class GenericFieldsB<B> : GenericFieldsA<B>
---@field b B
local GenericFieldsB

---@class GenericFieldsC<C> : GenericFieldsB<C>
---@field c C
local GenericFieldsC

---@type GenericFieldsA<string>
local genericA

---@type GenericFieldsB<string>
local genericB

---@type GenericFieldsC<string>
local genericC

---@type string
local aString

---@type number
local aNumber

aString = genericA.a
aNumber = <error descr="Type mismatch. Required: 'number' Found: 'string'">genericA.a</error>

aString = genericB.a
aNumber = <error descr="Type mismatch. Required: 'number' Found: 'string'">genericB.a</error>
aString = genericB.b
aNumber = <error descr="Type mismatch. Required: 'number' Found: 'string'">genericB.b</error>

aString = genericC.a
aNumber = <error descr="Type mismatch. Required: 'number' Found: 'string'">genericC.a</error>
aString = genericC.b
aNumber = <error descr="Type mismatch. Required: 'number' Found: 'string'">genericC.b</error>
aString = genericC.c
aNumber = <error descr="Type mismatch. Required: 'number' Found: 'string'">genericC.c</error>

aString = genericA.a
aString = <error descr="No such member 'b' found on type 'GenericFieldsA<string>'">genericA.b</error>
aString = <error descr="No such member 'c' found on type 'GenericFieldsA<string>'">genericA.c</error>


---@class GenericFieldsD<T>
---@field doThing fun(callback: nil | (fun(t: T): boolean)): void

---@type GenericFieldsD<number>
local genericFieldsD

genericFieldsD.doThing(function(t)
    aNumber = t
    aString = <error descr="Type mismatch. Required: 'string' Found: 'number'">t</error>
    return true
end)
