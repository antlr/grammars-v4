GlobalClass.doSomething()
GlobalClass.someNumber = 2

GlobalAnonymousClass.anotherNumber = 2
GlobalAnonymousClass.doSomethingElse()

---@type number
local aNumber

---@type string
local aString

aNumber = GlobalClass.someNumber
aNumber = GlobalAnonymousClass.anotherNumber

aString = <error descr="Type mismatch. Required: 'string' Found: 'number'">GlobalClass.someNumber</error>
aString = <error descr="Type mismatch. Required: 'string' Found: 'number'">GlobalAnonymousClass.anotherNumber</error>

---@type number
local fromStdLib = math.pi
