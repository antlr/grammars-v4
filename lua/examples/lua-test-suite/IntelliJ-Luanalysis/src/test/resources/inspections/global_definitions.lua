---@class GlobalClass
GlobalClass = {}

---@type number
GlobalClass.someNumber = 1

function GlobalClass.doSomething()
end

GlobalAnonymousClass = {}

---@type number
GlobalAnonymousClass.anotherNumber = 1

function GlobalAnonymousClass.doSomethingElse()
end

GlobalAnonymousClass.anotherNumber = 2
