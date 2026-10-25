---@param num number
local function wantsNumber(num) end

---@type number
local aNumber

---@type string
local aString

---@type any
local unknown

local assignedUnknown = <warning descr="Undeclared variable 'notAVarA'.">notAVarA</warning>
local implicitUnknown -- Not technically unknown (any), but for the most part should be treated like it.

wantsNumber(aNumber)
wantsNumber(<error descr="Type mismatch. Required: 'number' Found: 'string'">aString</error>)
wantsNumber(unknown)
wantsNumber(assignedUnknown)
wantsNumber(implicitUnknown)
wantsNumber(<warning descr="Undeclared variable 'notAVarA'.">notAVarA</warning>)

aNumber = aNumber
aNumber = <error descr="Type mismatch. Required: 'number' Found: 'string'">aString</error>
aNumber = unknown
aNumber = assignedUnknown
aNumber = implicitUnknown

aString = aString
aString = <error descr="Type mismatch. Required: 'string' Found: 'number'">aNumber</error>
aString = unknown
aString = assignedUnknown
aString = implicitUnknown

unknown = unknown
unknown = aNumber
unknown = aString
unknown = assignedUnknown
unknown = implicitUnknown

unknown.aVar = 1
unknown.doThing()
unknown:doThing()

assignedUnknown.aVar = 1
assignedUnknown.doThing()
assignedUnknown:doThing()

implicitUnknown.aVar = 1
implicitUnknown.doThing()
implicitUnknown:doThing()

-- As above, not technically unknown (any), but for the most part should be treated like it
function implicitUnknown(param)
    aNumber = param
    aString = param
    unknown = param

    param.aVar = 1
    param.doThing()
    param:doThing()
end

---@param param any
function explicitUnknown(param)
    aNumber = param
    aString = param
    unknown = param

    param.aVar = 1
    param.doThing()
    param:doThing()
end
