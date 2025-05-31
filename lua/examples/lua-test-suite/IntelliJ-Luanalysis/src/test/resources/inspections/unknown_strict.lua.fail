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

<error descr="No such member 'aVar' found on type 'any'">unknown.aVar</error> = 1
<error descr="No such member 'doThing' found on type 'any'">unknown.doThing</error>()
<error descr="No such member 'doThing' found on type 'any'">unknown:doThing</error>()

<error descr="No such member 'aVar' found on type 'any'">assignedUnknown.aVar</error> = 1
<error descr="No such member 'doThing' found on type 'any'">assignedUnknown.doThing</error>()
<error descr="No such member 'doThing' found on type 'any'">assignedUnknown:doThing</error>()

<error descr="No such member 'aVar' found on type '[local implicitUnknown]'">implicitUnknown.aVar</error> = 1
<error descr="Unknown function 'doThing'."><error descr="No such member 'doThing' found on type '[local implicitUnknown]'">implicitUnknown.doThing</error>()</error>
<error descr="Unknown function 'doThing'."><error descr="No such member 'doThing' found on type '[local implicitUnknown]'">implicitUnknown:doThing</error>()</error>

function implicitUnknown(param)
    aNumber = param
    aString = param
    unknown = param

    <error descr="No such member 'aVar' found on type '[local param]'">param.aVar</error> = 1
    <error descr="Unknown function 'doThing'."><error descr="No such member 'doThing' found on type '[local param]'">param.doThing</error>()</error>
    <error descr="Unknown function 'doThing'."><error descr="No such member 'doThing' found on type '[local param]'">param:doThing</error>()</error>
end

---@param param any
function explicitUnknown(param)
    aNumber = param
    aString = param
    unknown = param

    <error descr="No such member 'aVar' found on type 'any'">param.aVar</error> = 1
    <error descr="No such member 'doThing' found on type 'any'">param.doThing</error>()
    <error descr="No such member 'doThing' found on type 'any'">param:doThing</error>()
end
