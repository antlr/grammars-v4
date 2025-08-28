---@type number
local aNumber

---@type boolean
local aBoolean

---@type nil | number
local nilOrNumber

---@type nil | boolean
local nilOrBoolean

---@type fun(): number, boolean...
local varreturnFunction

aNumber, nilOrBoolean, nilOrBoolean = varreturnFunction()
aNumber, nilOrBoolean, nilOrBoolean, <error descr="Type mismatch. Required: 'nil | number' Found: 'boolean | nil'">nilOrNumber</error>, nilOrBoolean = <error descr="Result 4, type mismatch. Required: 'nil | number' Found: 'boolean | nil'">varreturnFunction()</error>


---@param numberParam number
---@return number, boolean...
local function varreturnFunction2()
    if aNumber == 1 then
        return 1
    elseif aNumber == 2 then
        return 1, <error descr="Result 2, type mismatch. Required: 'boolean' Found: '\"not a boolean\"'">"not a boolean"</error>
    elseif aNumber == 3 then
        return 1, true
    elseif aNumber == 4 then
        return 1, true, false
    else
        <error descr="Incorrect number of values. Expected 1 but found 0.">return</error>
    end
end

aNumber, nilOrBoolean, nilOrBoolean = varreturnFunction2()
aNumber, nilOrBoolean, nilOrBoolean, <error descr="Type mismatch. Required: 'nil | number' Found: 'boolean | nil'">nilOrNumber</error>, nilOrBoolean = <error descr="Result 4, type mismatch. Required: 'nil | number' Found: 'boolean | nil'">varreturnFunction2()</error>

---@param a number
---@param b string
local function acceptsNumberString(a, b) end

acceptsNumberString(<error descr="Result 2, type mismatch. Required: 'string' Found: 'boolean'">varreturnFunction2()</error><error descr="Missing argument: b: string">)</error>

---@param a number
---@vararg string
local function acceptsNumberVariadicString(a, ...) end

acceptsNumberVariadicString(<error descr="Variadic result, type mismatch. Required: 'string' Found: 'boolean'">varreturnFunction2()</error>)

---@type fun(): boolean...
local varreturnFunction3

nilOrBoolean, nilOrBoolean = varreturnFunction3()
nilOrBoolean, nilOrBoolean, <error descr="Type mismatch. Required: 'nil | number' Found: 'boolean | nil'">nilOrNumber</error>, nilOrBoolean = <error descr="Result 3, type mismatch. Required: 'nil | number' Found: 'boolean | nil'">varreturnFunction3()</error>

---@return boolean...
local function varreturnFunction4()
    if aNumber == 1 then
        return
    elseif aNumber == 2 then
        return <error descr="Type mismatch. Required: 'boolean' Found: '\"not a boolean\"'">"not a boolean"</error>
    elseif aNumber == 3 then
        return true
    elseif aNumber == 4 then
        return true, false
    end
end

nilOrBoolean, nilOrBoolean = varreturnFunction4()
nilOrBoolean, nilOrBoolean, <error descr="Type mismatch. Required: 'nil | number' Found: 'boolean | nil'">nilOrNumber</error>, nilOrBoolean = <error descr="Result 3, type mismatch. Required: 'nil | number' Found: 'boolean | nil'">varreturnFunction4()</error>

---@generic T
---@param list T[]
---@return T...
local function genericVarreturn(list)
    return table.unpack(list)
end

nilOrNumber, nilOrNumber = genericVarreturn({1, 2})
nilOrNumber, <error descr="Type mismatch. Required: 'boolean | nil' Found: '1 | 2 | nil'">nilOrBoolean</error> = <error descr="Result 2, type mismatch. Required: 'boolean | nil' Found: '1 | 2 | nil'">genericVarreturn({1, 2})</error>

local implicitNilOrNumber1, implicitNilOrNumber2 = genericVarreturn({ 1, 2})

nilOrNumber = implicitNilOrNumber1
nilOrBoolean = <error descr="Type mismatch. Required: 'boolean | nil' Found: '1 | 2 | nil'">implicitNilOrNumber1</error>
nilOrNumber = implicitNilOrNumber2
nilOrBoolean = <error descr="Type mismatch. Required: 'boolean | nil' Found: '1 | 2 | nil'">implicitNilOrNumber2</error>

---@type fun(): boolean...
local booleanVarreturn

---@type fun(a: boolean | nil): void
local nilOrBooleanParameter

nilOrBooleanParameter(booleanVarreturn()<error descr="Missing argument: a: boolean | nil">)</error> -- Expect error
nilOrBooleanParameter((booleanVarreturn()))

---@type fun(): number...
local numberVarreturn

---@type number
local aNumber

aNumber = numberVarreturn() or aNumber
aNumber = <error descr="Type mismatch. Required: 'number' Found: 'boolean | number'">numberVarreturn() or aBoolean</error>
aNumber = <error descr="Type mismatch. Required: 'number' Found: 'nil | number'">numberVarreturn() or numberVarreturn()</error>
nilOrNumber = numberVarreturn() or numberVarreturn()
