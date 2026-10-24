---@type string
local aString

---@type boolean
local aBoolean

-- NOTE: The space after `table` is intentional, we're also testing our look-ahead behavior.
---@type fun(table : string, boolean: boolean): string
local poorlyNamedParamFunction1

---@type fun(fun: string, boolean: boolean): string
local poorlyNamedParamFunction2

---@type fun(vararg: string, boolean: boolean): string
local poorlyNamedParamFunction3

---@param table string
---@param boolean boolean
---@return string
local function poorlyNamedParamFunction4(table, boolean) return aString end

---@param fun string
---@param boolean boolean
---@return string
local function poorlyNamedParamFunction5(table, boolean) return aString end

---@param vararg string
---@param boolean boolean
---@return string
local function poorlyNamedParamFunction6(table, boolean) return aString end

aString = poorlyNamedParamFunction1(aString, aBoolean)
aString = poorlyNamedParamFunction2(aString, aBoolean)
aString = poorlyNamedParamFunction3(aString, aBoolean)
aString, <error descr="Too many assignees, will be assigned nil.">aString</error> = poorlyNamedParamFunction3(aString, aBoolean) -- Expect error
aString = poorlyNamedParamFunction4(aString, aBoolean)
aString = poorlyNamedParamFunction5(aString, aBoolean)
aString = poorlyNamedParamFunction6(aString, aBoolean)
