---@type number
local aNumber

---@return number
local function returnsANumber() return math.random() end

aNumber = returnsANumber()
aNumber = 1
aNumber = 1 + 1

---@type 1
local theNumberOne

theNumberOne = 1
theNumberOne = 0x1 -- Hex representation
theNumberOne = 1.0 -- All numbers are floats in Lua < 5.3. >= 5.3 they're implicitly comparable i.e. (1 == 1.0) == true

theNumberOne = <error descr="Type mismatch. Required: '1' Found: '2'">2</error>
theNumberOne = <error descr="Type mismatch. Required: '1' Found: 'number'">returnsANumber()</error>

---@type -1
local negativeOne

negativeOne = <error descr="Type mismatch. Required: '-1' Found: '1'">1</error> -- Expect error
negativeOne = -1
negativeOne = <error descr="Type mismatch. Required: '-1' Found: '1'">- - 1</error> -- Expect error
negativeOne = - - - 1
negativeOne = <error descr="Type mismatch. Required: '-1' Found: '\"1\"'">"1"</error> -- Expect error
negativeOne = -"1"
negativeOne = -" 1"
negativeOne = <error descr="Type mismatch. Required: '-1' Found: '1'">-"-1"</error> -- Expect error
negativeOne = <error descr="Type mismatch. Required: '-1' Found: 'string'">-"- -1"</error> -- Expect error
negativeOne = -"0x1"
negativeOne = <error descr="Type mismatch. Required: '-1' Found: '1'">-"-0x1"</error> -- Expect error
