---@type boolean
local unknownBoolean

---@type true|false
local unknownLiteralBoolean

---@type true
local trueLiteral

---@type false
local falseLiteral

unknownBoolean = unknownLiteralBoolean
unknownLiteralBoolean = unknownBoolean

unknownBoolean = true
unknownBoolean = false
unknownBoolean = trueLiteral
unknownBoolean = falseLiteral

unknownLiteralBoolean = true
unknownLiteralBoolean = false
unknownLiteralBoolean = trueLiteral
unknownLiteralBoolean = falseLiteral

trueLiteral = true
trueLiteral = <error descr="Type mismatch. Required: 'true' Found: 'false'">false</error>
trueLiteral = trueLiteral
trueLiteral = <error descr="Type mismatch. Required: 'true' Found: 'false'">falseLiteral</error>
trueLiteral = <error descr="Type mismatch. Required: 'true' Found: 'boolean'">unknownBoolean</error>
trueLiteral = <error descr="Type mismatch. Required: 'true' Found: 'boolean'">unknownLiteralBoolean</error>

falseLiteral = <error descr="Type mismatch. Required: 'false' Found: 'true'">true</error>
falseLiteral = false
falseLiteral = falseLiteral
falseLiteral = <error descr="Type mismatch. Required: 'false' Found: 'true'">trueLiteral</error>
falseLiteral = <error descr="Type mismatch. Required: 'false' Found: 'boolean'">unknownBoolean</error>
falseLiteral = <error descr="Type mismatch. Required: 'false' Found: 'boolean'">unknownLiteralBoolean</error>
