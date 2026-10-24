---@type string
local str

local implictlyTypedString = "notTypedAsALiteral"

---@type "stringLiteral"
local explictlyTypedLiteral

str = "some string"
str = implictlyTypedString
str = explictlyTypedLiteral

explictlyTypedLiteral = "stringLiteral"
explictlyTypedLiteral = <error descr="Type mismatch. Required: '\"stringLiteral\"' Found: 'string'">str</error>
explictlyTypedLiteral = <error descr="Type mismatch. Required: '\"stringLiteral\"' Found: 'string'">implictlyTypedString</error>

local literalTable = {
    a = "stringLiteral",
    b = "aDifferentStringLiteral"
}

str = literalTable.a
explictlyTypedLiteral = literalTable.a
explictlyTypedLiteral = <error descr="Type mismatch. Required: '\"stringLiteral\"' Found: '\"aDifferentStringLiteral\"'">literalTable.b</error>
