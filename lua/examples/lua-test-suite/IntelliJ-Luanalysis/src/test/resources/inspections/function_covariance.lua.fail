---@class ReturnA
local ReturnA = {}

---@class ReturnB : ReturnA
local ReturnB = {}

---@class ReturnC : ReturnB
local ReturnC = {}

---@type fun(a: "stringLiteral"): ReturnB
local callback

---@type fun(a: string): ReturnC @Covariant return, contravariant parameters.
local subtype

callback = subtype

---@type fun(a: string): ReturnA
local notSubtype

callback = <error descr="Type mismatch. Required: 'fun(a: \"stringLiteral\"): ReturnB' Found: 'fun(a: string): ReturnA'">notSubtype</error>

---@type fun(a: string): ReturnB
local callback2

---@type fun(a: "stringLiteral"): ReturnB
local notSubtype2

callback2 = <error descr="Type mismatch. Required: 'fun(a: string): ReturnB' Found: 'fun(a: \"stringLiteral\"): ReturnB'">notSubtype2</error>
