---@type string | number
local stringOrNumberVar

---@param returnString boolean
---@return string | number
local function stringOrNumber(returnString)
    return returnString and "someString" or 1
end

---@type any
local unknown

stringOrNumberVar = "hi"
stringOrNumberVar = 7
stringOrNumberVar = stringOrNumber(unknown)

---@type "hi" | number
local stringLiteralOrNumber

stringOrNumberVar = stringLiteralOrNumber

stringLiteralOrNumber = <error>stringOrNumberVar</error>

---@type table<string, "A" | "B">
local aOrB

---@type table<string, "B" | "A">
local bOrA

---@type table<string, "A" | "C">
local aOrC

aOrB = bOrA
bOrA = aOrB
aOrB = <error>aOrC</error>

---@shape Engine
---@field run function

---@shape Horse
---@field run function

---@shape Shed
---@field occupant Engine | Horse

---@type Shed
local shed

shed = {
    occupant = <error descr="Type mismatch. Required: 'Engine | Horse' Found: '\"invalid\"'">"invalid"</error>
}

---@shape Foal : Horse

---@shape Nursery
---@field occupant Foal

---@shape Stable
---@field occupant Horse

---@type nil | Nursery
local maybeNursery

---@type Stable
local stable

local horseShedOccupant = (maybeNursery or stable).occupant

horseShedOccupant = <error descr="Type mismatch. Required: 'Horse' Found: '\"invalid\"'">"invalid"</error>
