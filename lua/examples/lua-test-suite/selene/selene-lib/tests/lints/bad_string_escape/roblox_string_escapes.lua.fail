local x = "\a"
local x = "\b"
local x = "\f"
local x = "\n"
local x = "\r"
local x = "\t"
local x = "\v"
local x = "\""
local x = "\\"

local x = "\'\""

local x = '\"\''

local x = "\97"

-- local x = "\ -- Full-moon doesn't like this style of string
-- "

local bad = "\z"

local bad2 = "\x1\x10"

local bad3 = "\u{1337}\u{1234\u{1337}"

local bad4 = "\u{10ffff}\u{110000}"

local bad5 = "\m"

local bad6 = "\999"

local bad7 = "\u{ffffffffff}"

local good = [[\z\x1\u{1234\u{110000}\m\"\'\999]]

-- See: Issue #292
local decimal_decimal_hex = "\01B"
local decimal_hex = "\1B"
local all_hex = "\aaa"