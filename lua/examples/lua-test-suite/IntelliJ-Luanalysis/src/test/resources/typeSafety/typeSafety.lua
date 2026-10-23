---
--- Created by Perry.
--- DateTime: 30-9-2017 20:56
---

--- Basic test
--- @param x number
function test(x) end

test("3") -- Type mismatch, string instead of number
test(nil) -- Type mismatch, nil instead of number (Only if strict nil option is checked)
test(3) -- Valid
test(4, 5) -- Too many arguments
test() -- Not enough arguments

--- Union test
--- @param x number | nil
function test2(x) end

test2(3) -- Valid
test2(nil) -- Valid
test2("") -- Type mismatch, string instead of number or nil

--- Overload test
--- @overload fun(x: number, y: string): boolean
--- @param x number
function test3(x) end

test3(3) -- Valid, main signature
test3(3, "4") -- Valid, overload
test3(3, 4, 5) -- No matching overload
test3("") -- No matching overload

--- List test
--- @param x number[]
function testList(x) end

--- @type number[]
local list = {1,2,3}
testList(list) --- Valid
