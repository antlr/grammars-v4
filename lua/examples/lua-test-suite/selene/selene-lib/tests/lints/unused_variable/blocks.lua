-- Functions
local function unusedFunction()
    local unusedVariableA = 1
    local unusedVariableB = 1
end

print(unusedVariableB)
local overidden = true

local function overridesIt()
    local overidden = false
    print(overidden)
end

overridesIt()

-- Anonymous functions
local a = 1
print(function()
    _G.foo = a
end)
