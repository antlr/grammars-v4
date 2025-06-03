local x = 1

if true then
    x = 2
    local x = 3
    local x = 4
    local y = 3
end

if true then
    local y = 4
end

local function foo() end
if true then
    local function foo() end
end

for _ = 1, 5 do
    for _ = 1, 5 do
    end
end
