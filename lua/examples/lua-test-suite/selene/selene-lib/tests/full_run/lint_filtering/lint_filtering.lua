--# selene: allow(type_check_inside_call)

-- selene: allow(unused_variable)
local function a(x, y)
    local unusedHereToo = true
end

-- selene: allow(undefined_variable, unused_variable)
local b = call()

-- selene: allow(lint_thatll_never_be_created)
local c = 1

print(
    -- selene: allow(undefined_variable)
    x,
    -- This one should not be ignored, since the last filter was only on x
    y
)

-- selene: allow(undefined_variable)
-- selene: allow(unused_variable)
local d = call()

-- selene: allow(undefined_variable)
-- This is a very interesting comment in between filters.
-- You'll never know what you'll find in here.
-- selene: allow(unused_variable)
local e = call()

--[[
    selene: allow(undefined_variable)
    selene: allow(unused_variable)
]]
local f = call()

-- Even though "selene: allow(unused_variable)" is in this comment, it shouldn't be applied since it isn't its own line
local g = 1

-- selene: allow(unused_variable)
local function callback()
    local unusedButGood = 1
    -- selene: deny(unused_variable)
    local unusedButBad = 2
end

-- This case is allowed file wide in the beginning
print(type(1 == "boolean"))

--# selene: allow(unused_variable)
print("The above comment should lint, since its not at the beginning of the file")
print("This is to make sure programmers understand the difference!")

-- selene: allow(unused_variable)
do
    local doA = 1
    do
        local doB = 2

        -- selene: deny(unused_variable)
        do
            local doC = 3
            do
                local doD = 4
                -- selene: allow(unused_variable)
                do
                    local doE = 5
                end
            end
        end

        local doF = 6
    end
end

-- selene: allow(unused_variable)
-- selene: deny(unused_variable)
print("WHAT DO YOU WANT FROM ME")

--[[
    selene: allow(unused_variable)
    selene: deny(unused_variable)
]]
print("THIS DOESNT HELP!!!")

print("Finale!")
