_G.foo = 1

do
    -- This triggers another lint, but it shouldn't for this one
    local _G = {}

    _G.bar = 1
end
