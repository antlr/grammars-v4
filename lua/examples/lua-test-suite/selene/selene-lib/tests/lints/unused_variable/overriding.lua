local a = 1

-- Variables defined inside the function should still lint
local function foo()
    local b = 1
    if true then
        b = b + 1

        local c = 1
        if true then
            c = c + 1
        end
    end

    local d = 1
    return function()
        return d
    end, function()
        d = d + 1
    end
end

return function()
    return a
end, function(arg)
    a = arg
end
