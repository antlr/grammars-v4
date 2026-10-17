-- Tests for regressions of #116
local foo = function(bar)
    if bar then
        print(bar)
    else
        -- This causes problems
    end
end

local baz = function(bar)
    print(bar)
end
