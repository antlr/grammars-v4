bar = true

local foo = 1
foo = 2

local bar = 1
bar = 2

pcall(function()
    foo = 3
    baz = 1
end)

_ = 3
