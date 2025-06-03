local foo     =      bar
do
    -- stylua: ignore start
    local bar   =     baz
    -- stylua: ignore end
    local bar   =     baz
end
local bar   =     baz
