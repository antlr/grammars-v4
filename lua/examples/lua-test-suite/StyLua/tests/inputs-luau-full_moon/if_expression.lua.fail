local x = if foo then foo.x else 5
local y = (if x then x.indices else create()):update(if shouldUpdate then information else defaults)
local z = (if bar then foo.y else 5) :: number

local a = if foo then foo.x elseif bar then bar.x else 5
local b = if foo then if bar then bar else foo else 5
local c = if foo then (foo.x :: number) elseif bar then bar.x()() else 5
local d = if foo then 5 else baz :: number

if if foo then bar else baz then
end