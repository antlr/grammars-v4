local dispatch

if math.random() > 0.5 then
    dispatch = function() end
else
    dispatch = function(a) end
end

dispatch()
dispatch(1)
dispatch(1, 2)

local hasVararg

if math.random() > 0.5 then
    hasVararg = function(a, b, ...) end
else
    hasVararg = function(a, b, c, d) end
end

hasVararg(1)
hasVararg(1, 2, 3, 4, 5)
