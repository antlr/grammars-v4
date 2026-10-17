local dispatch

local function setDispatch()
    dispatch = function() end
end

dispatch("foo", "bar")
