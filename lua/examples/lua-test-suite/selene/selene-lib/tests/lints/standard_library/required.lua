-- ipairs requires an argument--this should fail.
ipairs()

-- setmetatable requires a first argument, but not a second one. this should still fail.
setmetatable()

-- This, however, should not.
setmetatable({})

-- nil counts as filling in an unrequired argument.
setmetatable({}, nil)
