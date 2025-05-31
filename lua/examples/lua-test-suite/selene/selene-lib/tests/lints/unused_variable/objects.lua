-- Mutated, but never used
local foo = {}
foo.a = 1

-- This is allowed, since mutating it might be doing something
local bar = call()
bar.a = 1
