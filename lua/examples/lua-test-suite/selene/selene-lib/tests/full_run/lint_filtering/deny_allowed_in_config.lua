-- The test for this allows unused_variable, so this should not lint
local foo = 1

-- selene: deny(unused_variable)
local bar = 1
