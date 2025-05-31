-- Since functions can pass multiple parameters through a function if they're the last ones to be called,
-- selene needs to be able to recognize this and not error.

-- This should not lint, since `unpack(stuff)` could pass in multiple parameters.
math.max(unpack(stuff))

-- This should lint, since functions only pass multiple parameters if they are the last argument.
-- debug.setlocal is used because it takes three required parameters.
debug.setlocal(unpack(stuff), 2)

-- This is still wrong since it has too many parameters.
string.upper("text", unpack(stuff))
