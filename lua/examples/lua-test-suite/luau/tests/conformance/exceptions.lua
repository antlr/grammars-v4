-- This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
print('testing lua_exception')

-- Verify that no exception is generated
function empty_function()
end

function pass_number_to_error()
    -- Verify the error value of 42 is part of the exception's string.
    error(42)
end

function pass_string_to_error()
    -- Verify the error value of "string argument" is part of the exception's string.
    error("string argument")
end

function pass_table_to_error()
    -- Pass a table to `error`. A table is used since it is won't be
    -- convertable to a string using `lua_tostring`.
    error({field="value"})
end

function infinite_recursion_error()
    -- Generate a stack overflow error
    infinite_recursion_error()
end

function large_allocation_error()
    -- Create a table that will require more memory than the test's memory
    -- allocator will allow.
    table.create(1000000)
end

return('OK')
