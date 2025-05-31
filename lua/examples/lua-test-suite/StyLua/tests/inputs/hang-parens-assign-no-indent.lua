-- https://github.com/JohnnyMorganz/StyLua/issues/274
local tbl = {
	key = long_variable_name,
	key = long_variable_name,
	key = long_variable_name,
	key = long_variable_name,
	key = long_variable_name,
	key = long_variable_name,
	key = long_variable_name,
}
function_call(
	long_variable_name,
	long_variable_name,
	long_variable_name,
	long_variable_name,
	long_variable_name,
	long_variable_name,
	long_variable_name,
	long_variable_name
)
local test = (
	long_variable_name
	+ long_variable_name
	+ long_variable_name
	+ long_variable_name
	+ long_variable_name
	+ long_variable_name
	+ long_variable_name
	+ long_variable_name
	+ long_variable_name
	+ long_variable_name
)

-- Multiple assigns
local test, test2 = (
	long_variable_name
	+ long_variable_name
	+ long_variable_name
	+ long_variable_name
	+ long_variable_name
	+ long_variable_name
	+ long_variable_name
	+ long_variable_name
	+ long_variable_name
	+ long_variable_name
), (
	long_variable_name
	+ long_variable_name
	+ long_variable_name
	+ long_variable_name
	+ long_variable_name
	+ long_variable_name
	+ long_variable_name
	+ long_variable_name
	+ long_variable_name
	+ long_variable_name
)

-- Multiple assigns of different types
local test, test2 = foo and bar or baz, (
	long_variable_name
	+ long_variable_name
	+ long_variable_name
	+ long_variable_name
	+ long_variable_name
	+ long_variable_name
	+ long_variable_name
	+ long_variable_name
	+ long_variable_name
	+ long_variable_name
)
