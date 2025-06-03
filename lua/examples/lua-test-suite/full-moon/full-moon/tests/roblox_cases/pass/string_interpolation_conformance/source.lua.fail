local function assertEq(left, right)
	assert(typeof(left) == "string", "left is a " .. typeof(left))
	assert(typeof(right) == "string", "right is a " .. typeof(right))

	if left ~= right then
		error(string.format("%q ~= %q", left, right))
	end
end

assertEq(`hello {"world"}`, "hello world")
assertEq(`Welcome {"to"} {"Luau"}!`, "Welcome to Luau!")

assertEq(`2 + 2 = {2 + 2}`, "2 + 2 = 4")

assertEq(`{1} {2} {3} {4} {5} {6} {7}`, "1 2 3 4 5 6 7")

local combo = {5, 2, 8, 9}
assertEq(`The lock combinations are: {table.concat(combo, ", ")}`, "The lock combinations are: 5, 2, 8, 9")

assertEq(`true = {true}`, "true = true")

local name = "Luau"
assertEq(`Welcome to {
	name
}!`, "Welcome to Luau!")

local nameNotConstantEvaluated = (function() return "Luau" end)()
assertEq(`Welcome to {nameNotConstantEvaluated}!`, "Welcome to Luau!")

assertEq(`This {localName} does not exist`, "This nil does not exist")

assertEq(`Welcome to \
{name}!`, "Welcome to \nLuau!")

assertEq(`empty`, "empty")

assertEq(`Escaped brace: \{}`, "Escaped brace: {}")
assertEq(`Escaped brace \{} with {"expression"}`, "Escaped brace {} with expression")
assertEq(`Backslash \ that escapes the space is not a part of the string...`, "Backslash  that escapes the space is not a part of the string...")
assertEq(`Escaped backslash \\`, "Escaped backslash \\")
assertEq(`Escaped backtick: \``, "Escaped backtick: `")

assertEq(`Hello {`from inside {"a nested string"}`}`, "Hello from inside a nested string")

assertEq(`1 {`2 {`3 {4}`}`}`, "1 2 3 4")

local health = 50
assert(`You have {health}% health` == "You have 50% health")

local function shadowsString(string)
	return `Value is {string}`
end

assertEq(shadowsString("hello"), "Value is hello")
assertEq(shadowsString(1), "Value is 1")

assertEq(`\u{0041}\t`, "A\t")

return "OK"
