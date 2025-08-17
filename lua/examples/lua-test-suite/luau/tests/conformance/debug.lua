-- This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
print "testing debug library"

-- traceback
function foo(...)
	return debug.traceback(...)
end

function bar()
	coroutine.yield()
end

assert(foo():find("foo") > 0)
assert(foo("hello"):find("hello") > 0)
assert(foo("hello"):find("foo") > 0)
assert(foo("hello", 2):find("hello") > 0)
assert(foo("hello", 2):find("foo") == nil)

local co = coroutine.create(bar)
coroutine.resume(co)

assert(debug.traceback(co):find("bar") > 0)
assert(debug.traceback(co, "hello"):find("hello") > 0)
assert(debug.traceback(co, "hello"):find("bar") > 0)
assert(debug.traceback(co, "hello", 2):find("hello") > 0)
assert(debug.traceback(co, "hello", 2):find("bar") == nil)

-- traceback for the top frame
function halp(key, value)
	local t = {}
	t[key] = value -- line 30
	return t
end

local co2 = coroutine.create(halp)
coroutine.resume(co2, 0 / 0, 42)

assert(debug.traceback(co2) == "debug.lua:31 function halp\n")
assert(debug.info(co2, 0, "l") == 31)
assert(debug.info(co2, 0, "f") == halp)

-- info errors
function qux(...)
	local ok, err = pcall(debug.info, ...)
	assert(not ok)
	return err
end

assert(qux():find("function or level expected"))
assert(qux(1):find("string expected"))
assert(qux(-1):find("level can't be negative"))
assert(qux(1, "?"):find("invalid option"))
assert(qux(1, "nn"):find("duplicate option"))
assert(qux(co):find("function or level expected"))
assert(qux(co, 1):find("string expected"))

-- info single-arg returns
function baz(...)
	return debug.info(...)
end

assert(baz(0, "n") == "info")
assert(baz(1, "n") == "baz")
assert(baz(2, "n") == "") -- main/anonymous
assert(baz(3, "n") == nil)
assert(baz(0, "s") == "[C]")
assert(baz(1, "s") == "debug.lua")
assert(baz(0, "l") == -1)
assert(baz(1, "l") > 42)
assert(baz(0, "f") == debug.info)
assert(baz(1, "f") == baz)
assert(baz(0, "a") == 0)
assert(baz(1, "a") == 0)
assert(baz(co, 1, "n") == "bar")
assert(baz(co, 2, "n") == nil)
assert(baz(math.sqrt, "n") == "sqrt")
assert(baz(math.sqrt, "f") == math.sqrt) -- yes this is pointless

local t = { foo = function() return 1 end }
assert(baz(t.foo, "n") == "foo")

-- info multi-arg returns
function quux(...)
	return {debug.info(...)}
end

assert(#(quux(1, "nlsf")) == 4)
assert(quux(1, "nlsf")[1] == "quux")
assert(quux(1, "nlsf")[2] > 64)
assert(quux(1, "nlsf")[3] == "debug.lua")
assert(quux(1, "nlsf")[4] == quux)

-- info arity
function quuz(f)
	local a, v = debug.info(f, "a")
	return tostring(a) .. " " .. tostring(v)
end

assert(quuz(math.cos) == "0 true") -- C functions are treated as fully variadic
assert(quuz(function() end) == "0 false")
assert(quuz(function(...) end) == "0 true")
assert(quuz(function(a, b) end) == "2 false")
assert(quuz(function(a, b, ...) end) == "2 true")

-- info linedefined & line
function testlinedefined()
	local line = debug.info(1, "l")
	local linedefined = debug.info(testlinedefined, "l")
	assert(linedefined + 1 == line)
end

testlinedefined()

return 'OK'
