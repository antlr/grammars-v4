--!strict
type Foo = (...number) -> ()
type Baz = (string, ...Foo) -> ...Foo
type Bar = (...number) -> (string, ...number) -> ...any
type Boom = (..."hit" | "miss") -> (string, ...("critical" | "weak" | "normal")) -> ...("dead" | "alive")

function _bar(...: number): ...number | string end

local f: Boom = function(...)
	return function(x, ...)
		return "alive", "dead"
	end
end

f("hit")

local Boo = {}
function Boo:f(name: string, ...: number): () -> (string, ...Foo) -> ()
	return function()
		return function(_x: string, ...: Foo) end
	end
end
