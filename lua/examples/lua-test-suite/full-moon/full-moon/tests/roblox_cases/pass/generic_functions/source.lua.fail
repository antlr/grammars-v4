--!strict
function _foo<x, y>()
end

local function _bar<x>()
end

export type Foo0 = {
	bar: <T>(
		a: T,
		b: nil | number | boolean
	) -> T,
}
local _baz
export type Foo1 = {
	bar: <T>(
		a: T,
		b: nil | number | boolean
	) -> ((arg0: T) -> ())?,
}

_baz = function<T>(a: T, b: number | boolean | nil): nil | T
    return nil
end