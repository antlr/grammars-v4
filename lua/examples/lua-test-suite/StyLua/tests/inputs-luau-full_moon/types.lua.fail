--!strict
local _fn3
type Identity<T> = T
type Array<T> = { [number]: T }
type Map<K, V> = { [K]: V }
type Function<T> = (...any) -> ...T
type Object = { x: number, y: number }
type Typeof = typeof(2 + 2 + _fn3())
type FetchResult = "alice" | "mallet"
type Element = { ["$$typeof"]: number }

type Callback1 = (string) -> number
type Callback2 = (string, string) -> number
type Callback3 = (string, string) -> (string, nil)
type Callback3a = (string, "error" | "success") -> (string, "weasel" | "basilisk")
type Callback4 = (string) -> (string) -> ()
type NetworkError = { message: string? } & { handled: true }

type Foo = {
	bar: number,
	baz: number,
}

local foo0: number = 3
local _foo1: number?
local _foo2: Array<string>
local _foo3: Map<number, "allow" | "deny">
local _bar0 = foo0 :: number
local _foo4: string, _bar1: string

local _union: number | string
local _multiUnion: number | string | nil

local _intersection: number & string
local _multiIntersection: number & string & nil

function _fn0(param: string): string
	return param
end

function _fn2(a: string, b: string, ...) end

local _fn3 = function(): number | nil
	return 3
end

local function _concat<T, S>(source: Array<T>, ...: Array<S> | S): Array<T> & Array<S>
    return (source :: any) :: Array<S> & Array<T>
end