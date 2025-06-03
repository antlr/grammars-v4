-- https://github.com/JohnnyMorganz/StyLua/issues/397

--[[opening type comment]]
type Doo<
	T --[[ per-generic argument comment]]
> =
	--[[ opening RHS comment]]
	string --[[ per-RHS comment]]

type Foo<T = --[[leading]]
string
--[[trailing]]> = { baz: T, }

type Bar<T
--[[ Trailing comment ]]> = {}

-- This is a comment before
type Foo = --[[ Comment before Bar ]]
Bar<--[[ Before X ]]
X, --[[ After X ]]
--[[ Before Y ]]
Y, --[[ After Y ]]
--[[ Before Z ]]
Z
--[[ After Z ]]> -- This is a comment after

--[[comment]]
type Doo
--[[comment]]
<
--[[comment]]
T
--[[comment]]
>
--[[comment]]
=
--[[comment]]
string
--[[comment]]
