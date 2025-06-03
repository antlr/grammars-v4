-- https://github.com/JohnnyMorganz/StyLua/issues/386
repeat x = x + 1 until z * (
	x + y -- comment
)

repeat x = x + 1 until z * (
	x + y -- comment
) < 2
