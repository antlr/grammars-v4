-- https://github.com/JohnnyMorganz/StyLua/issues/471
local foo = {
	x = props.Item.Type == "Crystal"
		and utf8.char(0x221e) -- Infinite symbol
		or nil,
}
