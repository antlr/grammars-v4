-- https://github.com/JohnnyMorganz/StyLua/issues/287: whitespace around tokens causes inconsistency
local foo = {
	getTileProps = function(tile)
		local result = {
			adId = not GetFFlagLuaAppAddUniverseIdToGameImpress()         and           (tile.props.entry and tile.props.entry.adId)
				or nil,
		}
	end,
}
