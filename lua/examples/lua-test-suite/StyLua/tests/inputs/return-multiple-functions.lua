-- https://github.com/JohnnyMorganz/StyLua/issues/302
return function()
	if overrides == nil then
		setupOverrides()
	end

	if overrides[key] == nil then
		return value
	end

	return overrides[key]
end, function(callback)
	overrideWatchers[key] = callback
end
