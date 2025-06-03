local x = function()
	return (function()
		local z = 3 + 4
		return complexCall(z)
	end)
end

local x = function()
	return (function()
		local z = 3 + 4
		return complexCall(z)
	end)()
end

local x = function()
	return not (function()
		local z = 3 + 4
		return complexCall(z)
	end)()
end

local x = function()
	return { function() return true end }
end

local x = function()
	return { [(function() return false end)()] = true }
end

local x = function()
	return call "string"
end

local x = function()
	return call { function() end }
end

local x = function()
	(function() return x[5] end)[10] = 5
end

local x = function()
	y[function() end] = z
end

local x = function()
	break
end
