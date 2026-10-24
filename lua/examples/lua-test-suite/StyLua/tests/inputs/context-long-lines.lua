do
	local region = Region3.new(part.Position - (0.5 * part.Size), part.Position + (0.5 * part.Size))

	do
		do
			return function(...)
				callback(LOG_FORMAT:format(os.date("%H:%M:%S"), key, level, fmt.fmt(...)))
			end
		end
	end

	self.digits = math.ceil(math.log10(math.max(math.abs(self.props.maxValue), math.abs(self.props.minValue))))
end

local gamemodes, keysById, idsByKey = createDataIndex(script.AllGamemodes, validateGamemodeSchema)

HealthRegen.ValidateInstance = t.intersection(ComponentUtil.HasComponentValidator("Health"), ComponentUtil.HasComponentValidator("Target"))