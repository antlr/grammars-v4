do
	do
		local CreatedDirection = (
			DirectionalCF * CFrame.fromOrientation(0, 0, RNG:NextNumber(0, TAU)) * CFrame.fromOrientation(
				math.rad(RNG:NextNumber(
					GunMainConfiguration.BulletMinSpreadAngle or GlobalConfiguration.DEFAULT_MIN_SPREAD_ANGLE,
					GunMainConfiguration.BulletMaxSpreadAngle or GlobalConfiguration.DEFAULT_MAX_SPREAD_ANGLE
				)),
				0,
				0
			)
		).LookVector
	end
end

		