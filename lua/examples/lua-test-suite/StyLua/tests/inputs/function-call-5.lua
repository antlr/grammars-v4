function GamepadModule.gamepadLinearToCurve(thumbstickPosition)
	return Vector2.new(
		math.clamp(
			math.sign(thumbstickPosition.X)
				* fromSCurveSpace(SCurveTransform(toSCurveSpace(math.abs(thumbstickPosition.X)))),
			-1,
			1
		),
		math.clamp(
			math.sign(thumbstickPosition.Y)
				* fromSCurveSpace(SCurveTransform(toSCurveSpace(math.abs(thumbstickPosition.Y)))),
			-1,
			1
		)
	)
end
