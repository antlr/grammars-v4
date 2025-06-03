return cframe * Vector3.new( -- Clamp & transform into world space
		math.clamp(transform.X, -halfSize.X, halfSize.X),
		math.clamp(transform.Y, -halfSize.Y, halfSize.Y),
		math.clamp(transform.Z, -halfSize.Z, halfSize.Z)
	), cframe.Position
