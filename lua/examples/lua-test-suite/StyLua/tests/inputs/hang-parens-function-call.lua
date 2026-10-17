-- https://github.com/JohnnyMorganz/StyLua/issues/456
do
	do
		WallCollisionPart.Position = Vector3.new(
			WallCollisionPart.Position.X,
			(
				(GeneratedTower.Top.PrimaryPart.Position.Y - GeneratedTower.Top.PrimaryPart.Size.Y / 2)
				+ (GeneratedTower.Bottom.PrimaryPart.Position.Y - GeneratedTower.Bottom.PrimaryPart.Size.Y / 2)
			) / 2,
			WallCollisionPart.Position.Z
		)

		WallCollisionPart.Position = Vector3.new(
			WallCollisionPart.Position.X,
			(
				(GeneratedTower.Top.PrimaryPart.Position.Y - GeneratedTower.Top.PrimaryPart.Size.Y / 2)
				+ (GeneratedTower.Bottom.PrimaryPart.Position.Y - GeneratedTower.Bottom.PrimaryPart.Size.Y / 2)
			) / AComplexFunctionCall(withAReallyLongArgument, "this is a complex function call message", anotherReallyLongArgument),
			WallCollisionPart.Position.Z
		)

		WallCollisionPart.Position = Vector3.new(
			WallCollisionPart.Position.X,
			AComplexFunctionCall(withAReallyLongArgument, "this is a complex function call message", anotherReallyLongArgument) /
			(
				(GeneratedTower.Top.PrimaryPart.Position.Y - GeneratedTower.Top.PrimaryPart.Size.Y / 2)
				+ (GeneratedTower.Bottom.PrimaryPart.Position.Y - GeneratedTower.Bottom.PrimaryPart.Size.Y / 2)
			),
			WallCollisionPart.Position.Z
		)
	end
end
