Roact.createElement("ImageLabel", {
	Size = UDim2.new(
		0,
		TextService:GetTextSize(self.props.PhysicalTool.Name, 16, Enum.Font.SourceSansBold, Vector2.new(100000, 100000)).X
			+ 10,
		0,
		20
	),
})