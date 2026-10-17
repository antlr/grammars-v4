local App = Roact.createElement("Frame", {
	Size = UDim2.new(0,0,0,0),
	Position = UDim2.new(0,0,0,0)
}, {
	Child1 = Roact.createElement("TextLabel", {
		Text = "foo",
		AnchorPoint = Vector2.new(0, 0), -- comment
		foo = bar
	}),

	Child2 = Roact.createElement("TextLabel", {
		Text = "foo",
		AnchorPoint = Vector2.new(0, 0),
		foo = bar
	}),
})

doSomething({
	aLongKey = aLongValue,
	anotherLongKey = anotherLongValue
}, notATableLiteral, {
	aLongKey = anotherLongValue,
	anotherLongKey = aLongValue
})

table.sort(recommendedDeveloperProducts, function(a, b)
	return a.amount < b.amount
end)