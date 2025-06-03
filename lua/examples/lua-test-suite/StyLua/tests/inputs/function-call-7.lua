-- https://github.com/JohnnyMorganz/StyLua/issues/298
do
	return Roact.createElement(StyleContext.Provider, {
		value = styleObject,
	}, Roact.oneChild(self.props[Roact.Children]))
end
