setmetatable({
	_words = words,
	_morewords = words,
	_evenmorewords = words,
	_words = words,
	_morewords = words,
	_evenmorewords = words,
}, Class)

foo({
	foo = bar,
}, baz, {
	bar = baz,
})

Roact.createElement("Frame", {
	foo = bar, bar = baz,
}, self.props[Roact.Children])