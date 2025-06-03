local foo = {
	"foo",
	"bar",
	[1] = true,
}

local bar = {
	"foo",
	"bar",
	[123] = 15,
	[false] = true,
	"baz",
	"foo",
	[3] = "5",
}

local baz = {
	[0] = true,
	[1] = false,
	[0] = false,
}
