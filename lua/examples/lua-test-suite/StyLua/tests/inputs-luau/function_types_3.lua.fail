function foo(args: {
	id: string,
	fenderer: SubBassGuitar,
	fendererInterface: BasGuitarInterface,
})
	print("foo")
end

function foo(args: {
	id: string,
	fenderer: SubBassGuitar,
	fendererInterface: BasGuitarInterface,
}, type: string)
	print("foo")
end

local subs = {
	amps.sub("fenderer-nations", function(
		args: {
		id: string,
		fenderer: SubBassGuitar,
		fendererInterface: BasGuitarInterface,
	}
	)
		print("test")
	end),
}
