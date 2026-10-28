-- https://github.com/JohnnyMorganz/StyLua/issues/405
do
	for _,v in ipairs({
		Kind.SELECTION_sET,
		Kind.DIRECTIVE,
		Kind.OEPRATION_DEFINITION,
		Kind.INLINE_FRAGMENT,
		Kind.FRAGMENT_DEFINITION,
		Kind.ARGUMENT,
	}) do
	end
end

do
	for _,v in ipairs {
		Kind.SELECTION_sET,
		Kind.DIRECTIVE,
		Kind.OEPRATION_DEFINITION,
		Kind.INLINE_FRAGMENT,
		Kind.FRAGMENT_DEFINITION,
		Kind.ARGUMENT,
	} do
	end
end

-- These cases should not hug:
do
	for _,v in ipairs({
		Kind.SELECTION_sET,
		Kind.DIRECTIVE,
		Kind.OEPRATION_DEFINITION,
		Kind.INLINE_FRAGMENT,
		Kind.FRAGMENT_DEFINITION,
		Kind.ARGUMENT,
	}) -- comment
	do
	end
end

do
	for _,v in ipairs(foo and {
		Kind.SELECTION_sET,
		Kind.DIRECTIVE,
		Kind.OEPRATION_DEFINITION,
		Kind.INLINE_FRAGMENT,
		Kind.FRAGMENT_DEFINITION,
		Kind.ARGUMENT,
	} or bar)
	do
	end
end

do
	for _,v in call(function()
		return { test, another }
	end) do
	end
end

do
	for _,v in call({
		Kind.SELECTION_sET,
		Kind.DIRECTIVE,
		Kind.OEPRATION_DEFINITION,
		Kind.INLINE_FRAGMENT,
		Kind.FRAGMENT_DEFINITION,
		Kind.ARGUMENT,
	}), anotherThing do
	end
end

do
	for _,v in call({
		Kind.SELECTION_sET,
		Kind.DIRECTIVE,
		Kind.OEPRATION_DEFINITION,
		Kind.INLINE_FRAGMENT,
		Kind.FRAGMENT_DEFINITION,
		Kind.ARGUMENT,
	}, "failure case") do
	end
end

do
	for _,v in call({
		Kind.SELECTION_sET,
		Kind.DIRECTIVE,
		Kind.OEPRATION_DEFINITION,
		Kind.INLINE_FRAGMENT,
		Kind.FRAGMENT_DEFINITION,
		Kind.ARGUMENT,
	})(true) do
	end
end

do
	for _,v in x.y.z.call({
		Kind.SELECTION_sET,
		Kind.DIRECTIVE,
		Kind.OEPRATION_DEFINITION,
		Kind.INLINE_FRAGMENT,
		Kind.FRAGMENT_DEFINITION,
		Kind.ARGUMENT,
	}) do
	end
end

do
	for _,v in foo and call({
		Kind.SELECTION_sET,
		Kind.DIRECTIVE,
		Kind.OEPRATION_DEFINITION,
		Kind.INLINE_FRAGMENT,
		Kind.FRAGMENT_DEFINITION,
		Kind.ARGUMENT,
	}) or otherCall() do
	end
end

do
	for _,v in (foo({
		Kind.SELECTION_sET,
		Kind.DIRECTIVE,
		Kind.OEPRATION_DEFINITION,
		Kind.INLINE_FRAGMENT,
		Kind.FRAGMENT_DEFINITION,
		Kind.ARGUMENT,
	}) or true) do
	end
end

do
	for _,v in "thissssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss" do
	end
end
