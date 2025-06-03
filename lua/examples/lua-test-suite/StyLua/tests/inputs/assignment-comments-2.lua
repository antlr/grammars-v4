-- https://github.com/JohnnyMorganz/StyLua/issues/416
local variable = call(somethingToCall().foo.bar.baz, "some super long string that will stay on this line aaaaaaaaaaaaaaaaa") -- a comment
	.. "another string"
