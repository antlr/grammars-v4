-- hug table braces with parentheses

print({ foo_variable = "some long value", foo_variable = "some long value", foo_variable = "some long value", foo_variable = "some long value", })

print({ foo_variable = "somenge", foo_variable = "malue", foo_variable = "alueeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee" })

-- but not if there is a comment present

foo( -- test
   { bar })

foo( -- test
	{
	   bar
	}
)

