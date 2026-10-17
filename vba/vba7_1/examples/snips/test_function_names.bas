Attribute VB_Name = "TestFunctionNames"
' The word "String" is a reserved type, but is allowed
' as a function name
Function Baz()
    Foo = String(5, 42)
    Bar = Date
    Foo2 = String$(5, 42)
    Bar2 = Date$
    Foo3 = Abs(-1)
End Function
