' sub calls
Call SubExample1
SubExample1

Call SubExample2("some arg")
SubExample2 "some arg"

' function calls
Call FunctionExample1
FunctionExample1

Call FunctionExample2(2.0, 3, 4, 2)
FunctionExample2 2.0, 3, 4, 2

Sub SubExample1()
	' sub body
End Sub

Sub SubExample2(Arg1)
	' sub body
End Sub

Function FunctionExample1()
	' function body
End Function

Function FunctionExample2(ByVal FirstArg As Double, ParamArray AdditionalArgs())
	' function body
End Function
