Private Type TypeName1
	Variable1 As Integer
	Variable2 As Double
End Type

Public Type TypeName2
	Variable1 As Currency
	
	Variable2 As String
	Variable3
	Variable4 As TypeName1
End Type


Dim typeElement As TypeName1
typeElement.Variable1 =1

Dim typeElement2 As TypeName2
typeElement2.Variable2 = "test"


Dim typeArray(1 To 10) As TypeName2
typeArray(1).Variable2 = "hello world"
