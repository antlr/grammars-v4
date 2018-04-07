Function SomeFunction()
	Dim Text As String
	Text = "Hello World"

	Goto LineLabel1
	
LineLabel1:
	Text = "1"
LineLabel2:
	Text = "2"
	Debug.Print Text 
End Function


SomeFunction
