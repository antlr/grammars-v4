Dim CondA As String
Dim CondB As String

Select Case CondA
	Case "A"
		Beep
	Case "B"
		Select Case CondB
			Case "A"
				Beep
			Case "B"
				Beep
			Case Else
				Beep
		End Select
	Case Else
		Beep
End Select
