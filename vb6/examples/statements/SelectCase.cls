Dim Grade As String
Grade = "B"

Select Case Grade
	Case "A"
		Beep
	Case "B"
		Beep
	Case Else
		Beep
End Select

Dim Percent
Percent = 50

Select Case Percent
	Case 0 To 25
		Beep
	Case 26 To 49, 50
	Case 51 To 75
		Beep
	Case 76 To 80, 81, 82 To 89, Is > 90
	Case Else
		Beep
End Select

Select Case Grade
	Case "A"
		Beep
	Case Else
		Beep
	Case "B"
		Beep
	Case Else
		Beep
End Select
