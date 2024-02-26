Sub GotoStatementDemo() 
Dim Number, MyString 
 Number = 1
 If Number = 1 Then GoTo 1 Else GoTo 2
1: 
 MyString = "Number equals 1" 
 GoTo 3
2: 
 MyString = "Number equals 2" 
3: 
 Debug.Print MyString
End Sub
