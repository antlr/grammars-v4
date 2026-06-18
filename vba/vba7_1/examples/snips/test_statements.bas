Attribute VB_Name = "statements"

Public Static Function Module()
Attribute Module.VB_Description = "My Description"
Attribute Module.VB_UserMemId = 0
    ' Call
    Call bar
    bar.Type baz:="7", foo:=5
    Call baz(4)

    'While
    While True
    Wend

    ' For
    For i = 0 To 7 Step 2
    Next
    
    For i = 0 To 7 Step 2
    Next i

    For i = 0 To 7 Step 2
        For j = 0 To 7
            sum = sum + i * j
        Next j
        for each j in k
next j, i

select case foo
    case bar: baz
    case baz: quix
end select

    If True Then
        ' Test out the end statement
        End
    ElseIf True Then 'skip
        Call foo 'skip
    Else
        Call bar
    EndIf

'Multiple range-clause elements can be present in a case-clause.
Select Case Foo
    case 1, 2, 3, 4
        bar = 3
    case 5, 6, 7
        baz = 4
    case is > 3
End Select


' The Input function accepts a marked file number as a prameter
Open foo for Input as #ff
    bar = Input(LOF(ff), #ff)
Close #ff

' No WS around operators
Foo = (foo)/count
Debug.Print A B, C; D & E
End Function
