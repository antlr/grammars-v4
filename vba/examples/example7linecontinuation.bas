Sub TestLineContinuedMemberCalls(Optional Foo = 1, _
        Optional Bar = 2)
'   Valid line continuation syntax with
'   method / property chaining.
    With A.B _
        .B
    End With

    With A _
        .B
    End With

    With A. _
        B
    End With
End Sub
