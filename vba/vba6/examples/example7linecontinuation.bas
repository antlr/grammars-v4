Sub TestLineContinuedMemberCalls()
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
