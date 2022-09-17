Private Sub cmdClear_Click()
    txtHello.Text = ""
End Sub
 
Private Sub cmdExit_Click()
    End
End Sub
 
Private Sub cmdHello_Click()
    txtHello.Text = "Hello World!"
    With txtHello
        .Font = "Arial"
        .FontSize = 16
        .ForeColor = vbBlue
    End With
End Sub

