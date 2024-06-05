VERSION 5.00
Begin {C62A69F0-16DC-11CE-9E98-00AA00574A4F} Login 
   Caption         =   "Please Log In"
   ClientHeight    =   1920
   ClientLeft      =   120
   ClientTop       =   465
   ClientWidth     =   2295
   OleObjectBlob   =   "Login.frx":0000
   StartUpPosition =   1  'CenterOwner
End
Attribute VB_Name = "Login"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Public Ready As Boolean

Private Sub Form_Load()
Me.Show
Ready = False
Call Wait
'Label1.Visible = True
End Sub

Public Function Wait()
Do While Ready = False
    DoEvents
Loop
End Function

Private Sub LoginButton_Click()
    Ready = True
    Me.Hide
End Sub
