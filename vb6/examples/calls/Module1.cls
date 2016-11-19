VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Module1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Public SomeVariable
Public SomeModule As New Module1

Function Function1()

End Function

Function Function2(I, J)

End Function

Sub Sub1()

End Sub

Sub Sub2(I, J)

End Sub

Function GetModule() As Module1
    Dim Module As New Module1
    Set GetModule = Module
End Function
