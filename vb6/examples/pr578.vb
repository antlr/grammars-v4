VERSION 1.0 CLASS
BEGIN
MultiUse = -1 'True
Persistable = 0 'NotPersistable
DataBindingBehavior = 0 'vbNone
DataSourceBehavior = 0 'vbNone
MTSTransactionMode = 0 'NotAnMTSObject
END
Attribute VB_Name = "ConstDirective"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Const EvalDirective = 0

Public Function EvalConstDirective() As Long
#If EvalDirective Then
EvalConstDirective = 1
#Else
EvalConstDirective = 0
#End If
End Function

