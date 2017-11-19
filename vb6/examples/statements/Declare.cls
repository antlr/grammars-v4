Private Declare Sub subName1 Lib "MyLib" Alias "alias1" (arg1, arg2, ParamArray arg3)

Public Declare Sub subName2 Lib "MyLib" Alias "alias2" (arg1, arg2 As Long, ByRef arg3)

Private Declare Function functionName1 Lib "MyLib" (Optional arg1, ByVal arg2, arg3) As Currency

Public Declare Function functionName2 Lib "MyLib" Alias "alias3" ()


Private Declare Function GetComputerName Lib "kernel32" Alias "GetComputerNameA" (ByVal lpBuffer As String, nSize As Long) As Long

Declare Function SetComputerName Lib "kernel32" _
  Alias "SetComputerNameA" ( _
  ByVal lpComputerName As String) As Long
  
Private Declare Function ExitWindowsEx Lib "user32" ( _
  ByVal uFlags As Long, _
  ByVal dwReserved As Long) As Long
  
Declare Function Func Lib "Foo.dll" (a$)
