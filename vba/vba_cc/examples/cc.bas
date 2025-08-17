Attribute VB_Name = "CCTEST"
#Const conDebug = 1

#If conDebuge = 1 Then 'Run debug code
    Debug.Print "oops"
#Elseif conDebug > 1 Then
    Debug.Print "help"
#Else
    #If Cdbl(Abs(conDebug)) < (1 Mod 3) Then
    
        foo = 2
    #Endif
#End If

Open pth for output as #ff
    Print #ff, cont
Close #ff

' The blocks can each be empty.
#if Win64 Then
#Elseif Win32 Then
#Else
#Endif

' The blocks can each be empty.
#if 1 / 2 = .5 Then
    foo = 4
#Endif
