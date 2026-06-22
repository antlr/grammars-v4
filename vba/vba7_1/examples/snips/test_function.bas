Attribute VB_Name = "foo"
' Missing coverage for endLabel
Public Static Function Module()

End Function

Private Function Module As Boolean Static
    callAnotherSub:

' An identifierStatementLabel is recognized as a callStatement
' when in the endLabel position.
label1:End Function ' Comment

Global Function Module#(Foo As Boolean, Optional Bar=5)
' A lineNumberLabel is recognised correctly in the
' endLabel position, but not as an endlabel.
2: End Function : REM sefkljnsa
