VERSION 5.00
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "COMDLG32.OCX"
Begin VB.Form Form1 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Virtually Mapped File Hex Viewer"
   ClientHeight    =   5070
   ClientLeft      =   150
   ClientTop       =   795
   ClientWidth     =   10215
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   5070
   ScaleWidth      =   10215
   StartUpPosition =   3  'Windows Default
   Begin VB.VScrollBar VScroll1 
      Height          =   4815
      LargeChange     =   20
      Left            =   9855
      Max             =   100
      Min             =   -32768
      TabIndex        =   0
      TabStop         =   0   'False
      Top             =   120
      Value           =   -32768
      Width           =   255
   End
   Begin VB.PictureBox Picture2 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BeginProperty Font 
         Name            =   "Courier"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H80000008&
      Height          =   4815
      Left            =   120
      ScaleHeight     =   4785
      ScaleWidth      =   1065
      TabIndex        =   2
      Top             =   120
      Width           =   1095
   End
   Begin VB.PictureBox Picture1 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BeginProperty Font 
         Name            =   "Courier"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H80000008&
      Height          =   4815
      Left            =   1200
      ScaleHeight     =   4785
      ScaleWidth      =   8625
      TabIndex        =   1
      Top             =   120
      Width           =   8655
   End
   Begin MSComDlg.CommonDialog CD 
      Left            =   5760
      Top             =   4560
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
   End
   Begin VB.Menu mnuFile 
      Caption         =   "&File"
      Begin VB.Menu mnuFileOpen 
         Caption         =   "&Open"
      End
   End
End
Attribute VB_Name = "Form1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
''
' This demonstrates techniques for using the MemoryMappedFile class
' and some additional classes and functions.
'
Option Explicit

Private Const LINES_PER_PAGE    As Long = 24
Private Const BYTES_PER_LINE    As Long = 32

Private mFile               As MemoryMappedFile
Private mStream             As MemoryMappedViewStream
Private mTopLine            As Long
Private mPageBuilder        As New StringBuilder
Private mLineNumberBuilder  As New StringBuilder
Private mReader             As BinaryReader


''
' Lets load up the file, shall we?
Private Sub LoadFile(ByVal FileName As String)
    ' Get the file mapped to memory and byte array accessible.
    Call OpenFile(FileName)
    
    ' This will overflow on 2+ meg files because of the scrollbar limit.
    ' We want the ceiling so we can count for any fractional portion.
    '
    ' We offset it by -32768 to utilize the full range of the
    ' limited scrollbar control.
    Dim MaxLine As Long
    
    MaxLine = Ceiling(mStream.Length / BYTES_PER_LINE) - (32768 + LINES_PER_PAGE)
    If MaxLine < -32768 Then MaxLine = -32768
    VScroll1.Max = MaxLine
    VScroll1.Value = VScroll1.Min
    
    ' And update the display.
    Call UpdateDisplay
End Sub

''
' Creates a new memory mapping of a file and makes it byte array accessible.
Private Sub OpenFile(ByVal FileName As String)
    ' Be sure to release the previous file.
    Call CloseFile
    
    ' Create our memory mapped file.
    Set mFile = MemoryMappedFile.CreateFromFile(FileName)
    
    ' And retrieve a accessor for the file.
    Set mStream = mFile.CreateViewStream
    Set mReader = NewBinaryReader(mStream, LeaveOpen:=True)
End Sub

''
' Build up the text to be displayed in the two picture controls.
' This gives us a virtual view of the mapped file.
Private Sub DisplayPage()
    ' Reset our builders to clear anything in them.
    mPageBuilder.Length = 0
    mLineNumberBuilder.Length = 0
    
    ' Where do we start in the byte array?
    Dim Index As Long
    Index = mTopLine * BYTES_PER_LINE
    mStream.Position = Index
    
    Dim i As Long
    For i = 1 To BYTES_PER_LINE * LINES_PER_PAGE Step 4
        ' We can't convert less than 4 bytes at a time, so
        ' make sure we haven't run out of 4 byte chunks.
        If Index + 4 > mStream.Length Then Exit For
        
        ' Add the line number for the current line only if
        ' we are at the beginning of the line currently being built.
        If Index Mod 32 = 0 Then Call mLineNumberBuilder.AppendFormat("{0:X8}" & vbCrLf, Index)
        
        ' Convert a 4 byte chunk to a vbLong and append
        ' it to the text as a hex value with atleast 8 characters.
'        Call mPageBuilder.AppendFormat("{0:X8} ", BitConverter.ToLong(mBytes, Index))
        Call mPageBuilder.AppendFormat("{0:X8}", mReader.ReadInt32)
        
        ' Move to the next 4 byte chunk.
        Index = Index + 4
        
        ' If we have reached the end of the line, then start a new line.
        If Index Mod 32 = 0 Then Call mPageBuilder.AppendString(vbCrLf)
    Next i
    
    ' Check if our index is within the last 4 bytes of the
    ' end of the array. If so, we have to manually append
    ' the remaining bytes manually, since we didn't have
    ' a 4 byte chunk to append previously.
    If Index <= mStream.Length And Index + 4 > mStream.Length Then
        ' Loop through the remaining bytes backwards so
        ' we can build up a final vbLong value.
        For i = mStream.Length To Index Step -1
            mStream.Position = i
            
            Dim j As Long
            j = j * &H100 + mStream.ReadByte
        Next i
        
        ' Append the remaining byte values.
        Call mPageBuilder.AppendFormat("{0:X8}", j)
    End If
    
    ' Display the hex mapped values.
    Picture1.Cls
    Picture1.Print mPageBuilder.ToString
    
    ' Display the hex line numbers.
    Picture2.Cls
    Picture2.Print mLineNumberBuilder.ToString
End Sub

''
' Releases the byte array back to the mapped file and closes the file.
'
Private Sub CloseFile()
    ' The byte array view is attached to a barrowed
    ' view of the mapped file. We must give it back
    ' or bad things can happen during teardown.
'    If Not CorArray.IsNull(mBytes) Then
'        Call mFile.DeleteView(mBytes)
'        Call mFile.CloseFile
'    End If
    If Not mStream Is Nothing Then
        mStream.CloseStream
        Set mStream = Nothing
    End If
End Sub

''
' Set the first line of the page to be displayed and display the page.
Private Sub UpdateDisplay()
    mTopLine = VScroll1.Value + 32768
    Call DisplayPage
End Sub

''
' We are getting out of here.
'
Private Sub Form_Unload(Cancel As Integer)
    ' We want to be sure to release the byte array
    ' back to the mapped file before the variables
    ' are deallocated by VB, or else bad things will happen.
    
    Call CloseFile
End Sub

Private Sub mnuFileOpen_Click()
    On Error GoTo errTrap
    With CD
        .CancelError = True
        .DialogTitle = "Find File"
        Call .ShowOpen
        On Error GoTo 0
        Call LoadFile(.FileName)
    End With
errTrap:
End Sub

Private Sub VScroll1_Change()
    Call UpdateDisplay
End Sub

Private Sub VScroll1_Scroll()
    Call UpdateDisplay
End Sub
