VERSION 5.00
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "Comdlg32.ocx"
Object = "{3B7C8863-D78F-101B-B9B5-04021C009402}#1.2#0"; "RICHTX32.OCX"
Begin VB.Form SomeForm
  BorderStyle     =   3  'Fixed Dialog
  Caption         =   "Some Form"
  ClientHeight    =   7950
  MaxButton       =   0   'False
  Begin VB.Frame SomeFrame
      Caption         =   "Frame"
      Height          =   1335
      Left            =   120
      TabIndex        =   8
      Top             =   120
      Width           =   10755
      Shortcut        =   {F1}
      Begin VB.CommandButton SomeButton
         Caption         =   "Button"
         ItemData        =   "SomeForm.frx":0000
         Height          =   315
         Left            =   9600
         TabIndex        =   3
         Top             =   780
         Width           =   330
         _Version        =   21563
         TextRTF         =   $"SomeForm.frx":008A
         RightMargin     =   1.31072e5
         CurCell.BeginLfDblClick=   0   'False
		 Shortcut        =   ^F
      End
  End
End