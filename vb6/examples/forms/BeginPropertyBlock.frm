VERSION 5.00
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "Comdlg32.ocx"
Object = "{3B7C8863-D78F-101B-B9B5-04021C009402}#1.2#0"; "RICHTX32.OCX"
Begin VB.Form SomeForm
  BorderStyle     =   3  'Fixed Dialog
  Begin VB.Frame SomeFrame
      Caption         =   "Frame"
      BeginProperty ColumnHeader(1) {BDD1F052-858B-11D1-B16A-00C0F0283628}
          Text        =   "Description"
          Height      =   315
          Left        =   9600
          TabInd      =   3
          BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
              Name            =   "Courier New"
              Size            =   9
              Charset         =   0
              Weight          =   400
              Underline       =   0   'False
              Italic          =   0   'False
              Strikethrough   =   0   'False
          EndProperty
          BeginProperty SomeNestedProp 
              Name            =   "Tahoma"
              Size            =   8.25
              Charset         =   0
              Weight          =   400
              Underline       =   0   'False
              Italic          =   0   'False
              Object.Strikethrough   =   0   'False
          EndProperty
          BeginProperty Panels {8E3867A5-8586-11D1-B16A-00C0F0283628} 
          EndProperty
      EndProperty
      Top             =   120
      Width           =   10755
  End
End