Dim I As Integer, J As Integer, K As Variant

Private Const L As Integer = 10


For I = 1 TO 10
	Beep
Next


For J=0 TO I
	Beep
Next J


For I=0 To K.Value
	For J=1 To 20 Step 2
		Beep
	Next
Next


For i = 1 To 32000: Next i

Dim i%
  
For i% = 1 To 10 Step -1
	DoEvents
Next i%
