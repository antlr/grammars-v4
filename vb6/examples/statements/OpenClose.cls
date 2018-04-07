Open Filename For Output As #A
Open "c:\Tmp\File" FOR BiNaRy AS #2
Open "c:/Tmp/File2" FOR Append ACCESS READ WRITE LOCK READ WRITE AS #B LEN= 2

Dim tmp as String, content as String
Line Input #A, tmp

Close #A
Close


Open "C:\file3" For Input as #1
While Eof(1) = 0
	line input #1, tmp
	content = content + tmp
Wend
Close #1


Private Sub save()
	Open "test" & ".dat" For Output As #1
	Save
End Sub
