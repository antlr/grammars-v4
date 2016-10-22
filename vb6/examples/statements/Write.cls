Open "c:/test.csv" For Output As #I

Write #I, 'blank
Write #I, "Cell 1", 123, "Cell 3", "Cell 4"
Write #I, 123; 456

Dim MovieTitle
Dim Ean
Dim MovieSubTitle
Dim Date
Dim Price

Print #I, MovieTitle; Tab(20); Format$(Ean, "@@@@"); _
	Tab(40); MovieSubTitle; _
	Tab(60); Format$(Date, "m/d/yyyy"); _
	Tab(80); Format$(Format$(Price, "#0.00"), "@@@@@")

Close