1 REM FBCS FIX-0012
2 REM CLAIM D-15.1-2
3 REM TYPE SELF
4 REM Intermediate results keep P+2 digits (15.1.6); rounding to the fixed-point-size happens on assignment (15.1.4)
10 OPTION ARITHMETIC FIXED *8.2
20 LET A = 1/3
30 LET B = 2/3
40 LET C = 10/4
50 LET OK = 1
60 IF A <> .33 OR B <> .67 OR C <> 2.5 THEN LET OK = 0
70 IF 1/3 = .33 OR 2/3 = .67 THEN LET OK = 0
80 IF 1+1 <> 2 THEN LET OK = 0
90 IF OK = 1 THEN PRINT "*** TEST PASSED ***" ELSE PRINT "*** TEST FAILED ***"
100 END
