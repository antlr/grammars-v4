1 REM FBCS EXC-0101
2 REM CLAIM A-12.1.4-15
3 REM TYPE SELF
4 REM STD 12.1.4 l.8923-8927 and 13.5.5: the exceptions raised by the system itself (here 5002, a
5 REM transform too large for its receiving array) have a text, read inside the handler.
100 OPTION BASE 1
110 DIM T(2, 2)
120 LET OK = 1
130 WHEN EXCEPTION IN
140   MAT T = ROTATE(30)
150   LET OK = 0
160 USE
170   LET E = EXTYPE
180   LET M$ = EXTEXT$(EXTYPE)
190 END WHEN
200 IF E <> 5002 OR M$ = "" THEN LET OK = 0
210 IF E <> 5002 OR M$ = "" THEN PRINT "FALLO"; E; M$
220 IF OK = 1 THEN PRINT "*** TEST PASSED ***" ELSE PRINT "*** TEST FAILED ***"
230 END
