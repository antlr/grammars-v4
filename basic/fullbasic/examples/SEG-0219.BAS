1 REM FBCS SEG-0219
2 REM CLAIM A-9.1.4.1-01
3 REM TYPE SELF
4 REM ONE reference H(G(1), G(2)) with two arguments, each one a call that appends to S$;
5 REM evaluating them from left to right (l.4306-4309) leaves "12"
100 LET S$ = ""
110 DECLARE FUNCTION G, H
120 IF H(G(1), G(2)) = 3 AND S$ = "12" THEN PRINT "*** TEST PASSED ***" ELSE PRINT "*** TEST FAILED ***"; S$
130 FUNCTION G(N)
140 LET S$ = S$ & STR$(N)
150 LET G = N
160 END FUNCTION
170 FUNCTION H(A, B)
180 LET H = A + B
190 END FUNCTION
200 END
