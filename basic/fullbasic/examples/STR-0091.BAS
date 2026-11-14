1 REM FBCS STR-0091
2 REM CLAIM E-1051
3 REM TYPE SELF
100 REM A string-expression whose length exceeds the maximum string length of the implementation
110 REM raises the fatal exception 1051 (6.3.5 l.2556, Table 9 l.12796) instead of being truncated.
120 REM Each count below gives a result of about 2E9 characters, beyond any maximum this
130 REM implementation documents (1E9); the null string repeated any number of times is still null.
140 LET FAIL = 0
150 WHEN EXCEPTION IN
160   LET A$ = REPEAT$("A", 2E9)
170   LET FAIL = FAIL + 1
180 USE
190   IF EXTYPE <> 1051 THEN LET FAIL = FAIL + 1
200 END WHEN
210 WHEN EXCEPTION IN
220   LET B$ = REPEAT$("AB", 1E9)
230   LET FAIL = FAIL + 1
240 USE
250   IF EXTYPE <> 1051 THEN LET FAIL = FAIL + 1
260 END WHEN
270 LET C$ = REPEAT$("", 2E9)
280 IF LEN(C$) <> 0 THEN LET FAIL = FAIL + 1
290 IF FAIL = 0 THEN PRINT "*** TEST PASSED ***" ELSE PRINT "*** TEST FAILED ***"
300 END
