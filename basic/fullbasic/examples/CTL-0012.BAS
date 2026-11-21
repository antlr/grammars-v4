1 REM FBCS CTL-0012
2 REM CLAIM A-8.1.4-05
3 REM TYPE SELF
100 REM Short-circuit evaluation of relational-expressions (l.3572-3589)
110 DIM A(1 TO 10)
120 LET K = 1
130 DO WHILE K <= 10
140   LET A(K) = K
150   LET K = K + 1
160 LOOP
170 LET OK = 1
180 REM first false term makes the conjunction false, rest skipped (A(11) out of range)
190 IF 1 = 2 AND A(11) = 0 THEN LET OK = 0
200 REM first true conjunction makes the disjunction true, rest skipped
210 IF NOT (1 = 1 OR A(11) = 0) THEN LET OK = 0
220 IF NOT (A(5) = 5 AND 1 = 1) THEN LET OK = 0
230 IF NOT (1 = 1 AND A(5) = 5) THEN LET OK = 0
240 REM parenthesized disjunction re-applies the rules; skipped part inside parens
250 IF 1 = 2 AND (1 = 1 OR A(11) = 0) THEN LET OK = 0
260 IF NOT (1 = 1 AND 2 = 2 AND A(3) = 3) THEN LET OK = 0
270 IF OK = 1 THEN PRINT "*** TEST PASSED ***" ELSE PRINT "*** TEST FAILED ***"
280 END
