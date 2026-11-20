1 REM FBCS ARR-0371
2 REM CLAIM A-12.1.4-TR143
3 REM TYPE OUTPUT
4 REM 12.1.4 l.8805-8807, 12.1.2 l.8708-8713: the detached-handler of an internal SUB of the main program assigns to an
5 REM array that the main program declares with DIM.
10 DIM A(3)
20 SUB S
30   WHEN EXCEPTION USE H
40     CAUSE EXCEPTION 1
50   END WHEN
60   HANDLER H
70     LET A(2) = 7
80   END HANDLER
90 END SUB
100 CALL S
110 PRINT A(2)
120 END
