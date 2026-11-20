1 REM FBCS GFX-0240
2 REM CLAIM A-13.5.4-TR141
3 REM TYPE OUTPUT
4 REM 13.5.4, 9.2.4.2 l.4718-4721, 9.2.4.1 l.4660-4668: a recursive internal picture first drawn with a variable; the nested
5 REM DRAW passes an expression, so each invocation has its own parameter.
10 LET C = 0
20 PICTURE Q(N)
30   LET C = C + 1
40   IF C > 9 THEN STOP
50   PRINT "Q"; N
60   IF N < 3 THEN DRAW Q(N + 1)
70 END PICTURE
80 LET K = 1
90 DRAW Q(K)
100 END
