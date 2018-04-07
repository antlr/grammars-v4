100 REM ****************************
110 REM MAIN TEST
120 REM ****************************
121 REM
122 REM
125 REM *********** CLEAR *********
130 CLEAR
135 REM *********** RETURN *********
140 RETURN
145 REM *********** LIST *********
150 LIST
155 REM *********** RUN *********
160 RUN
165 REM *********** END *********
170 END
175 REM *********** GOTO *********
180 GOTO 100
185 REM *********** GOSUB *********
190 GOSUB 100
195 REM *********** LET *********
180 LET x = 12
185 REM *********** INPUT *********
200 INPUT "sdsdsd";y
225 REM *********** IF *********
220 IF x=13 THEN PRINT "xx"
230 REM *********** IF with assign *********
240 IF x=13 THEN LET y=12
245 IF x=13 THEN y=12
245 REM *********** PRINT *********
250 PRINT "xx"
260 REM *********** FOR *********
270 FOR i=10 TO 15 STEP 20
275 FOR i=10 TO 15 
260 REM *********** NEXT *********
261 NEXT i
300 REM *********** DIM *********
310 DIM j(14)
400 REM *********** SQR *********
410 LET h = SQR(12)
420 REM *********** CHR *********
430 LET h = CHR$(12)
440 REM *********** COLON TWO STATEMENTS *********
450 PRINT "1" : PRINT "2"
460 REM *********** COLON COMMENT *********
470 PRINT "1" : REM
480 PRINT "1" : REM 44
490 REM *********** NAKED ASSIGN *********
500 j=77
490 REM *********** PRINT EXPRESSION *********
510 PRINT CHR$ (7)
1000 TEXT
1010 HGR
490 REM *********** LEN *********
520 ll = LEN("abc")
490 REM *********** CALL *********
11 CALL 5010
490 REM *********** ASC *********
520 ll = ASC("a")
490 REM *********** MID *********
520 ll = MID$("abc",1,2)
490 REM *********** STRING VAR *********
520 d$="sdsd"
490 REM *********** PLOT *********
550 HPLOT 0,YS TO 279,YS
550 VPLOT 0,YS TO 279,YS
560 VPLOT 0,YS TO 279,YS
490 REM *********** PR *********
1000 PR#1
1001 PR#2
490 REM *********** FLOATS *********
20  XC = -0.5        : REM CENTER COORD X
30  YC = 0           : REM   "      "   Y
40  S = 2            : REM SCALE
490 REM *********** NEQ *********
40  IF N< > 0 GOTO 80
490 REM *********** TAB *********
10  VTAB 6: HTAB 3
