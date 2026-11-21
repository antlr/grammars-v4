	title	'CP/M Bdos Interface, Bdos, Version 3.0 Nov, 1982'
;*****************************************************************
;*****************************************************************
;**                                                             **
;**   B a s i c    D i s k   O p e r a t i n g   S y s t e m    **
;**								**
;**               C o n s o l e   P o r t i o n                 **
;**                                                             **
;*****************************************************************
;*****************************************************************
;
;       November 1982
;
;
;	Console handlers
;
conin:
	;read console character to A
	lxi h,kbchar! mov a,m! mvi m,0! ora a! rnz
	;no previous keyboard character ready
	jmp coninf ;get character externally
	;ret
;
conech:
	LXI H,STA$RET! PUSH H
CONECH0:
	;read character with echo
	call conin! call echoc! JC CONECH1 ;echo character?
        ;character must be echoed before return
	push psw! mov c,a! call tabout! pop psw
	RET
CONECH1:
	CALL TEST$CTLS$MODE! RNZ
	CPI CTLS! JNZ CONECH2
	CALL CONBRK2! JMP CONECH0
CONECH2:
	CPI CTLQ! JZ CONECH0
	CPI CTLP! JZ CONECH0
	RET
;
echoc:
	;echo character if graphic
	;cr, lf, tab, or backspace
	cpi cr! rz ;carriage return?
	cpi lf! rz ;line feed?
	cpi tab! rz ;tab?
	cpi ctlh! rz ;backspace?
	cpi ' '! ret ;carry set if not graphic
;
CONSTX:
	LDA KBCHAR! ORA A! JNZ CONB1
	CALL CONSTF! ANI 1! RET
;
if BANKED

SET$CTLS$MODE:
	;SET CTLS STATUS OR INPUT FLAG FOR QUEUE MANAGER
	LXI H,QFLAG! MVI M,40H! XTHL! PCHL

endif
;
TEST$CTLS$MODE:
	;RETURN WITH Z FLAG RESET IF CTL-S CTL-Q CHECKING DISABLED
	MOV B,A! LDA CONMODE! ANI 2! MOV A,B! RET
;
conbrk:	;check for character ready
	CALL TEST$CTLS$MODE! JNZ CONSTX
	lda kbchar! ora a! jnz CONBRK1 ;skip if active kbchar
		;no active kbchar, check external break
		;DOES BIOS HAVE TYPE AHEAD?
if BANKED
		LDA TYPE$AHEAD! INR A! JZ CONSTX ;YES
endif
		;CONBRKX CALLED BY CONOUT

	CONBRKX:
		;HAS CTL-S INTERCEPT BEEN DISABLED?
		CALL TEST$CTLS$MODE! RNZ ;YES
		;DOES KBCHAR CONTAIN CTL-S?
		LDA KBCHAR! CPI CTLS! JZ CONBRK1 ;YES
if BANKED
		CALL SET$CTLS$MODE
endif
		;IS A CHARACTER READY FOR INPUT?
		call constf
if BANKED
		POP H! MVI M,0
endif
		ani 1! rz ;NO
		;character ready, read it
if BANKED
		CALL SET$CTLS$MODE
endif
		call coninf
if BANKED
		POP H! MVI M,0
endif
	CONBRK1:
		cpi ctls! jnz conb0 ;check stop screen function
		;DOES KBCHAR CONTAIN A CTL-S?
		LXI H,KBCHAR! CMP M! JNZ CONBRK2 ;NO
		MVI M,0 ; KBCHAR = 0
		;found ctls, read next character
	CONBRK2:

if BANKED
		CALL SET$CTLS$MODE
endif
		call coninf ;to A
if BANKED
		POP H! MVI M,0
endif
		cpi ctlc! JNZ CONBRK3
		LDA CONMODE! ANI 08H! JZ REBOOTX
		XRA A
	CONBRK3:
		SUI CTLQ! RZ ; RETURN WITH A = ZERO IF CTLQ
		INR A! CALL CONB3! JMP CONBRK2
	conb0:
		LXI H,KBCHAR

		MOV B,A
		;IS CONMODE(1) TRUE?
		LDA CONMODE! RAR! JNC $+7 ;NO
		;DOES KBCHAR = CTLC?
		MVI A,CTLC! CMP M! RZ ;YES - RETURN
		MOV A,B

		CPI CTLQ! JZ CONB2
		CPI CTLP! JZ CONB2
		;character in accum, save it
		MOV M,A
	conb1:
		;return with true set in accumulator
		mvi a,1! ret
	CONB2:
		XRA A! MOV M,A! RET
	CONB3:
		CZ TOGGLE$LISTCP
		MVI C,7! CNZ CONOUTF
		RET
;
TOGGLE$LISTCP:
	; IS PRINTER ECHO DISABLED?
	LDA CONMODE! ANI 14H! JNZ TOGGLE$L1 ;YES
	LXI H,LISTCP! MVI A,1! XRA M! ANI 1
	MOV M,A! RET
TOGGLE$L1:
	XRA A! RET
;
QCONOUTF:
	;DOES FX = INPUT?
	LDA FX! DCR A! JZ CONOUTF ;YES
	;IS ESCAPE SEQUENCE DECODING IN EFFECT?
	MOV A,B! ANI 8! JNZ SCONOUTF ;YES
	JMP CONOUTF
;
conout:
	;compute character position/write console char from C
	;compcol = true if computing column position
	lda compcol! ora a! jnz compout
		;write the character, then compute the column
		;write console character from C
		;B ~= 0 -> ESCAPE SEQUENCE DECODING
		LDA CONMODE! ANI 14H! MOV B,A
		push b
		;CALL CONBRKX FOR OUTPUT FUNCTIONS ONLY
		LDA FX! DCR A! CNZ CONBRKX
		pop b! push b ;recall/save character
		call QCONOUTF ;externally, to console
		pop b
		;SKIP ECHO WHEN CONMODE & 14H ~= 0
		MOV A,B! ORA A! JNZ COMPOUT
		push b ;recall/save character
		;may be copying to the list device
		lda listcp! ora a! cnz listf ;to printer, if so
		pop b ;recall the character
	compout:
		mov a,c ;recall the character
		;and compute column position
		lxi h,column ;A = char, HL = .column
		cpi rubout! rz ;no column change if nulls
		inr m ;column = column + 1
		cpi ' '! rnc ;return if graphic
		;not graphic, reset column position
		dcr m ;column = column - 1
		mov a,m! ora a! rz ;return if at zero
		;not at zero, may be backspace or end line
		mov a,c ;character back to A
		cpi ctlh! jnz notbacksp
			;backspace character
			dcr m ;column = column - 1
			ret
		notbacksp:
			;not a backspace character, eol?
			cpi cr! rnz ;return if not
			;end of line, column = 0
			mvi m,0 ;column = 0
		ret
;
ctlout:
	;send C character with possible preceding up-arrow
	mov a,c! call echoc ;cy if not graphic (or special case)
	jnc tabout ;skip if graphic, tab, cr, lf, or ctlh
		;send preceding up arrow
		push psw! mvi c,ctl! call conout ;up arrow
		pop psw! ori 40h ;becomes graphic letter
		mov c,a ;ready to print
if BANKED
		call chk$column! rz
endif
		;(drop through to tabout)
;
tabout:
	;IS FX AN INPUT FUNCTION?
	LDA FX! DCR A! JZ TABOUT1 ;YES - ALWAYS EXPAND TABS FOR ECHO
	;HAS TAB EXPANSION BEEN DISABLED OR
	;ESCAPE SEQUENCE DECODING BEEN ENABLED?
	LDA CONMODE! ANI 14H! JNZ CONOUT ;YES
TABOUT1:
	;expand tabs to console
	mov a,c! cpi tab! jnz conout ;direct to conout if not
		;tab encountered, move to next tab position
	tab0:

if BANKED
		lda fx! cpi 1! jnz tab1
		call chk$column! rz
	tab1:
endif

		mvi c,' '! call conout ;another blank
		lda column! ani 111b ;column mod 8 = 0 ?
		jnz tab0 ;back for another if not
	ret
;
;
backup:
	;back-up one screen position
	call pctlh

if BANKED
	lda comchr! cpi ctla! rz
endif

	mvi c,' '! call conoutf
;	(drop through to pctlh)				;
pctlh:
	;send ctlh to console without affecting column count
	mvi c,ctlh! jmp conoutf
	;ret
;
crlfp:
	;print #, cr, lf for ctlx, ctlu, ctlr functions
	;then move to strtcol (starting column)
	mvi c,'#'! call conout
	call crlf
	;column = 0, move to position strtcol
	crlfp0:
		lda column! lxi h,strtcol
		cmp m! rnc ;stop when column reaches strtcol
		mvi c,' '! call conout ;print blank
		jmp crlfp0
;;
;
crlf:
	;carriage return line feed sequence
	mvi c,cr! call conout! mvi c,lf! jmp conout
	;ret
;
print:
	;print message until M(BC) = '$'
	LXI H,OUTDELIM
	ldax b! CMP M! rz ;stop on $
		;more to print
		inx b! push b! mov c,a ;char to C
		call tabout ;another character printed
		pop b! jmp print
;
QCONIN:

if BANKED
	lhld apos! mov a,m! sta ctla$sw
endif
	;IS BUFFER ADDRESS = 0?
	LHLD CONBUFFADD! MOV A,L! ORA H! JZ CONIN ;YES
	;IS CHARACTER IN BUFFER < 5?

if BANKED
	call qconinx ; mov a,m with bank 1 switched in
else
	MOV A,M
endif

	INX H
	ORA A! JNZ QCONIN1 ; NO
	LXI H,0
QCONIN1:
	SHLD CONBUFFADD! SHLD CONBUFFLEN! RNZ ; NO
	JMP CONIN

if BANKED
	
chk$column:
	lda conwidth! mov e,a! lda column! cmp e! ret
;
expand:
	xchg! lhld apos! xchg
expand1:
	ldax d! ora a! rz
	inx d! inx h! mov m,a! inr b! jmp expand1
;
copy$xbuff:
	mov a,b! ora a! rz
	push b! mov c,b! push h! xchg! inx d
	lxi h,xbuff
	call move
	mvi m,0! shld xpos
	pop h! pop b! ret
;
copy$cbuff:
	lda ccpflgs+1! ral! rnc
	lxi h,xbuff! lxi d,cbuff! inr c! jnz copy$cbuff1
	xchg! mov a,b! ora a! rz
	sta cbuff$len
	push d! lxi b,copy$cbuff2! push b
	mov b,a
copy$cbuff1:
	inr b! mov c,b! jmp move
copy$cbuff2:
	pop h! dcx h! mvi m,0! ret
;
save$col:
	lda column! sta save$column! ret
;
clear$right:
	lda column! lxi h,ctla$column! cmp m! rnc
	mvi c,20h! call conout! jmp clear$right
;
reverse:
	lda save$column! lxi h,column! cmp m! rnc
	mvi c,ctlh! call conout! jmp reverse
;
chk$buffer$size:
	push b! push h
	lhld apos! mvi e,0
cbs1:
	mov a,m! ora a! jz cbs2
	inr e! inx h! jmp cbs1
cbs2:
	mov a,b! add e! cmp c
	push a! mvi c,7! cnc conoutf
	pop a! pop h! pop b! rc
	pop d! pop d! jmp readnx
;
refresh:
	lda ctla$sw! ora a! rz
	lda comchr! cpi ctla! rz
	cpi ctlf! rz
	cpi ctlw! rz
refresh0:
	push h! push b
	call save$col
	lhld apos
refresh1:
	mov a,m! ora a! jz refresh2
	mov c,a! call chk$column! jc refresh05
	mov a,e! sta column! jmp refresh2
refresh05:
	push h! call ctlout
	pop h! inx h! jmp refresh1
refresh2:
	lda column! sta new$ctla$col
refresh3:
	call clear$right
	call reverse
	lda new$ctla$col! sta ctla$column
	pop b! pop h! ret
;
init$apos:
	lxi h,aposi! shld apos
	xra a! sta ctla$sw
	ret
;
init$xpos:
	lxi h,xbuff! shld xpos! ret
;
set$ctla$column:
	lxi h,ctla$sw! mov a,m! ora a! rnz
	inr m! lda column! sta ctla$column! ret
;
readi:
	call chk$column! cnc crlf
	lda cbuff$len! mov b,a
	mvi c,0! call copy$cbuff
else

readi:
	MOV A,D! ORA E! JNZ READ
	LHLD DMAAD! SHLD INFO
	INX H! INX H! SHLD CONBUFFADD
endif

read:	;read to info address (max length, current length, buffer)

if BANKED
	call init$xpos
	call init$apos
readx:
	call refresh
	xra a! sta ctlw$sw
readx1:

endif

	MVI A,1! STA FX
	lda column! sta strtcol ;save start for ctl-x, ctl-h
	lhld info! mov c,m! inx h! push h
	XRA A! MOV B,A! STA SAVEPOS
	CMP C! JNZ $+4
	INR C
	;B = current buffer length,
	;C = maximum buffer length,
	;HL= next to fill - 1
	readnx:
		;read next character, BC, HL active
		push b! push h ;blen, cmax, HL saved
		readn0:

if BANKED
			lda ctlw$sw! ora a! cz qconin
nxtline:
			sta comchr
else
			CALL QCONIN ;next char in A
endif

			;ani 7fh ;mask parity bit
			pop h! pop b ;reactivate counters
			cpi cr! jz readen ;end of line?
			cpi lf! jz readen ;also end of line

if BANKED
			cpi ctlf! jnz not$ctlf
		do$ctlf:
			call chk$column! dcr e! cmp e! jnc readnx
		do$ctlf0:
			xchg! lhld apos! mov a,m! ora a! jz ctlw$l15
			inx h! shld apos! xchg! jmp notr
		not$ctlf:
			cpi ctlw! jnz not$ctlw
		do$ctlw:
			xchg! lhld apos! mov a,m! ora a! jz ctlw$l1
			xchg! call chk$column! dcr e! cmp e! xchg! jc ctlw$l0
			xchg! call refresh0! xchg! jmp ctlw$l13
		ctlw$l0:
			lhld apos! mov a,m
			inx h! shld apos! jmp ctlw$l3
		ctlw$l1:
			lxi h,ctla$sw! mov a,m! mvi m,0
			ora a! jz ctlw$l2
		ctlw$l13:
			lxi h,ctlw$sw! mvi m,0
		ctlw$l15:
			xchg! jmp readnx
		ctlw$l2:
			lda ctlw$sw! ora a! jnz ctlw$l25
			mov a,b! ora a! jnz ctlw$l15
			call init$xpos
		ctlw$l25:
			lhld xpos! mov a,m! ora a
			sta ctlw$sw! jz ctlw$l15
			inx h! shld xpos
		ctlw$l3:
			lxi h,ctlw$sw! mvi m,ctlw
			xchg! jmp notr
		not$ctlw:
			cpi ctla! jnz not$ctla
		do$ctla:
			;do we have any characters to back over?
			lda strtcol! mov d,a! lda column! cmp d
			jz readnx
			sta compcol ;COL > 0
			mov a,b! ora a! jz linelen
			;characters remain in buffer, backup one
			dcr b ;remove one character
			;compcol > 0 marks repeat as length compute
			;backup one position in xbuff
			push h
			call set$ctla$column
			pop d
			lhld apos! dcx h
			shld apos! ldax d! mov m,a! xchg! jmp linelen
		not$ctla:
			cpi ctlb! jnz not$ctlb
		do$ctlb:
			lda save$pos! cmp b! jnz ctlb$l0
			mvi a,ctlw! sta ctla$sw
			sta comchr! jmp do$ctlw
		ctlb$l0:
			xchg! lhld apos! inr b
		ctlb$l1:
			dcr b! lda save$pos! cmp b! jz ctlb$l2
			dcx h! ldax d! mov m,a! dcx d! jmp ctlb$l1
		ctlb$l2:
			shld apos
			push b! push d
			call set$ctla$column
		ctlb$l3:
			lda column! mov b,a
			lda strtcol! cmp b! jz read$n0
 			mvi c,ctlh! call conout! jmp ctlb$l3
		not$ctlb:
			cpi ctlk! jnz not$ctlk
			xchg! lxi h,aposi! shld apos
			xchg! call refresh
			jmp readnx
		not$ctlk:
			cpi ctlg! jnz not$ctlg
			lda ctla$sw! ora a! jz readnx
			jmp do$ctlf0
		not$ctlg:
endif

			cpi ctlh! jnz noth ;backspace?
			LDA CTLH$ACT! INR A! JZ DO$RUBOUT
		DO$CTLH:
			;do we have any characters to back over?
			LDA STRTCOL! MOV D,A! LDA COLUMN! CMP D
			jz readnx
			STA COMPCOL ;COL > 0
			MOV A,B! ORA A! JZ $+4
			;characters remain in buffer, backup one
			dcr b ;remove one character
			;compcol > 0 marks repeat as length compute
			jmp linelen ;uses same code as repeat
		noth:
			;not a backspace
			cpi rubout! jnz notrub ;rubout char?
			LDA RUBOUT$ACT! INR A! JZ DO$CTLH
		DO$RUBOUT:
if BANKED
			mvi a,rubout! sta comchr
			lda ctla$sw! ora a! jnz do$ctlh
endif
			;rubout encountered, rubout if possible
			mov a,b! ora a! jz readnx ;skip if len=0
			;buffer has characters, resend last char
			mov a,m! dcr b! dcx h ;A = last char
			;blen=blen-1, next to fill - 1 decremented
			jmp rdech1 ;act like this is an echo
		notrub:
			;not a rubout character, check end line
			cpi ctle! jnz note ;physical end line?
			;yes, save active counters and force eol
			push b! MOV A,B! STA SAVE$POS
			push h
if BANKED
			lda ctla$sw! ora a! cnz clear$right
endif
			call crlf
if BANKED
			call refresh
endif
			xra a! sta strtcol ;start position = 00
			jmp readn0 ;for another character
		note:
			;not end of line, list toggle?
			cpi ctlp! jnz notp ;skip if not ctlp
			;list toggle - change parity
			push h ;save next to fill - 1
			PUSH B
			XRA A! CALL CONB3
			POP B
			pop h! jmp readnx ;for another char
		notp:
			;not a ctlp, line delete?
			cpi ctlx! jnz notx
			pop h ;discard start position
			;loop while column > strtcol
			backx:
				lda strtcol! lxi h,column
if BANKED
				cmp m! jc backx1
				lhld apos! mov a,m! ora a! jnz readx
				jmp read
			    backx1:
else
				cmp m! jnc read ;start again
endif
				dcr m ;column = column - 1
				call backup ;one position
				jmp backx
		notx:
			;not a control x, control u?
			;not control-X, control-U?
			cpi ctlu! jnz notu ;skip if not
if BANKED
			xthl! call copy$xbuff! xthl
endif
			;delete line (ctlu)
		do$ctlu:
			call crlfp ;physical eol
			pop h ;discard starting position
			jmp read ;to start all over
		notu:
			;not line delete, repeat line?
			cpi ctlr! jnz notr
			XRA A! STA SAVEPOS
if BANKED
			xchg! call init$apos! xchg
			mov a,b! ora a! jz do$ctlu
			xchg! lhld apos! inr b
		ctlr$l1:
			dcr b! jz ctlr$l2
			dcx h! ldax d! mov m,a! dcx d
			jmp ctlr$l1
		ctlr$l2:
			shld apos! push b! push d
			call crlfp! mvi a,ctlw! sta ctlw$sw
			sta ctla$sw! jmp readn0
endif
		linelen:
			;repeat line, or compute line len (ctlh)
			;if compcol > 0
			push b! call crlfp ;save line length
			pop b! pop h! push h! push b
			;bcur, cmax active, beginning buff at HL
		rep0:
			mov a,b! ora a! jz rep1 ;count len to 00
			inx h! mov c,m ;next to print
			DCR B
			POP D! PUSH D! MOV A,D! SUB B! MOV D,A
			push b! push h ;count length down
			LDA SAVEPOS! CMP D! CC CTLOUT
			pop h! pop b ;recall remaining count
			jmp rep0 ;for the next character
		rep1:
			;end of repeat, recall lengths
			;original BC still remains pushed
			push h ;save next to fill
			lda compcol! ora a ;>0 if computing length
			jz readn0 ;for another char if so
			;column position computed for ctlh
			lxi h,column! sub m ;diff > 0
			sta compcol ;count down below
			;move back compcol-column spaces
		backsp:
			;move back one more space
			call backup ;one space
			lxi h,compcol! dcr m
			jnz backsp
if BANKED
			call refresh
endif
			jmp readn0 ;for next character
		notr:
			;not a ctlr, place into buffer
			;IS BUFFER FULL?
			PUSH A
			MOV A,B! CMP C! JC RDECH0 ;NO
			;DISCARD CHARACTER AND RING BELL
			POP A! PUSH B! PUSH H
			MVI C,7! CALL CONOUTF! JMP READN0
		RDECH0:

if BANKED
			lda comchr! cpi ctlg! jz rdech05
			lda ctla$sw! ora a! cnz chk$buffer$size
		rdech05:
endif

			POP A
			inx h! mov m,a ;character filled to mem
			inr b ;blen = blen + 1
		rdech1:
			;look for a random control character
			push b! push h ;active values saved
			mov c,a ;ready to print
if BANKED
			call save$col
endif
			call ctlout ;may be up-arrow C
			pop h! pop b
if BANKED
			lda comchr! cpi ctlg! jz do$ctlh
			cpi rubout! jz rdech2
			call refresh
		rdech2:
endif
			LDA CONMODE! ANI 08H! JNZ NOTC
			mov a,m ;recall char
			cpi ctlc ;set flags for reboot test
			mov a,b ;move length to A
			jnz notc ;skip if not a control c
			cpi 1 ;control C, must be length 1
			jz REBOOTX ;reboot if blen = 1
			;length not one, so skip reboot
		notc:
			;not reboot, are we at end of buffer?
if BANKED
			cmp c! jnc buffer$full
else
			jmp readnx ;go for another if not
endif

if BANKED
			push b! push h
			call chk$column! jc readn0
			lda ctla$sw! ora a! jz do$new$line
			lda comchr! cpi ctlw! jz back$one
			cpi ctlf! jz back$one
				
		do$newline:
			mvi a,ctle! jmp nxtline

		back$one:
			;back up to previous character
			pop h! pop b
			dcr b! xchg
			lhld apos! dcx h! shld apos
			ldax d! mov m,a! xchg! dcx h
			push b! push h! call reverse
			;disable ctlb or ctlw
			xra a! sta ctlw$sw! jmp readn0
		
		buffer$full:
			xra a! sta ctlw$sw! jmp readnx
endif
		readen:
			;end of read operation, store blen
if BANKED
			call expand
endif
			pop h! mov m,b ;M(current len) = B
if BANKED
			push b
			call copy$xbuff
			pop b
			mvi c,0ffh! call copy$cbuff
endif
			LXI H,0! SHLD CONBUFFADD
			mvi c,cr! jmp conout ;return carriage
			;ret
;
func1	equ	CONECH
	;return console character with echo
;
func2:	equ	tabout
	;write console character with tab expansion
;
func3:
	;return reader character
	call readerf
	jmp sta$ret
;
;func4:	equated to punchf
	;write punch character
;
;func5:	equated to listf
	;write list character
	;write to list device
;
func6:
	;direct console i/o - read if 0ffh
	mov a,c! inr a! jz dirinp ;0ffh => 00h, means input mode
		inr a! JZ DIRSTAT ;0feh => direct STATUS function
		INR A! JZ DIRINP1 ;0fdh => direct input, no status
		JMP CONOUTF
	DIRSTAT:
		;0feH in C for status
		CALL CONSTX! JNZ LRET$EQ$FF! JMP STA$RET
	dirinp:
		CALL CONSTX ;status check
		ora a! RZ ;skip, return 00 if not ready
		;character is ready, get it
	dirinp1:
		call CONIN ;to A
		jmp sta$ret
;
func7:
	call auxinstf
	jmp sta$ret
;
func8:
	call auxoutstf
	jmp sta$ret
;
func9:
	;write line until $ encountered
	xchg	;was lhld info	
	mov c,l! mov b,h ;BC=string address
	jmp print ;out to console	

func10	equ	readi
	;read a buffered console line

func11:
	;IS CONMODE(1) TRUE?
	LDA CONMODE! RAR! JNC NORMAL$STATUS ;NO
	;CTL-C ONLY STATUS CHECK
if BANKED
	LXI H,QFLAG! MVI M,80H! PUSH H
endif
	LXI H,CTLC$STAT$RET! PUSH H
	;DOES KBCHAR = CTL-C?
	LDA KBCHAR! CPI CTLC! JZ CONB1 ;YES
	;IS THERE A READY CHARACTER?
	CALL CONSTF! ORA A! RZ ;NO
	;IS THE READY CHARACTER A CTL-C?
	CALL CONINF! CPI CTLC! JZ CONB0 ;YES
	STA KBCHAR! XRA A! RET

CTLC$STAT$RET:

if BANKED
	CALL STA$RET
	POP H! MVI M,0! RET
else
	JMP STA$RET
endif

NORMAL$STATUS:
	;check console status
	call conbrk
	;(drop through to sta$ret)
sta$ret:
	;store the A register to aret
	sta aret
func$ret:						;
	ret ;jmp goback (pop stack for non cp/m functions)
;
setlret1:
	;set lret = 1
	mvi a,1! jmp sta$ret				;
;
FUNC109:			;GET/SET CONSOLE MODE
	;DOES DE = 0FFFFH?
	MOV A,D! ANA E! INR A
	LHLD CONMODE! JZ STHL$RET ;YES - RETURN CONSOLE MODE
	XCHG! SHLD CONMODE! RET ;NO - SET CONSOLE MODE
;
FUNC110:			;GET/SET FUNCTION 9 DELIMITER
	LXI H,OUT$DELIM
	;DOES DE = 0FFFFH?
	MOV A,D! ANA E! INR A
	MOV A,M! JZ STA$RET ;YES - RETURN DELIMITER
	MOV M,E! RET ;NO - SET DELIMITER
;
FUNC111:			;PRINT BLOCK TO CONSOLE
FUNC112:			;LIST BLOCK
	XCHG! MOV E,M! INX H! MOV D,M! INX H
	MOV C,M! INX H! MOV B,M! XCHG
	;HL = ADDR OF STRING
	;BC = LENGTH OF STRING
BLK$OUT:
	MOV A,B! ORA C! RZ
	PUSH B! PUSH H! MOV C,M
	LDA FX! CPI 111! JZ BLK$OUT1
	CALL LISTF! JMP BLK$OUT2
BLK$OUT1:
	CALL TABOUT
BLK$OUT2:
	POP H! INX H! POP B! DCX B
	JMP BLK$OUT

SCONOUTF	EQU	CONOUTF

;
;	data areas
;
compcol:db	0	;true if computing column position
strtcol:db	0	;starting column position after read

if not BANKED

kbchar:	db	0	;initial key char = 00

endif

SAVEPOS:DB	0	;POSITION IN BUFFER CORRESPONDING TO
			;BEGINNING OF LINE
if BANKED

comchr:		db	0
cbuff$len:	db	0
cbuff:		ds	256
		db	0
xbuff:		db	0
		ds	354
aposi:		db	0
xpos:		dw	0
apos:		dw	0
ctla$sw:	db	0
ctlw$sw:	db	0
save$column:	db	0
ctla$column:	db	0
new$ctla$col:	db	0

endif

;	end of BDOS Console module
