	title	'Bdos Interface, Bdos, Version 2.2 Feb, 1980'
;*****************************************************************
;*****************************************************************
;**                                                             **
;**   B a s i c    D i s k   O p e r a t i n g   S y s t e m    **
;**            I n t e r f a c e   M o d u l e                   **
;**                                                             **
;*****************************************************************
;*****************************************************************
;
;	Copyright (c) 1978, 1979, 1980
;	Digital Research
;	Box 579, Pacific Grove
;	California
;
;
;      20 january 1980
;
;
on	equ	0ffffh
off	equ	00000h
test	equ	off
;
	if	test
	org	0dc00h
	else
	org	0800h
	endif
;	bios value defined at end of module
;
ssize	equ	24		;24 level stack
;
;	low memory locations
reboot	equ	0000h		;reboot system
ioloc	equ	0003h		;i/o byte location
bdosa	equ	0006h		;address field of jmp BDOS
;
;	bios access constants
bootf	set	bios+3*0	;cold boot function
wbootf	set	bios+3*1	;warm boot function
constf	set	bios+3*2	;console status function
coninf	set	bios+3*3	;console input function
conoutf	set	bios+3*4	;console output function
listf	set	bios+3*5	;list output function
punchf	set	bios+3*6	;punch output function
readerf	set	bios+3*7	;reader input function
homef	set	bios+3*8	;disk home function
seldskf	set	bios+3*9	;select disk function
settrkf	set	bios+3*10	;set track function
setsecf	set	bios+3*11	;set sector function
setdmaf	set	bios+3*12	;set dma function
readf	set	bios+3*13	;read disk function
writef	set	bios+3*14	;write disk function
liststf	set	bios+3*15	;list status function
sectran	set	bios+3*16	;sector translate
;
;	equates for non graphic characters
ctlc	equ	03h	;control c
ctle	equ	05h	;physical eol
ctlh	equ	08h	;backspace
ctlp	equ	10h	;prnt toggle
ctlr	equ	12h	;repeat line
ctls	equ	13h	;stop/start screen
ctlu	equ	15h	;line delete
ctlx	equ	18h	;=ctl-u
ctlz	equ	1ah	;end of file
rubout	equ	7fh	;char delete
tab	equ	09h	;tab char
cr	equ	0dh	;carriage return
lf	equ	0ah	;line feed
ctl	equ	5eh	;up arrow
;
	db	0,0,0,0,0,0
;
;	enter here from the user's program with function number in c,
;	and information address in d,e
	jmp	bdose	;past parameter block
;
;	************************************************
;	*** relative locations 0009 - 000e           ***
;	************************************************
pererr:	dw	persub	;permanent error subroutine
selerr:	dw	selsub	;select error subroutine
roderr:	dw	rodsub	;ro disk error subroutine
roferr:	dw	rofsub	;ro file error subroutine
;
;
bdose:	;arrive here from user programs
	xchg! shld info! xchg ;info=DE, DE=info
	mov a,e! sta linfo ;linfo = low(info) - don't equ
	lxi h,0! shld aret ;return value defaults to 0000
	;save user's stack pointer, set to local stack
	dad sp! shld entsp ;entsp = stackptr
	lxi sp,lstack ;local stack setup
	xra a! sta fcbdsk! sta resel ;fcbdsk,resel=false
	lxi h,goback ;return here after all functions
	push h ;jmp goback equivalent to ret
	mov a,c! cpi nfuncs! rnc ;skip if invalid #
	mov c,e ;possible output character to C
	lxi h,functab! mov e,a! mvi d,0 ;DE=func, HL=.ciotab
	dad d! dad d! mov e,m! inx h! mov d,m ;DE=functab(func)
		lhld info	;info in DE for later xchg	
	xchg! pchl ;dispatched
;
;	dispatch table for functions
functab:
	dw	wbootf, func1, func2, func3
	dw	punchf, listf, func6, func7
	dw	func8, func9, func10,func11
diskf	equ	($-functab)/2	;disk funcs
	dw	func12,func13,func14,func15
	dw	func16,func17,func18,func19
	dw	func20,func21,func22,func23
	dw	func24,func25,func26,func27
	dw	func28,func29,func30,func31
	dw	func32,func33,func34,func35
	dw	func36,func37,func38,func39		;
	dw	func40					;
nfuncs	equ	($-functab)/2
;
;
;	error subroutines
persub:	;report permanent error
	lxi h,permsg! call errflg ;to report the error
	cpi ctlc! jz reboot ;reboot if response is ctlc
	ret ;and ignore the error
;
selsub:	;report select error
	lxi h,selmsg! jmp wait$err ;wait console before boot
;
rodsub:	;report write to read/only disk
	lxi h,rodmsg! jmp wait$err ;wait console
;
rofsub:	;report read/only file
	lxi h,rofmsg ;drop through to wait for console
;
wait$err:
	;wait for response before boot
	call errflg! jmp reboot
;
;	error messages
dskmsg:	db	'Bdos Err On '
dskerr:	db	' : $'	;filled in by errflg
permsg:	db	'Bad Sector$'
selmsg:	db	'Select$'
rofmsg:	db	'File '
rodmsg:	db	'R/O$'
;
;
errflg:
	;report error to console, message address in HL
	push h! call crlf ;stack mssg address, new line
	lda curdsk! adi 'A'! sta dskerr ;current disk name
	lxi b,dskmsg! call print ;the error message
	pop b! call print ;error mssage tail
	;jmp conin ;to get the input character			
	;(drop through to conin)
	;ret
;
;
;	console handlers
conin:
	;read console character to A
	lxi h,kbchar! mov a,m! mvi m,0! ora a! rnz
	;no previous keyboard character ready
	jmp coninf ;get character externally
	;ret
;
conech:
	;read character with echo
	call conin! call echoc! rc ;echo character?
        ;character must be echoed before return
	push psw! mov c,a! call tabout! pop psw
	ret ;with character in A
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
conbrk:	;check for character ready
	lda kbchar! ora a! jnz conb1 ;skip if active kbchar
		;no active kbchar, check external break
		call constf! ani 1! rz ;return if no char ready
		;character ready, read it
		call coninf ;to A
		cpi ctls! jnz conb0 ;check stop screen function
		;found ctls, read next character
		call coninf ;to A
		cpi ctlc! jz reboot ;ctlc implies re-boot
		;not a reboot, act as if nothing has happened
		xra a! ret ;with zero in accumulator
	conb0:
		;character in accum, save it
		sta kbchar
	conb1:
		;return with true set in accumulator
		mvi a,1! ret
;
conout:
	;compute character position/write console char from C
	;compcol = true if computing column position
	lda compcol! ora a! jnz compout
		;write the character, then compute the column
		;write console character from C
		push b! call conbrk ;check for screen stop function
		pop b! push b ;recall/save character
		call conoutf ;externally, to console
		pop b! push b ;recall/save character
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
			cpi lf! rnz ;return if not
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
		;(drop through to tabout)
;
tabout:
	;expand tabs to console
	mov a,c! cpi tab! jnz conout ;direct to conout if not
		;tab encountered, move to next tab position
	tab0:
		mvi c,' '! call conout ;another blank
		lda column! ani 111b ;column mod 8 = 0 ?
		jnz tab0 ;back for another if not
	ret
;
;
backup:
	;back-up one screen position
	call pctlh! mvi c,' '! call conoutf
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
	ldax b! cpi '$'! rz ;stop on $
		;more to print
		inx b! push b! mov c,a ;char to C
		call tabout ;another character printed
		pop b! jmp print
;
read:	;read to info address (max length, current length, buffer)
	lda column! sta strtcol ;save start for ctl-x, ctl-h
	lhld info
	mov c,m! inx h! push h! mvi b,0
	;B = current buffer length,
	;C = maximum buffer length,
	;HL= next to fill - 1
	readnx:
		;read next character, BC, HL active
		push b! push h ;blen, cmax, HL saved
		readn0:
			call conin ;next char in A
			ani 7fh ;mask parity bit
			pop h! pop b ;reactivate counters
			cpi cr! jz readen ;end of line?
			cpi lf! jz readen ;also end of line
			cpi ctlh! jnz noth ;backspace?
			;do we have any characters to back over?
			mov a,b! ora a! jz readnx
			;characters remain in buffer, backup one
			dcr b ;remove one character
			lda column! sta compcol ;col > 0
			;compcol > 0 marks repeat as length compute
			jmp linelen ;uses same code as repeat
		noth:
			;not a backspace
			cpi rubout! jnz notrub ;rubout char?
			;rubout encountered, rubout if possible
			mov a,b! ora a! jz readnx ;skip if len=0
			;buffer has characters, resend last char
			mov a,m! dcr b! dcx h ;A = last char
			;blen=blen-1, next to fill - 1 decremented
			jmp rdech1 ;act like this is an echo
;
		notrub:
			;not a rubout character, check end line
			cpi ctle! jnz note ;physical end line?
			;yes, save active counters and force eol
			push b! push h! call crlf
			xra a! sta strtcol ;start position = 00
			jmp readn0 ;for another character
		note:
			;not end of line, list toggle?
			cpi ctlp! jnz notp ;skip if not ctlp
			;list toggle - change parity
			push h ;save next to fill - 1
			lxi h,listcp ;HL=.listcp flag
			mvi a,1! sub m ;True-listcp
			mov m,a ;listcp = not listcp
			pop h! jmp readnx ;for another char
		notp:
			;not a ctlp, line delete?
			cpi ctlx! jnz notx
			pop h ;discard start position
			;loop while column > strtcol
			backx:
				lda strtcol! lxi h,column
				cmp m! jnc read ;start again
				dcr m ;column = column - 1
				call backup ;one position
				jmp backx
		notx:
			;not a control x, control u?
			;not control-X, control-U?
			cpi ctlu! jnz notu ;skip if not
			;delete line (ctlu)
			call crlfp ;physical eol
			pop h ;discard starting position
			jmp read ;to start all over
		notu:
			;not line delete, repeat line?
			cpi ctlr! jnz notr
		linelen:
			;repeat line, or compute line len (ctlh)
			;if compcol > 0
			push b! call crlfp ;save line length
			pop b! pop h! push h! push b
			;bcur, cmax active, beginning buff at HL
		rep0:
			mov a,b! ora a! jz rep1 ;count len to 00
			inx h! mov c,m ;next to print
			dcr b! push b! push h ;count length down
			call ctlout ;character echoed
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
			jmp readn0 ;for next character
		notr:
			;not a ctlr, place into buffer
		rdecho:
			inx h! mov m,a ;character filled to mem
			inr b ;blen = blen + 1
		rdech1:
			;look for a random control character
			push b! push h ;active values saved
			mov c,a ;ready to print
			call ctlout ;may be up-arrow C
			pop h! pop b! mov a,m ;recall char
			cpi ctlc ;set flags for reboot test
			mov a,b ;move length to A
			jnz notc ;skip if not a control c
			cpi 1 ;control C, must be length 1
			jz reboot ;reboot if blen = 1
			;length not one, so skip reboot
		notc:
			;not reboot, are we at end of buffer?
			cmp c! jc readnx ;go for another if not
		readen:
			;end of read operation, store blen
			pop h! mov m,b ;M(current len) = B
			mvi c,cr! jmp conout ;return carriage
			;ret
func1:
	;return console character with echo
	call conech
	jmp sta$ret
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
		inr a! jz constf	;0feH in C for status
		;direct output function
		jmp conoutf			;
	dirinp:
		call constf ;status check
		ora a! jz retmon ;skip, return 00 if not ready
		;character is ready, get it
		call coninf ;to A
		jmp sta$ret
;
func7:
	;return io byte
	lda ioloc
	jmp sta$ret
;
func8:
	;set i/o byte
	lxi h,ioloc
	mov m,c
	ret ;jmp goback
;
func9:
	;write line until $ encountered
	xchg	;was lhld info	
	mov c,l! mov b,h ;BC=string address
	jmp print ;out to console	
;
func10:	equ	read
	;read a buffered console line
;
func11:
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
;
;
;	data areas
;
compcol:db	0	;true if computing column position
strtcol:db	0	;starting column position after read
column:	db	0	;column position
listcp:	db	0	;listing toggle
kbchar:	db	0	;initial key char = 00
entsp:	ds	2	;entry stack pointer
	ds	ssize*2	;stack size
lstack:
;	end of Basic I/O System
;
;*****************************************************************
;*****************************************************************
;
;	common values shared between bdosi and bdos
usrcode:db	0	;current user number
curdsk:	db	0	;current disk number
info:	ds	2	;information address
aret:	ds	2	;address value to return
lret	equ	aret	;low(aret)
;
;*****************************************************************
;*****************************************************************
;**                                                             **
;**   B a s i c    D i s k   O p e r a t i n g   S y s t e m    **
;**                                                             **
;*****************************************************************
;*****************************************************************
;
dvers	equ	22h	;version 2.2
;	module addresses
;
;	literal constants
true	equ	0ffh	;constant true
false	equ	000h	;constant false
enddir	equ	0ffffh	;end of directory
byte	equ	1	;number of bytes for "byte" type
word	equ	2	;number of bytes for "word" type
;
;	fixed addresses in low memory
tfcb	equ	005ch	;default fcb location
tbuff	equ	0080h	;default buffer location
;
;	fixed addresses referenced in bios module are
;	pererr (0009), selerr (000c), roderr (000f)
;
;	error message handlers
;
;per$error:	 
	;report permanent error to user	
;	lxi h,pererr  jmp goerr		
;
;rod$error:						;
	;report read/only disk error
;	lxi h,roderr  jmp goerr				;
;
;rof$error:						;
	;report read/only file error			;
;	lxi h,roferr	;jmp goerr	
;
sel$error:
	;report select error
	lxi h,selerr					;
;
;
goerr:
	;HL = .errorhandler, call subroutine
	mov e,m! inx h! mov d,m ;address of routine in DE
	xchg! pchl ;to subroutine
;
;
;
;	local subroutines for bios interface
;
move:
	;move data length of length C from source DE to
	;destination given by HL
	inr c ;in case it is zero
	move0:
		dcr c! rz ;more to move
		ldax d! mov m,a ;one byte moved
		inx d! inx h ;to next byte
		jmp move0
;
selectdisk:
	;select the disk drive given by curdsk, and fill
	;the base addresses curtrka - alloca, then fill
	;the values of the disk parameter block
	lda curdsk! mov c,a ;current disk# to c
	;lsb of e = 0 if not yet logged - in
	call seldskf ;HL filled by call
	;HL = 0000 if error, otherwise disk headers
	mov a,h! ora l! rz ;return with 0000 in HL and z flag
		;disk header block address in hl
		mov e,m! inx h! mov d,m! inx h ;DE=.tran
		shld cdrmaxa! inx h! inx h ;.cdrmax
		shld curtrka! inx h! inx h ;HL=.currec
		shld curreca! inx h! inx h ;HL=.buffa
		;DE still contains .tran
		xchg! shld tranv ;.tran vector
		lxi h,buffa ;DE= source for move, HL=dest
		mvi c,addlist! call move ;addlist filled
		;now fill the disk parameter block
		lhld dpbaddr! xchg ;DE is source
		lxi h,sectpt ;HL is destination
		mvi c,dpblist! call move ;data filled
		;now set single/double map mode
		lhld maxall ;largest allocation number
		mov a,h ;00 indicates < 255
		lxi h,single! mvi m,true ;assume a=00
		ora a! jz retselect
		;high order of maxall not zero, use double dm
		mvi m,false
	retselect:
	mvi a,true! ora a! ret ;select disk function ok
;
home:
	;move to home position, then offset to start of dir
	call homef ;move to track 00, sector 00 reference
	;lxi h,offset ;mov c,m ;inx h ;mov b,m ;call settrkf	;
	;first directory position selected
	xra a ;constant zero to accumulator
	lhld curtrka! mov m,a! inx h! mov m,a ;curtrk=0000
	lhld curreca! mov m,a! inx h! mov m,a ;currec=0000
	;curtrk, currec both set to 0000
	ret
;
rdbuff:
	;read buffer and check condition
	call readf ;current drive, track, sector, dma
	jmp diocomp	;check for i/o errors
;
wrbuff:
	;write buffer and check condition
	;write type (wrtype) is in register C
	;wrtype = 0 => normal write operation
	;wrtype = 1 => directory write operation
	;wrtype = 2 => start of new block
	call writef ;current drive, track, sector, dma
diocomp:	;check for disk errors
	ora a! rz			;
	lxi	h,pererr			;
	jmp goerr
;
seekdir:
	;seek the record containing the current dir entry
	lhld dcnt ;directory counter to HL
	mvi c,dskshf! call hlrotr ;value to HL
	shld arecord! shld drec ;ready for seek
	;  jmp seek				;
	;ret
;
;
seek:
	;seek the track given by arecord (actual record)
	;local equates for registers
	arech  equ b! arecl  equ c ;arecord = BC
	crech  equ d! crecl  equ e ;currec  = DE
	ctrkh  equ h! ctrkl  equ l ;curtrk  = HL
	tcrech equ h! tcrecl equ l ;tcurrec = HL
	;load the registers from memory
	lxi h,arecord! mov arecl,m! inx h! mov arech,m
	lhld curreca ! mov crecl,m! inx h! mov crech,m
	lhld curtrka ! mov a,m! inx h! mov ctrkh,m! mov ctrkl,a
	;loop while arecord < currec
	seek0:
		mov a,arecl! sub crecl! mov a,arech! sbb crech
		jnc seek1 ;skip if arecord >= currec
			;currec = currec - sectpt
			push ctrkh! lhld sectpt
			mov a,crecl! sub l! mov crecl,a
			mov a,crech! sbb h! mov crech,a
			pop ctrkh
			;curtrk = curtrk - 1
			dcx ctrkh
		jmp seek0 ;for another try
	seek1:
	;look while arecord >= (t:=currec + sectpt)
		push ctrkh
		lhld sectpt! dad crech ;HL = currec+sectpt
		  jc seek2		;can be > FFFFH	
		mov a,arecl! sub tcrecl! mov a,arech! sbb tcrech
		jc seek2 ;skip if t > arecord
			;currec = t
			xchg
			;curtrk = curtrk + 1
			pop ctrkh! inx ctrkh
		jmp seek1 ;for another try
	seek2:	pop ctrkh
	;arrive here with updated values in each register
	push arech! push crech! push ctrkh ;to stack for later
	;stack contains (lowest) BC=arecord, DE=currec, HL=curtrk
	xchg! lhld offset! dad d ;HL = curtrk+offset
	mov b,h! mov c,l! call settrkf ;track set up
	;note that BC - curtrk is difference to move in bios
	pop d ;recall curtrk
	lhld curtrka! mov m,e! inx h! mov m,d ;curtrk updated
	;now compute sector as arecord-currec
	pop crech ;recall currec
	lhld curreca! mov m,crecl! inx h! mov m,crech
	pop arech ;BC=arecord, DE=currec
	mov a,arecl! sub crecl! mov arecl,a
	mov a,arech! sbb crech! mov arech,a
	lhld tranv! xchg ;BC=sector#, DE=.tran
	call sectran ;HL = tran(sector)
	mov c,l! mov b,h ;BC = tran(sector)
	jmp setsecf ;sector selected
	;ret
;
;	file control block (fcb) constants
empty	equ	0e5h	;empty directory entry
lstrec	equ	127	;last record# in extent
recsiz	equ	128	;record size
fcblen	equ	32	;file control block size
dirrec	equ	recsiz/fcblen	;directory elts / record
dskshf	equ	2	;log2(dirrec)
dskmsk	equ	dirrec-1
fcbshf	equ	5	;log2(fcblen)
;
extnum	equ	12	;extent number field
maxext	equ	31	;largest extent number
ubytes	equ	13	;unfilled bytes field
modnum	equ	14	;data module number
maxmod	equ	15	;largest module number
fwfmsk	equ	80h	;file write flag is high order modnum
namlen	equ	15	;name length
reccnt	equ	15	;record count field
dskmap	equ	16	;disk map field
lstfcb	equ	fcblen-1
nxtrec	equ	fcblen
ranrec	equ	nxtrec+1;random record field (2 bytes)
;
;	reserved file indicators
rofile	equ	9	;high order of first type char
invis	equ	10	;invisible file in dir command
;	equ	11	;reserved
;
;	utility functions for file access
;
dm$position:
	;compute disk map position for vrecord to HL
	lxi h,blkshf! mov c,m ;shift count to C
	lda vrecord ;current virtual record to A
	dmpos0:
		ora a! rar! dcr c! jnz dmpos0
	;A = shr(vrecord,blkshf) = vrecord/2**(sect/block)
	mov b,a ;save it for later addition
	mvi a,8! sub m ;8-blkshf to accumulator
	mov c,a ;extent shift count in register c
	lda extval ;extent value ani extmsk
	dmpos1:
		;blkshf = 3,4,5,6,7, C=5,4,3,2,1
		;shift is 4,3,2,1,0
		dcr c! jz dmpos2
		ora a! ral! jmp dmpos1
	dmpos2:
	;arrive here with A = shl(ext and extmsk,7-blkshf)
	add b ;add the previous shr(vrecord,blkshf) value
	;A is one of the following values, depending upon alloc
	;bks blkshf
	;1k   3     v/8 + extval * 16
	;2k   4     v/16+ extval * 8
	;4k   5     v/32+ extval * 4
	;8k   6     v/64+ extval * 2
	;16k  7     v/128+extval * 1
	ret ;with dm$position in A
;
getdm:
	;return disk map value from position given by BC
	lhld info ;base address of file control block
	lxi d,dskmap! dad d ;HL =.diskmap
	dad b ;index by a single byte value
	lda single ;single byte/map entry?
	ora a! jz getdmd ;get disk map single byte
		mov l,m! mvi h,0! ret ;with HL=00bb
	getdmd:
		dad b ;HL=.fcb(dm+i*2)
		;double precision value returned
		mov e,m! inx h! mov d,m! xchg! ret
;
index:
	;compute disk block number from current fcb
	call dm$position ;0...15 in register A
	mov c,a! mvi b,0! call getdm ;value to HL
	shld arecord! ret
;
allocated:
	;called following index to see if block allocated
	lhld arecord! mov a,l! ora h! ret
;
atran:
	;compute actual record address, assuming index called
	lda blkshf ;shift count to reg A
	lhld arecord
	atran0:
		dad h! dcr a! jnz atran0 ;shl(arecord,blkshf)
		shld arecord1		;save shifted block #  
	lda blkmsk! mov c,a ;mask value to C
	lda vrecord! ana c ;masked value in A
	ora l! mov l,a ;to HL
	shld arecord ;arecord=HL or (vrecord and blkmsk)
	ret
;
getexta:
	;get current extent field address to A
	lhld info! lxi d,extnum! dad d ;HL=.fcb(extnum)
	ret
;
getfcba:
	;compute reccnt and nxtrec addresses for get/setfcb
	lhld info! lxi d,reccnt! dad d! xchg ;DE=.fcb(reccnt)
	lxi h,(nxtrec-reccnt)! dad d ;HL=.fcb(nxtrec)
	ret
;
getfcb:
	;set variables from currently addressed fcb
	call getfcba ;addresses in DE, HL
	mov a,m! sta vrecord ;vrecord=fcb(nxtrec)
	xchg! mov a,m! sta rcount ;rcount=fcb(reccnt)
	call getexta ;HL=.fcb(extnum)
	lda extmsk ;extent mask to a
	ana m ;fcb(extnum) and extmsk
	sta extval
	ret
;
setfcb:
	;place values back into current fcb
	call getfcba ;addresses to DE, HL
	lda seqio
	cpi 02! jnz setfcb1! xra a	;check ranfill	
setfcb1:
 	mov c,a ;=1 if sequential i/o
	lda vrecord! add c! mov m,a ;fcb(nxtrec)=vrecord+seqio
	xchg! lda rcount! mov m,a ;fcb(reccnt)=rcount
	ret
;
hlrotr:
	;hl rotate right by amount C
	inr c ;in case zero
	hlrotr0: dcr c! rz ;return when zero
		mov a,h! ora a! rar! mov h,a ;high byte
		mov a,l! rar! mov l,a ;low byte
		jmp hlrotr0
;
;
compute$cs:
	;compute checksum for current directory buffer
	mvi c,recsiz ;size of directory buffer
	lhld buffa ;current directory buffer
	xra a ;clear checksum value
	computecs0:
		add m! inx h! dcr c ;cs=cs+buff(recsiz-C)
		jnz computecs0
	ret ;with checksum in A
;
hlrotl:
	;rotate the mask in HL by amount in C
	inr c ;may be zero
	hlrotl0: dcr c! rz ;return if zero
		dad h! jmp hlrotl0
;
set$cdisk:
	;set a "1" value in curdsk position of BC
	push b ;save input parameter
	lda curdsk! mov c,a ;ready parameter for shift
	lxi h,1 ;number to shift
	call hlrotl ;HL = mask to integrate
	pop b ;original mask
	mov a,c! ora l! mov l,a
	mov a,b! ora h! mov h,a ;HL = mask or rol(1,curdsk)
	ret
;
nowrite:
	;return true if dir checksum difference occurred
	lhld rodsk! lda curdsk! mov c,a! call hlrotr
	mov a,l! ani 1b! ret ;non zero if nowrite
;
set$ro:
	;set current disk to read only
	lxi h,rodsk! mov c,m! inx h! mov b,m
	call set$cdisk ;sets bit to 1
	shld rodsk
	;high water mark in directory goes to max
	lhld dirmax! inx h! xchg ;DE = directory max	
	lhld cdrmaxa ;HL = .cdrmax
	mov m,e! inx h! mov m,d ;cdrmax = dirmax
	ret
;
check$rodir:
	;check current directory element for read/only status
	call getdptra ;address of element
;						
check$rofile:
	;check current buff(dptr) or fcb(0) for r/o status
	lxi d,rofile! dad d ;offset to ro bit
	mov a,m! ral! rnc ;return if not set
	lxi h,roferr! jmp goerr			;
;	jmp rof$error ;exit to read only disk message
;
;
check$write:
	;check for write protected disk
	call nowrite! rz ;ok to write if not rodsk
	lxi h,roderr! jmp goerr
;	jmp rod$error ;read only disk error
;
getdptra:
	;compute the address of a directory element at
	;positon dptr in the buffer
	lhld buffa! lda dptr			;
addh:
	;HL = HL + A
	add l! mov l,a! rnc
	;overflow to H
	inr h! ret
;
;
getmodnum:
	;compute the address of the module number 
	;bring module number to accumulator
	;(high order bit is fwf (file write flag)
	lhld info! lxi d,modnum! dad d ;HL=.fcb(modnum)
	mov a,m! ret ;A=fcb(modnum)
;
clrmodnum:
	;clear the module number field for user open/make
	call getmodnum! mvi m,0 ;fcb(modnum)=0
	ret
;
setfwf:
	call getmodnum ;HL=.fcb(modnum), A=fcb(modnum)
	;set fwf (file write flag) to "1"
	ori fwfmsk! mov m,a ;fcb(modnum)=fcb(modnum) or 80h
	;also returns non zero in accumulator
	ret
;
;
compcdr:
	;return cy if cdrmax > dcnt
	lhld dcnt! xchg ;DE = directory counter
	lhld cdrmaxa ;HL=.cdrmax
	mov a,e! sub m ;low(dcnt) - low(cdrmax)
	inx h ;HL = .cdrmax+1
	mov a,d! sbb m ;hig(dcnt) - hig(cdrmax)
	;condition dcnt - cdrmax  produces cy if cdrmax>dcnt
	ret
;
setcdr:
	;if not (cdrmax > dcnt) then cdrmax = dcnt+1
	call compcdr
	rc ;return if cdrmax > dcnt
	;otherwise, HL = .cdrmax+1, DE = dcnt
	inx d! mov m,d! dcx h! mov m,e
	ret
;
subdh:
	;compute HL = DE - HL
	mov a,e! sub l! mov l,a! mov a,d! sbb h! mov h,a
	ret
;
newchecksum:
	mvi c,true ;drop through to compute new checksum
checksum:
	;compute current checksum record and update the
	;directory element if C=true, or check for = if not
	;drec < chksiz?
	lhld drec! xchg! lhld chksiz! call subdh ;DE-HL
	rnc ;skip checksum if past checksum vector size
		;drec < chksiz, so continue
		push b ;save init flag
		call compute$cs ;check sum value to A
		lhld checka ;address of check sum vector
		xchg
		lhld drec ;value of drec
		dad d ;HL = .check(drec)
		pop b ;recall true=0ffh or false=00 to C
		inr c ;0ffh produces zero flag
		jz initial$cs
			;not initializing, compare
			cmp m ;compute$cs=check(drec)?
			rz ;no message if ok
			;checksum error, are we beyond
			;the end of the disk?
			call compcdr
			rnc ;no message if so
			call set$ro ;read/only disk set
			ret
		initial$cs:
			;initializing the checksum
			mov m,a! ret
;
;
wrdir:
	;write the current directory entry, set checksum
	call newchecksum ;initialize entry
	call setdir ;directory dma
	mvi c,1 ;indicates a write directory operation
	call wrbuff ;write the buffer
        jmp setdata ;to data dma address
	;ret
;
rd$dir:
	;read a directory entry into the directory buffer
	call setdir ;directory dma
	call rdbuff ;directory record loaded
        ; jmp setdata to data dma address    
	;ret
;
setdata:
	;set data dma address
	lxi h,dmaad! jmp setdma ;to complete the call
;
setdir:
	;set directory dma address
	lxi h,buffa  ;jmp setdma to complete call     
;
setdma:
	;HL=.dma address to set (i.e., buffa or dmaad)
	mov c,m! inx h! mov b,m ;parameter ready
	jmp setdmaf
;
;
dir$to$user:
	;copy the directory entry to the user buffer
	;after call to search or searchn by user code
	lhld buffa! xchg ;source is directory buffer
	lhld dmaad ;destination is user dma address
	mvi c,recsiz ;copy entire record
	jmp move
	;ret
;
end$of$dir:
	;return zero flag if at end of directory, non zero
	;if not at end (end of dir if dcnt = 0ffffh)
	lxi h,dcnt! mov a,m ;may be 0ffh
	inx h! cmp m ;low(dcnt) = high(dcnt)?
	rnz ;non zero returned if different
	;high and low the same, = 0ffh?
	inr a ;0ffh becomes 00 if so
	ret
;
set$end$dir:
	;set dcnt to the end of the directory
	lxi h,enddir! shld dcnt! ret
;
read$dir:
	;read next directory entry, with C=true if initializing
	lhld dirmax! xchg ;in preparation for subtract
	lhld dcnt! inx h! shld dcnt ;dcnt=dcnt+1
	;continue while dirmax >= dcnt (dirmax-dcnt no cy)
	call subdh ;DE-HL
	jnc read$dir0
		;yes, set dcnt to end of directory
		jmp set$end$dir			;
;		ret				;
	read$dir0:
		;not at end of directory, seek next element
		;initialization flag is in C
		lda dcnt! ani dskmsk ;low(dcnt) and dskmsk
		mvi b,fcbshf ;to multiply by fcb size
		read$dir1:
			add a! dcr b! jnz read$dir1
		;A = (low(dcnt) and dskmsk) shl fcbshf
		sta dptr ;ready for next dir operation
		ora a! rnz ;return if not a new record
		push b ;save initialization flag C
		call seek$dir ;seek proper record
		call rd$dir ;read the directory record
		pop b ;recall initialization flag
		jmp checksum ;checksum the directory elt
		;ret
;
;
getallocbit:
	;given allocation vector position BC, return with byte
	;containing BC shifted so that the least significant
	;bit is in the low order accumulator position.  HL is
	;the address of the byte for possible replacement in
	;memory upon return, and D contains the number of shifts
	;required to place the returned value back into position
	mov a,c! ani 111b! inr a! mov e,a! mov d,a
	;d and e both contain the number of bit positions to shift
	mov a,c! rrc! rrc! rrc! ani 11111b! mov c,a ;C shr 3 to C
	mov a,b! add a! add a! add a! add a! add a ;B shl 5
	ora c! mov c,a ;bbbccccc to C
	mov a,b! rrc! rrc! rrc! ani 11111b! mov b,a ;BC shr 3 to BC
	lhld alloca ;base address of allocation vector
	dad b! mov a,m ;byte to A, hl = .alloc(BC shr 3)
	;now move the bit to the low order position of A
	rotl: rlc! dcr e! jnz rotl! ret
;
;
setallocbit:
	;BC is the bit position of ALLOC to set or reset.  The
	;value of the bit is in register E.
	push d! call getallocbit ;shifted val A, count in D
	ani 1111$1110b ;mask low bit to zero (may be set)
	pop b! ora c ;low bit of C is masked into A
;	jmp rotr ;to rotate back into proper position	
	;ret
rotr:
	;byte value from ALLOC is in register A, with shift count
	;in register C (to place bit back into position), and
	;target ALLOC position in registers HL, rotate and replace
	rrc! dcr d! jnz rotr ;back into position
	mov m,a ;back to ALLOC
	ret
;
scandm:
	;scan the disk map addressed by dptr for non-zero
	;entries, the allocation vector entry corresponding
	;to a non-zero entry is set to the value of C (0,1)
	call getdptra ;HL = buffa + dptr
	;HL addresses the beginning of the directory entry
	lxi d,dskmap! dad d ;hl now addresses the disk map
	push b ;save the 0/1 bit to set
	mvi c,fcblen-dskmap+1 ;size of single byte disk map + 1
	scandm0:
		;loop once for each disk map entry
		pop d ;recall bit parity
		dcr c! rz ;all done scanning?
		;no, get next entry for scan
		push d ;replace bit parity
		lda single! ora a! jz scandm1
			;single byte scan operation
			push b ;save counter
			push h ;save map address
			mov c,m! mvi b,0 ;BC=block#
			jmp scandm2
		scandm1:
			;double byte scan operation
			dcr c ;count for double byte
			push b ;save counter
			mov c,m! inx h! mov b,m ;BC=block#
			push h ;save map address
		scandm2:
			;arrive here with BC=block#, E=0/1
			mov a,c! ora b ;skip if = 0000
			jz scanm3
				lhld maxall	;check invalid index
			mov a,l! sub c! mov a,h! sbb b	;maxall - block#
			cnc set$alloc$bit			;
			;bit set to 0/1
		scanm3:						;
			pop h! inx h ;to next bit position
			pop b ;recall counter
			jmp scandm0 ;for another item
;
initialize:
	;initialize the current disk
	;lret = false ;set to true if $ file exists
	;compute the length of the allocation vector - 2
	lhld maxall! mvi c,3 ;perform maxall/8
	;number of bytes in alloc vector is (maxall/8)+1
	call hlrotr! inx h ;HL = maxall/8+1
	mov b,h! mov c,l ;count down BC til zero
	lhld alloca ;base of allocation vector
	;fill the allocation vector with zeros
	initial0:
		mvi m,0! inx h ;alloc(i)=0
		dcx b ;count length down
		mov a,b! ora c! jnz initial0
	;set the reserved space for the directory
	lhld dirblk! xchg
	lhld alloca ;HL=.alloc()
	mov m,e! inx h! mov m,d ;sets reserved directory blks
	;allocation vector initialized, home disk
	call home
        ;cdrmax = 3 (scans at least one directory record)
	lhld cdrmaxa! mvi m,3! inx h! mvi m,0
	;cdrmax = 0000
	call set$end$dir ;dcnt = enddir
	;read directory entries and check for allocated storage
	initial2:
		mvi c,true! call read$dir
		call end$of$dir! rz ;return if end of directory
		;not end of directory, valid entry?
		call getdptra ;HL = buffa + dptr
		mvi a,empty! cmp m
		jz initial2 ;go get another item
		;not empty, user code the same?
		lda usrcode
		cmp m! jnz pdollar
		;same user code, check for '$' submit
		inx h! mov a,m ;first character
		sui '$' ;dollar file?
		jnz pdollar
		;dollar file found, mark in lret
		dcr a! sta lret ;lret = 255
	pdollar:
		;now scan the disk map for allocated blocks
		mvi c,1 ;set to allocated
		call scandm
		call setcdr ;set cdrmax to dcnt
		jmp initial2 ;for another entry
;
copy$dirloc:
	;copy directory location to lret following
	;delete, rename, ... ops
	lda dirloc! jmp sta$ret				;
;	ret						;
;
compext:
	;compare extent# in A with that in C, return nonzero
	;if they do not match
	push b ;save C's original value
	push psw! lda extmsk! cma! mov b,a
	;B has negated form of extent mask
	mov a,c! ana b! mov c,a ;low bits removed from C
	pop psw! ana b ;low bits removed from A
	sub c! ani maxext ;set flags
	pop b ;restore original values
	ret
;
search:
	;search for directory element of length C at info
	mvi a,0ffh! sta dirloc ;changed if actually found
	lxi h,searchl! mov m,c ;searchl = C
	lhld info! shld searcha ;searcha = info
	call set$end$dir ;dcnt = enddir
	call home ;to start at the beginning
	;(drop through to searchn)			;
;
searchn:
	;search for the next directory element, assuming
	;a previous call on search which sets searcha and
	;searchl
	mvi c,false! call read$dir ;read next dir element
	call end$of$dir! jz search$fin ;skip to end if so
		;not end of directory, scan for match
		lhld searcha! xchg ;DE=beginning of user fcb
		ldax d ;first character
		cpi empty ;keep scanning if empty
		jz searchnext
		;not empty, may be end of logical directory
		push d ;save search address
		call compcdr ;past logical end?
		pop d ;recall address
		jnc search$fin ;artificial stop
	searchnext:
		call getdptra ;HL = buffa+dptr
		lda searchl! mov c,a ;length of search to c
		mvi b,0 ;b counts up, c counts down
		searchloop:
			mov a,c! ora a! jz endsearch
			ldax d! cpi '?'! jz searchok ;? matches all
			;scan next character if not ubytes
			mov a,b! cpi ubytes! jz searchok
			;not the ubytes field, extent field?
			cpi extnum ;may be extent field
			ldax d ;fcb character
			jz searchext ;skip to search extent
			sub m! ani 7fh ;mask-out flags/extent modulus
			jnz searchn ;skip if not matched
			jmp searchok ;matched character
		searchext:
			;A has fcb character
			;attempt an extent # match
			push b ;save counters
			mov c,m ;directory character to c
			call compext ;compare user/dir char
			pop b ;recall counters
			jnz searchn ;skip if no match
		searchok:
			;current character matches
			inx d! inx h! inr b! dcr c
			jmp searchloop
		endsearch:
			;entire name matches, return dir position
			lda dcnt! ani dskmsk! sta lret
			;lret = low(dcnt) and 11b
			lxi h,dirloc! mov a,m! ral! rnc ;dirloc=0ffh?
			;yes, change it to 0 to mark as found
			xra a! mov m,a ;dirloc=0
			ret
		search$fin:
			;end of directory, or empty name
			call set$end$dir ;may be artifical end
			mvi a,255! jmp sta$ret		;
;
;
delete:
	;delete the currently addressed file
	call check$write ;write protected?
	mvi c,extnum! call search ;search through file type
	delete0:
		;loop while directory matches
		call end$of$dir! rz ;stop if end
		;set each non zero disk map entry to 0
		;in the allocation vector
		;may be r/o file
		call check$rodir ;ro disk error if found
		call getdptra ;HL=.buff(dptr)
		mvi m,empty
		mvi c,0! call scandm ;alloc elts set to 0
		call wrdir ;write the directory
		call searchn ;to next element
		jmp delete0 ;for another record
;
get$block:
	;given allocation vector position BC, find the zero bit
	;closest to this position by searching left and right.
	;if found, set the bit to one and return the bit position
	;in hl.  if not found (i.e., we pass 0 on the left, or
	;maxall on the right), return 0000 in hl
	mov d,b! mov e,c ;copy of starting position to de
	lefttst:
		mov a,c! ora b! jz righttst ;skip if left=0000
		;left not at position zero, bit zero?
		dcx b! push d! push b ;left,right pushed
		call getallocbit
		rar! jnc retblock ;return block number if zero
		;bit is one, so try the right
		pop b! pop d ;left, right restored
	righttst:
		lhld maxall ;value of maximum allocation#
		mov a,e! sub l! mov a,d! sbb h ;right=maxall?
		jnc retblock0 ;return block 0000 if so
		inx d! push b! push d ;left, right pushed
		mov b,d! mov c,e ;ready right for call
		call getallocbit
		rar! jnc retblock ;return block number if zero
		pop d! pop b ;restore left and right pointers
		jmp lefttst ;for another attempt
	retblock:
		ral! inr a ;bit back into position and set to 1
		;d contains the number of shifts required to reposition
		call rotr ;move bit back to position and store
		pop h! pop d ;HL returned value, DE discarded
		ret
	retblock0:
		;cannot find an available bit, return 0000
		mov a,c				;
		ora b! jnz lefttst	;also at beginning    
		lxi h,0000h! ret
;
copy$fcb:
	;copy the entire file control block
	mvi c,0! mvi e,fcblen ;start at 0, to fcblen-1
	;	jmp copy$dir			;
;
copy$dir:
	;copy fcb information starting at C for E bytes
	;into the currently addressed directory entry
	push d ;save length for later
	mvi b,0 ;double index to BC
	lhld info ;HL = source for data
	dad b! xchg ;DE=.fcb(C), source for copy
	call getdptra ;HL=.buff(dptr), destination
	pop b ;DE=source, HL=dest, C=length
	call move ;data moved
seek$copy:
	;enter from close to seek and copy current element
	call seek$dir ;to the directory element
	jmp wrdir ;write the directory element
	;ret
;
;
rename:
	;rename the file described by the first half of
	;the currently addressed file control block. the
	;new name is contained in the last half of the
	;currently addressed file conrol block.  the file
	;name and type are changed, but the reel number
	;is ignored.  the user number is identical
	call check$write ;may be write protected
	;search up to the extent field
	mvi c,extnum! call search
	;copy position 0
	lhld info! mov a,m ;HL=.fcb(0), A=fcb(0)
	lxi d,dskmap! dad d;HL=.fcb(dskmap)
	mov m,a ;fcb(dskmap)=fcb(0)
	;assume the same disk drive for new named file
	rename0:
		call end$of$dir! rz ;stop at end of dir
		;not end of directory, rename next element
		call check$rodir ;may be read-only file
		mvi c,dskmap! mvi e,extnum! call copy$dir
		;element renamed, move to next
		call searchn
		jmp rename0
;
indicators:
	;set file indicators for current fcb
	mvi c,extnum! call search ;through file type
	indic0:
		call end$of$dir! rz ;stop at end of dir
		;not end of directory, continue to change
		mvi c,0! mvi e,extnum ;copy name
		call copy$dir
		call searchn
		jmp indic0
;
open:
	;search for the directory entry, copy to fcb
	mvi c,namlen! call search
	call end$of$dir! rz ;return with lret=255 if end
	;not end of directory, copy fcb information
open$copy:
	;(referenced below to copy fcb info)
	call getexta! mov a,m! push psw! push h ;save extent#
	call getdptra! xchg ;DE = .buff(dptr)
	lhld info ;HL=.fcb(0)
	mvi c,nxtrec ;length of move operation
	push d ;save .buff(dptr)
	call move ;from .buff(dptr) to .fcb(0)
	;note that entire fcb is copied, including indicators
	call setfwf ;sets file write flag
	pop d! lxi h,extnum! dad d ;HL=.buff(dptr+extnum)
	mov c,m ;C = directory extent number
	lxi h,reccnt! dad d ;HL=.buff(dptr+reccnt)
	mov b,m ;B holds directory record count
	pop h! pop psw! mov m,a ;restore extent number
	;HL = .user extent#, B = dir rec cnt, C = dir extent#
	;if user ext < dir ext then user := 128 records
	;if user ext = dir ext then user := dir records
	;if user ext > dir ext then user := 0 records
		mov a,c! cmp m! mov a,b ;ready dir reccnt
		jz open$rcnt ;if same, user gets dir reccnt
		mvi a,0! jc open$rcnt ;user is larger
		mvi a,128 ;directory is larger
	open$rcnt: ;A has record count to fill
	lhld info! lxi d,reccnt! dad d! mov m,a
	ret
;
mergezero:
	;HL = .fcb1(i), DE = .fcb2(i),
	;if fcb1(i) = 0 then fcb1(i) := fcb2(i)
	mov a,m! inx h! ora m! dcx h! rnz ;return if = 0000
	ldax d! mov m,a! inx d! inx h ;low byte copied
	ldax d! mov m,a! dcx d! dcx h ;back to input form
	ret
;
close:
	;locate the directory element and re-write it
	xra a! sta lret! sta dcnt! sta dcnt+1	;
	call nowrite! rnz ;skip close if r/o disk
	;check file write flag - 0 indicates written
	call getmodnum ;fcb(modnum) in A
	ani fwfmsk! rnz ;return if bit remains set
	mvi c,namlen! call search ;locate file
	call end$of$dir! rz ;return if not found
	;merge the disk map at info with that at buff(dptr)
	lxi b,dskmap! call getdptra
	dad b! xchg ;DE is .buff(dptr+16)
	lhld info! dad b ;DE=.buff(dptr+16), HL=.fcb(16)
	mvi c,(fcblen-dskmap) ;length of single byte dm
	merge0:
		lda single! ora a! jz merged ;skip to double
		;this is a single byte map
		;if fcb(i) = 0 then fcb(i) = buff(i)
		;if buff(i) = 0 then buff(i) = fcb(i)
		;if fcb(i) <> buff(i) then error
		mov a,m! ora a! ldax d! jnz fcbnzero
			;fcb(i) = 0
			mov m,a ;fcb(i) = buff(i)
		fcbnzero:
		ora a! jnz buffnzero
			;buff(i) = 0
			mov a,m! stax d ;buff(i)=fcb(i)
		buffnzero:
		cmp m! jnz mergerr ;fcb(i) = buff(i)?
		jmp dmset ;if merge ok
	merged:
		;this is a double byte merge operation
		call mergezero ;buff = fcb if buff 0000
		xchg! call mergezero! xchg ;fcb = buff if fcb 0000
		;they should be identical at this point
		ldax d! cmp m! jnz mergerr ;low same?
		inx d! inx h ;to high byte
		ldax d! cmp m! jnz mergerr ;high same?
		;merge operation ok for this pair
		dcr c ;extra count for double byte
	dmset:
		inx d! inx h ;to next byte position
		dcr c! jnz merge0 ;for more
		;end of disk map merge, check record count
		;DE = .buff(dptr)+32, HL = .fcb(32)
		lxi b,-(fcblen-extnum)! dad b! xchg! dad b
		;DE = .fcb(extnum), HL = .buff(dptr+extnum)
		ldax d ;current user extent number
		;if fcb(ext) >= buff(fcb) then
		;buff(ext) := fcb(ext), buff(rec) := fcb(rec)
		cmp m! jc endmerge
		;fcb extent number >= dir extent number
		mov m,a ;buff(ext) = fcb(ext)
		;update directory record count field
		lxi b,(reccnt-extnum)! dad b! xchg! dad b
		;DE=.buff(reccnt), HL=.fcb(reccnt)
		mov a,m! stax d ;buff(reccnt)=fcb(reccnt)
	endmerge:
		mvi a,true! sta fcb$copied ;mark as copied
		jmp seek$copy ;ok to "wrdir" here - 1.4 compat
	;		ret				;
	mergerr:
		;elements did not merge correctly
		lxi h,lret! dcr m ;=255 non zero flag set
	ret
;
make:
	;create a new file by creating a directory entry
	;then opening the file
	call check$write ;may be write protected
	lhld info! push h ;save fcb address, look for e5
	lxi h,efcb! shld info ;info = .empty
	mvi c,1! call search ;length 1 match on empty entry
	call end$of$dir ;zero flag set if no space
	pop h ;recall info address
	shld info ;in case we return here
	rz ;return with error condition 255 if not found
	xchg ;DE = info address
	;clear the remainder of the fcb
	lxi h,namlen! dad d ;HL=.fcb(namlen)
	mvi c,fcblen-namlen ;number of bytes to fill
	xra a ;clear accumulator to 00 for fill
	make0:
		mov m,a! inx h! dcr c! jnz make0
	lxi h,ubytes! dad d ;HL = .fcb(ubytes)
	mov m,a ;fcb(ubytes) = 0
	call setcdr ;may have extended the directory
	;now copy entry to the directory
	call copy$fcb
	;and set the file write flag to "1"
	jmp setfwf
	;ret
;
open$reel:
	;close the current extent, and open the next one
	;if possible.  RMF is true if in read mode
		xra a! sta fcb$copied ;set true if actually copied
		call close ;close current extent
		;lret remains at enddir if we cannot open the next ext
		call end$of$dir! rz ;return if end
	;increment extent number
	lhld info! lxi b,extnum! dad b ;HL=.fcb(extnum)
	mov a,m! inr a! ani maxext! mov m,a ;fcb(extnum)=++1
	jz open$mod ;move to next module if zero
	;may be in the same extent group
	mov b,a! lda extmsk! ana b
	;if result is zero, then not in the same group
	lxi h,fcb$copied ;true if the fcb was copied to directory
	ana m ;produces a 00 in accumulator if not written
	jz open$reel0 ;go to next physical extent
	;result is non zero, so we must be in same logical ext
	jmp open$reel1 ;to copy fcb information
	open$mod:
		;extent number overflow, go to next module
		lxi b,(modnum-extnum)! dad b ;HL=.fcb(modnum)
		inr m ;fcb(modnum)=++1
		;module number incremented, check for overflow
		mov a,m! ani maxmod ;mask high order bits
		jz open$r$err ;cannot overflow to zero
		;otherwise, ok to continue with new module
	open$reel0:
		mvi c,namlen! call search ;next extent found?
		call end$of$dir! jnz open$reel1
			;end of file encountered
			lda rmf! inr a ;0ffh becomes 00 if read
			jz open$r$err ;sets lret = 1
			;try to extend the current file
			call make
			;cannot be end of directory
			call end$of$dir
			jz open$r$err ;with lret = 1
			jmp open$reel2
		open$reel1:
			;not end of file, open
			call open$copy
		open$reel2:
			call getfcb ;set parameters
			xra a! jmp sta$ret ;lret = 0	;
;			ret ;with lret = 0
	open$r$err:
		;cannot move to next extent of this file
		call setlret1 ;lret = 1
		jmp setfwf ;ensure that it will not be closed
		;ret
;
seqdiskread:
	;sequential disk read operation
	mvi a,1! sta seqio
	;drop through to diskread
;
diskread:	;(may enter from seqdiskread)
	mvi a,true! sta rmf ;read mode flag = true (open$reel)
	;read the next record from the current fcb
	call getfcb ;sets parameters for the read
	lda vrecord! lxi h,rcount! cmp m ;vrecord-rcount
	;skip if rcount > vrecord
	jc recordok
		;not enough records in the extent
		;record count must be 128 to continue
		cpi 128 ;vrecord = 128?
		jnz diskeof ;skip if vrecord<>128
		call open$reel ;go to next extent if so
		xra a! sta vrecord ;vrecord=00
		;now check for open ok
		lda lret! ora a! jnz diskeof ;stop at eof
	recordok:
		;arrive with fcb addressing a record to read
		call index
		;error 2 if reading unwritten data
		;(returns 1 to be compatible with 1.4)
		call allocated ;arecord=0000?
		jz diskeof
		;record has been allocated, read it
		call atran ;arecord now a disk address
		call seek ;to proper track,sector
		call rdbuff ;to dma address
		jmp setfcb ;replace parameter	
;		ret					;
	diskeof:
		jmp setlret1 ;lret = 1
		;ret
;
seqdiskwrite:
	;sequential disk write
	mvi a,1! sta seqio
	;drop through to diskwrite
;
diskwrite:	;(may enter here from seqdiskwrite above)
	mvi a,false! sta rmf ;read mode flag
	;write record to currently selected file
	call check$write ;in case write protected
	lhld info ;HL = .fcb(0)
	call check$rofile ;may be a read-only file
	call getfcb ;to set local parameters
	lda vrecord! cpi lstrec+1 ;vrecord-128
	;skip if vrecord > lstrec
		;vrecord = 128, cannot open next extent
		jnc setlret1	 ;lret=1		;
	diskwr0:
	;can write the next record, so continue
	call index
	call allocated
	mvi c,0 ;marked as normal write operation for wrbuff
	jnz diskwr1
		;not allocated
		;the argument to getblock is the starting
		;position for the disk search, and should be
		;the last allocated block for this file, or
		;the value 0 if no space has been allocated
		call dm$position
		sta dminx ;save for later
		lxi b,0000h ;may use block zero
		ora a! jz nopblock ;skip if no previous block
			;previous block exists at A
			mov c,a! dcx b ;previous block # in BC
			call getdm ;previous block # to HL
			mov b,h! mov c,l ;BC=prev block#
		nopblock:
			;BC = 0000, or previous block #
			call get$block ;block # to HL
		;arrive here with block# or zero
		mov a,l! ora h! jnz blockok
			;cannot find a block to allocate
			mvi a,2! jmp sta$ret 	;lret=2	
		blockok:
		;allocated block number is in HL
		shld arecord
		xchg ;block number to DE
		lhld info! lxi b,dskmap! dad b ;HL=.fcb(dskmap)
		lda single! ora a ;set flags for single byte dm
		lda dminx ;recall dm index
		jz allocwd ;skip if allocating word
			;allocating a byte value
			call addh! mov m,e ;single byte alloc
			jmp diskwru ;to continue
		allocwd:
		;allocate a word value
			mov c,a! mvi b,0 ;double(dminx)
			dad b! dad b ;HL=.fcb(dminx*2)
			mov m,e! inx h! mov m,d ;double wd
		diskwru:
		;disk write to previously unallocated block
		mvi c,2 ;marked as unallocated write
	diskwr1:
	;continue the write operation of no allocation error
	;C = 0 if normal write, 2 if to prev unalloc block
	lda lret! ora a! rnz ;stop if non zero returned value
	push b ;save write flag
	call atran ;arecord set
		lda seqio! dcr a! dcr a! jnz diskwr11   ;
		pop b! push b! mov a,c! dcr a! dcr a	;
		jnz diskwr11		;old allocation  
		push h		;arecord in hl ret from atran
		lhld buffa! mov d,a	;zero buffa & fill 
	fill0:  mov m,a! inx h! inr d! jp fill0		;
		call setdir! lhld arecord1		;
		mvi c,2					;
	fill1:  shld arecord! push b! call seek! pop b  ;
		call wrbuff	;write fill record	;
		lhld arecord!	;restore last record     
		mvi c,0		;change  allocate flag   
		lda blkmsk! mov b,a! ana l! cmp b!inx h	;
		jnz fill1	;cont until cluster is zeroed
		pop h! shld arecord! call setdata
	diskwr11:					;
	call seek ;to proper file position
	pop b! push b ;restore/save write flag (C=2 if new block)
	call wrbuff ;written to disk
	pop b ;C = 2 if a new block was allocated, 0 if not
	;increment record count if rcount<=vrecord
	lda vrecord! lxi h,rcount! cmp m ;vrecord-rcount
	jc diskwr2
		;rcount <= vrecord
		mov m,a! inr m ;rcount = vrecord+1
		mvi c,2 ;mark as record count incremented
	diskwr2:
	;A has vrecord, C=2 if new block or new record#
	dcr c! dcr c! jnz noupdate
		push psw ;save vrecord value
		call getmodnum ;HL=.fcb(modnum), A=fcb(modnum)
		;reset the file write flag to mark as written fcb
		ani (not fwfmsk) and 0ffh ;bit reset
		mov m,a ;fcb(modnum) = fcb(modnum) and 7fh
		pop psw ;restore vrecord
	noupdate:
	;check for end of extent, if found attempt to open
	;next extent in preparation for next write
	cpi lstrec ;vrecord=lstrec?
	jnz diskwr3 ;skip if not
	;may be random access write, if so we are done
					;change next     
	lda seqio! cpi 1! jnz diskwr3 ;skip next extent open op
		;update current fcb before going to next extent
		call setfcb
		call open$reel ;rmf=false
		;vrecord remains at lstrec causing eof if
		;no more directory space is available
		lxi h,lret! mov a,m! ora a! jnz nospace
			;space available, set vrecord=255
			dcr a! sta vrecord ;goes to 00 next time
		nospace:
		mvi m,0 ;lret = 00 for returned value
	diskwr3:
	jmp setfcb ;replace parameters
	;ret
;
rseek:
	;random access seek operation, C=0ffh if read mode
	;fcb is assumed to address an active file control block
	;(modnum has been set to 1100$0000b if previous bad seek)
	xra a! sta seqio ;marked as random access operation
rseek1:
	push b ;save r/w flag
	lhld info! xchg ;DE will hold base of fcb
		lxi h,ranrec! dad d ;HL=.fcb(ranrec)
		mov a,m! ani 7fh! push psw ;record number
		mov a,m! ral ;cy=lsb of extent#
		inx h! mov a,m! ral! ani 11111b ;A=ext#
		mov c,a ;C holds extent number, record stacked
		mov a,m! rar! rar! rar! rar! ani 1111b ;mod#
		mov b,a ;B holds module#, C holds ext#
		pop psw ;recall sought record #
		;check to insure that high byte of ran rec = 00
		inx h! mov l,m ;l=high byte (must be 00)
		inr l! dcr l! mvi l,6 ;zero flag, l=6
		;produce error 6, seek past physical eod
		jnz seekerr
		;otherwise, high byte = 0, A = sought record
		lxi h,nxtrec! dad d ;HL = .fcb(nxtrec)
		mov m,a ;sought rec# stored away
	;arrive here with B=mod#, C=ext#, DE=.fcb, rec stored
	;the r/w flag is still stacked.  compare fcb values
		lxi h,extnum! dad d! mov a,c ;A=seek ext#
		sub m! jnz ranclose ;tests for = extents
		;extents match, check mod#
		lxi h,modnum! dad d! mov a,b ;B=seek mod#
		;could be overflow at eof, producing module#
		;of 90H or 10H, so compare all but fwf
		sub m! ani 7fh! jz seekok ;same?
	ranclose:
		push b! push d ;save seek mod#,ext#, .fcb
		call close ;current extent closed
		pop d! pop b ;recall parameters and fill
		mvi l,3 ;cannot close error #3
		lda lret! inr a! jz badseek
		lxi h,extnum! dad d! mov m,c ;fcb(extnum)=ext#
		lxi h,modnum! dad d! mov m,b ;fcb(modnum)=mod#
		call open ;is the file present?
		lda lret! inr a! jnz seekok ;open successful?
		;cannot open the file, read mode?
		pop b ;r/w flag to c (=0ffh if read)
		push b ;everyone expects this item stacked
		mvi l,4 ;seek to unwritten extent #4
		inr c ;becomes 00 if read operation
		jz badseek ;skip to error if read operation
		;write operation, make new extent
		call make
		mvi l,5 ;cannot create new extent #5
		lda lret! inr a! jz badseek ;no dir space
		;file make operation successful
	seekok:
		pop b ;discard r/w flag
		xra a! jmp sta$ret 	;with zero set	
	badseek:
		;fcb no longer contains a valid fcb, mark
		;with 1100$000b in modnum field so that it
		;appears as overflow with file write flag set
		push h ;save error flag
		call getmodnum ;HL = .modnum
		mvi m,1100$0000b
		pop h ;and drop through
	seekerr:
		pop b ;discard r/w flag
		mov a,l! sta lret ;lret=#, nonzero
		;setfwf returns non-zero accumulator for err
		jmp setfwf ;flag set, so subsequent close ok
		;ret
;
randiskread:
	;random disk read operation
	mvi c,true ;marked as read operation
	call rseek
	cz diskread ;if seek successful
	ret
;
randiskwrite:
	;random disk write operation
	mvi c,false ;marked as write operation
	call rseek
	cz diskwrite ;if seek successful
	ret
;
compute$rr:
	;compute random record position for getfilesize/setrandom
	xchg! dad d
	;DE=.buf(dptr) or .fcb(0), HL = .f(nxtrec/reccnt)
	mov c,m! mvi b,0 ;BC = 0000 0000 ?rrr rrrr
	lxi h,extnum! dad d! mov a,m! rrc! ani 80h ;A=e000 0000
	add c! mov c,a! mvi a,0! adc b! mov b,a
	;BC = 0000 000? errrr rrrr
	mov a,m! rrc! ani 0fh! add b! mov b,a
	;BC = 000? eeee errrr rrrr
	lxi h,modnum! dad d! mov a,m ;A=XXX? mmmm
	add a! add a! add a! add a ;cy=? A=mmmm 0000
	push psw! add b! mov b,a
	;cy=?, BC = mmmm eeee errr rrrr
	push psw ;possible second carry
	pop h ;cy = lsb of L
	mov a,l ;cy = lsb of A
	pop h ;cy = lsb of L
	ora l ;cy/cy = lsb of A
	ani 1 ;A = 0000 000? possible carry-out
	ret
;
getfilesize:
	;compute logical file size for current fcb
	mvi c,extnum
	call search
	;zero the receiving ranrec field
	lhld info! lxi d,ranrec! dad d! push h ;save position
	mov m,d! inx h! mov m,d! inx h! mov m,d;=00 00 00
	getsize:
		call end$of$dir
		jz setsize
		;current fcb addressed by dptr
		call getdptra! lxi d,reccnt ;ready for compute size
		call compute$rr
		;A=0000 000? BC = mmmm eeee errr rrrr
		;compare with memory, larger?
		pop h! push h ;recall, replace .fcb(ranrec)
		mov e,a ;save cy
		mov a,c! sub m! inx h ;ls byte
		mov a,b! sbb m! inx h ;middle byte
		mov a,e! sbb m ;carry if .fcb(ranrec) > directory
		jc getnextsize ;for another try
		;fcb is less or equal, fill from directory
		mov m,e! dcx h! mov m,b! dcx h! mov m,c
	getnextsize:
		call searchn
		jmp getsize
	setsize:
	pop h ;discard .fcb(ranrec)
	ret
;
setrandom:
	;set random record from the current file control block
	lhld info! lxi d,nxtrec ;ready params for computesize
	call compute$rr ;DE=info, A=cy, BC=mmmm eeee errr rrrr
	lxi h,ranrec! dad d ;HL = .fcb(ranrec)
	mov m,c! inx h! mov m,b! inx h! mov m,a ;to ranrec
	ret
;
select:
	;select disk info for subsequent input or output ops
	lhld dlog! lda curdsk! mov c,a! call hlrotr
	push h! xchg ;save it for test below, send to seldsk
	call selectdisk! pop h ;recall dlog vector
	cz sel$error ;returns true if select ok
	;is the disk logged in?
	mov a,l! rar! rc ;return if bit is set
	;disk not logged in, set bit and initialize
	lhld dlog! mov c,l! mov b,h ;call ready
	call set$cdisk! shld dlog ;dlog=set$cdisk(dlog)
	jmp initialize
	;ret
;
curselect:
	lda linfo! lxi h,curdsk! cmp m! rz ;skip if linfo=curdsk
	mov m,a ;curdsk=info
	jmp select
	;ret
;
reselect:
	;check current fcb to see if reselection necessary
	mvi a,true! sta resel ;mark possible reselect
	lhld info! mov a,m ;drive select code
	ani 1$1111b ;non zero is auto drive select
	dcr a ;drive code normalized to 0..30, or 255
	sta linfo ;save drive code
	cpi 30! jnc noselect
		;auto select function, save curdsk
		lda curdsk! sta olddsk ;olddsk=curdsk
		mov a,m! sta fcbdsk ;save drive code
		ani 1110$0000b! mov m,a ;preserve hi bits
		call curselect
	noselect:
		;set user code
		lda usrcode ;0...31
		lhld info! ora m! mov m,a
		ret
;
;	individual function handlers
func12:
	;return version number
	mvi a,dvers! jmp sta$ret ;lret = dvers (high = 00)
;	ret ;jmp goback
;
func13:
	;reset disk system - initialize to disk 0
	lxi h,0! shld rodsk! shld dlog
	xra a! sta curdsk ;note that usrcode remains unchanged
	lxi h,tbuff! shld dmaad ;dmaad = tbuff
        call setdata ;to data dma address
	jmp select
	;ret ;jmp goback
;
func14:	equ	curselect			;
	;select disk info
	;ret ;jmp goback
;
func15:
	;open file
	call clrmodnum ;clear the module number
	call reselect
	jmp open
	;ret ;jmp goback
;
func16:
	;close file
	call reselect
	jmp close
	;ret ;jmp goback
;
func17:
	;search for first occurrence of a file
	mvi c,0 ;length assuming '?' true
	xchg		;was lhld info		
	mov a,m! cpi '?' ;no reselect if ?
	jz qselect ;skip reselect if so
		;normal search
		call getexta! mov a,m! cpi '?'	;	
		cnz clrmodnum ;module number zeroed
		call reselect
		mvi c,namlen
	qselect:
	call search
	jmp dir$to$user ;copy directory entry to user
	;ret ;jmp goback
;
func18:
	;search for next occurrence of a file name
	lhld searcha! shld info
	call reselect! call searchn
	jmp dir$to$user ;copy directory entry to user
	;ret ;jmp goback
;
func19:
	;delete a file
	call reselect
	call delete
	jmp copy$dirloc
	;ret ;jmp goback
;
func20:
	;read a file
	call reselect
	jmp seqdiskread				;
	 ;jmp goback
;
func21:
	;write a file
	call reselect
	jmp seqdiskwrite			;
	 ;jmp goback
;
func22:
	;make a file
	call clrmodnum
	call reselect
	jmp make
	;ret ;jmp goback
;
func23:
	;rename a file
	call reselect
	call rename
	jmp copy$dirloc
	;ret ;jmp goback
;
func24:
	;return the login vector
	lhld dlog! jmp sthl$ret			;
;	ret ;jmp goback
;
func25:
	;return selected disk number
	lda curdsk! jmp sta$ret			;
;	ret ;jmp goback
;
func26:
	;set the subsequent dma address to info
	xchg		;was lhld info	
	shld dmaad ;dmaad = info
        jmp setdata ;to data dma address
	;ret ;jmp goback
;
func27:
	;return the login vector address
	lhld alloca! jmp sthl$ret		;
;	ret ;jmp goback
;
func28:	equ	set$ro				;
	;write protect current disk
	;ret ;jmp goback
;
func29:
	;return r/o bit vector
	lhld rodsk! jmp sthl$ret		;
;	ret ;jmp goback
;
func30:
	;set file indicators
	call reselect
	call indicators
	jmp copy$dirloc ;lret=dirloc
	;ret ;jmp goback
;
func31:
	;return address of disk parameter block
	lhld dpbaddr
sthl$ret:
 	shld aret
	ret ;jmp goback
func32:
	;set user code
        lda linfo! cpi 0ffh! jnz setusrcode
		;interrogate user code instead
		lda usrcode! jmp sta$ret ;lret=usrcode	
;		ret ;jmp goback
	setusrcode:
		ani 1fh! sta usrcode
		ret ;jmp goback
;
func33:
	;random disk read operation
	call reselect
	jmp randiskread ;to perform the disk read
	;ret ;jmp goback
;
func34:
	;random disk write operation
	call reselect
	jmp randiskwrite ;to perform the disk write
	;ret ;jmp goback
;
func35:
	;return file size (0-65536)
	call reselect
	jmp getfilesize
	;ret ;jmp goback
;
func36:	equ	setrandom			;
	;set random record
	;ret ;jmp goback
func37:
;
	lhld	info
	mov	a,l! cma! mov e,a! mov a,h! cma
	lhld dlog! ana h! mov d,a! mov a,l! ana e
	mov e,a! lhld rodsk! xchg! shld dlog
	mov a,l! ana e! mov l,a
	mov a,h! ana d! mov h,a
	shld rodsk! ret
;
;
goback:
	;arrive here at end of processing to return to user
	lda resel! ora a! jz retmon
		;reselection may have taken place
		lhld info! mvi m,0 ;fcb(0)=0
		lda fcbdsk! ora a! jz retmon
		;restore disk number
		mov m,a ;fcb(0)=fcbdsk
		lda olddsk! sta linfo! call curselect
;
;	return from the disk monitor
retmon:
	lhld entsp! sphl ;user stack restored
	lhld aret! mov a,l! mov b,h ;BA = HL = aret
	ret
func38:	equ	func$ret
func39	equ	func$ret
func40:
	;random disk write with zero fill of unallocated block
	call reselect
	mvi a,2! sta seqio
	mvi	c,false
	call	rseek1
	cz	diskwrite	;if seek successful
	ret
;
;
;	data areas
;
;	initialized data
efcb:	db	empty	;0e5=available dir entry
rodsk:	dw	0	;read only disk vector
dlog:	dw	0	;logged-in disks
dmaad:	dw	tbuff	;initial dma address
;
;	curtrka - alloca are set upon disk select
;	(data must be adjacent, do not insert variables)
;	(address of translate vector, not used)
cdrmaxa:ds	word	;pointer to cur dir max value
curtrka:ds	word	;current track address
curreca:ds	word	;current record address
buffa:	ds	word	;pointer to directory dma address
dpbaddr:ds	word	;current disk parameter block address
checka:	ds	word	;current checksum vector address
alloca:	ds	word	;current allocation vector address
addlist	equ	$-buffa	;address list size
;
;	sectpt - offset obtained from disk parm block at dpbaddr
;	(data must be adjacent, do not insert variables)
sectpt:	ds	word	;sectors per track
blkshf:	ds	byte	;block shift factor
blkmsk:	ds	byte	;block mask
extmsk:	ds	byte	;extent mask
maxall:	ds	word	;maximum allocation number
dirmax:	ds	word	;largest directory number
dirblk:	ds	word	;reserved allocation bits for directory
chksiz:	ds	word	;size of checksum vector
offset:	ds	word	;offset tracks at beginning
dpblist	equ	$-sectpt	;size of area
;
;	local variables
tranv:	ds	word	;address of translate vector
fcb$copied:
	ds	byte	;set true if copy$fcb called
rmf:	ds	byte	;read mode flag for open$reel
dirloc:	ds	byte	;directory flag in rename, etc.
seqio:	ds	byte	;1 if sequential i/o
linfo:	ds	byte	;low(info)
dminx:	ds	byte	;local for diskwrite
searchl:ds	byte	;search length
searcha:ds	word	;search address
tinfo:	ds	word	;temp for info in "make"
single:	ds	byte	;set true if single byte allocation map
resel:	ds	byte	;reselection flag
olddsk:	ds	byte	;disk on entry to bdos
fcbdsk:	ds	byte	;disk named in fcb
rcount:	ds	byte	;record count in current fcb
extval:	ds	byte	;extent number and extmsk
vrecord:ds	word	;current virtual record
arecord:ds	word	;current actual record
arecord1:	ds	word	;current actual block# * blkmsk
;
;	local variables for directory access
dptr:	ds	byte	;directory pointer 0,1,2,3
dcnt:	ds	word	;directory counter 0,1,...,dirmax
drec:	ds	word	;directory record 0,1,...,dirmax/4
;
bios	equ	($ and 0ff00h)+100h	;next module
	end
