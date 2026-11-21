title	'PUT.RSX 3.0 - CP/M 3.0 Output Redirection - August 1982'
;******************************************************************
;
;	PUT  'Output Redirection Facility'  version 3.0
;
; 	11/30/82 - Doug Huskey
;	This RSX redirects console or list output to a file.
;******************************************************************
;
;
;	generation procedure
;
;	rmac putrsx
;	xref putrsx
;	link putrsx[op]
;	ERA put.RSX
;	REN put.RSX=putRSX.PRL
;	GENCOM put.com put.rsx
;
;	initialization procedure
;
;	PUTF makes a RSX function 60 call with a sub-function of
;	128.  PUTRSX returns the address of a data table containing:
;
;	init$table:
;		dw	kill		;remove PUT at warmboot flg
;		dw	0		;reserved
;		dw	bios$output	;BIOS entry point into PUT
;		dw	putfcb		;FCB address
;
;	PUTF initializes the data are between movstart: and movend:
;	and moves it into PUT.RSX.  This means that data should not
;	be reordered without also changing PUTF.ASM.
;
;		
true		equ	0ffffh
false		equ	00000h
;
bios$functions	equ	true	;intercept BIOS console functions
remove$rsx	equ	false	;this RSX does its own removal
;
;	low memory locations
;
wboot	equ	0000h
wboota	equ	wboot+1
bdos	equ	0005h
bdosl	equ	bdos+1
buf	equ	0080h
;
;	equates for non graphic characters
;
ctlc	equ	03h	; control c
ctle	equ	05h	; physical eol
ctlh	equ	08h	; backspace
ctlp	equ	10h	; prnt toggle
ctlr	equ	12h	; repeat line
ctls	equ	13h	; stop/start screen
ctlu	equ	15h	; line delete
ctlx	equ	18h	; =ctl-u
ctlz	equ	1ah	; end of file
rubout	equ	7fh	; char delete
tab	equ	09h	; tab char
cr	equ	0dh	; carriage return
lf	equ	0ah	; line feed
ctl	equ	5eh	; up arrow
;
;	BDOS function equates
;
cinf	equ	1	;read character
coutf	equ	2	;output character
crawf	equ	6	;raw console I/O
creadf	equ	10	;read buffer
cstatf	equ	11	;status
lchrf	equ	5	;print character
pbuff	equ	9	;print buffer
resetf	equ	13	;reset drive
openf	equ	15	;open file
closef	equ	16	;close file
delf	equ	19	;delete file
dreadf	equ	20	;disk read
writef	equ	21	;disk write
dmaf	equ	26	;set dma function
userf	equ	32	;set/PUT user number
resdvf	equ	37	;reset drive function
flushf	equ	48	;flush buffers function
scbf	equ	49	;set/PUT system control block word
loadf	equ	59	;Program load function
rsxf	equ	60	;RSX function call
resalvf	equ	98	;reset allocation vector
pblkf	equ	111	;print block to console
lblkf	equ	112	;print block to list device
ginitf	equ	128	;GET initialization sub-function no.
gkillf	equ	129	;GET delete sub-function no.
gfcbf	equ	130	;GET file display sub-function no.
pinitf	equ	132	;PUT initialization sub-function no.
pckillf	equ	133	;PUT console delete sub-function no.
plkillf	equ	137	;PUT list delete sub-function no.
pcfcbf	equ	134	;return PUT console fcb address
plfcbf	equ	138	;return PUT list fcb address
jinitf	equ	140	;JOURNAL initialization sub-function no.
jkillf	equ	141	;JOURNAL delete sub-function no.
jfcbf	equ	142	;return JOURNAL fcb address
;
;	System Control Block definitions
;
scba	equ	03ah	;offset of scbadr from SCB base
ccpflg	equ	0b3h	;offset of ccpflags word from page boundary
ccpres	equ	020h	;ccp resident flag = bit 5
bdosoff equ	0feh	;offset of BDOS address from page boundary
errflg	equ	0aah	;offset of error flag from page boundary
conmode	equ	0cfh	;offset of console mode word from pag. bound.
outdel	equ	0d3h	;offset of print buffer delimiter
listcp	equ	0d4h	;offset of ^P flag from page boundary
usrcode	equ	0e0h	;offset of user number from pg bnd.
dcnt	equ	0e1h	;offset of dcnt, searcha & searchl from pg bnd.
constfx	equ	06eh	;offset of constat JMP from page boundary
coninfx	equ	074h	;offset of conin JMP from page boundary
;
;
;******************************************************************
;		RSX HEADER 
;******************************************************************

serial:	db	0,0,0,0,0,0

trapjmp:
	jmp	trap		;trap read buff and DMA functions
next:	jmp	0		;go to BDOS
prev:	dw	bdos
kill:	db	0FFh		;Remove at wstart if not zero
nbank:	db	0
rname:	db	'PUT     '	;RSX name
space:	dw	0
patch:	db	0

;******************************************************************
;		START OF CODE
;******************************************************************
;
;	ABORT ROUTINE
;
puteof:				;close output file and abort
	lda	cbufp
	ora	a
	jz	restor
	mvi	e,ctlz
	call	putc
	jmp	puteof


;
;******************************************************************
;		BIOS TRAP ENTRY POINT
;******************************************************************
;
;
;	ARRIVE HERE ON EACH INTERCEPTED BIOS CALL
;
;
bios$output:
	;
if bios$functions
	;
	;enter here from BIOS constat
	mov	e,c		;character in E
	lda	bdosfunc	;BDOS function to use
	mov	c,a
	mvi	a,1		;offset in exit table = 1
	jmp	bios$trap
endif
;
;
;******************************************************************
;		BDOS TRAP ENTRY POINT 
;******************************************************************
;
;
;	ARRIVE HERE AT EACH BDOS CALL
;
trap:
	;
if bios$functions
	;
	xra 	a
biostrap:
	;enter here on BIOS calls
	sta	exit$off
endif
	pop	h		;return address
	push	h		;back to stack
	lda	trapjmp+2	;PUT.RSX page address
	cmp	h		;high byte of return address
	jc	exit		;skip calls on bdos above here
	mov	a,c
	cpi	rsxf
	jz	rsxfunc		;check for initialize or abort
	cpi	dmaf
	jz	dmafunc		;save users DMA address
	cpi	14		;reset function + 1
	jc	tbl$srch	;search if func < 14
	cpi	98
	jnc	tbl$srch	;search if func >= 98
	cpi	resdvf
	jz	tbl$srch	;search if func = 37
	;
	;	EXIT - FUNCTION NOT MATCHED
	;
exit:

if not bios$functions
	;
exit1:	jmp	next		;go to next RSX or BDOS

else
	lda	exit$off	;PUT type of call:
exit1:	lxi	h,exit$table	;0=BDOS call, 1=BIOS call
endif

tbl$jmp:

	;  a = offset (rel 0) 
	; hl = table address
	add	a		;double for 2 byte addresses
	call	addhla		;HL = .(exit routine)
	mov	b,m		;get low byte from table
	inx	h
	mov	h,m
	mov	l,b		;HL = exit routine
	pchl			;gone to BDOS or BIOS

tbl$srch:

	;
	;CHECK IF THIS FUNCTION IS IN FUNCTION TABLE
	;if matched b = offset in table (rel 0)
	;FF terminates table
	;FE is used to mark non-intercepted functions
	;
	lxi	h,func$tbl	;list of intercepted functions
	mvi	b,0		;start at beginning
tbl$srch1:
	mov	a,m		;get next table entry
	cmp	c		;is it the same?
	jz	intercept	;we found a match, B = offset
	inr	b
	inx	h
	inr	a		;0FFh terminates list 
	jnz	tbl$srch1	;try next one
	jmp	exit		;end of table - not found

;
;
;******************************************************************
;		REDIRECTION PROCESSOR
;******************************************************************
;
;
;	INTERCEPTED BDOS FUNCTIONS ARRIVE HERE 
;
;	enter with 
;			 B = routine offset in table
;			 C = function number
;			DE = BDOS parameters

intercept:

	;switch to local stack
	lxi	h,0
	dad	sp
	shld	oldstack
	lxi	sp,stack

redirect:

	push	d		;save info
	push	b		;save function
	lhld	scbadr
	;
	;are we active now?
	;
	lda	program
	ora	a		;program output only?
	cnz	ckccp		;if not, test if CCP is calling
	jz	cklist		;jump if not CCP or program output
	mov	a,c
	cpi	0ah		;is it function 10?
	jnz	skip		;skip if not
	lxi	h,ccpcnt	;decrement once for each
	dcr	m		;CCP function 10
	cm	puteof		;if 2nd appearance of CCP
	jmp	skip		;if CCP is active
	;
	;check for list processing and ^P status
	;
cklist:
	lda	list
	ora	a		;list redirection?
	jz	ckecho		;jump if not
	mvi	l,listcp	;HL = .^P flag
	mov	a,m
	ora	a		; ^P on?
	jnz	setecho		;set echo on if so
	mov	a,b
	cpi	2		;console function?
	jnc	skip		;skip if so
ckecho:	lda	echoflg		;echo parameter
setecho:
	sta	echo
	;
	;go to function trap routine
	;
gofunct:
	lxi	h,retmon	;program return routine
	push	h		;push on stack 
	mov	a,b		;offset
	lxi	h,trap$tbl
	jmp	tbl$jmp		;go to table address
;
;
rawio:
	;direct console i/o - read if 0ffh
	;returns to retmon
	mov 	a,e	
	cpi	0fdh
	jc	putchr
	cpi	0feh
	rz			;make the status call  (FE)
	jc	conin		;make the input call   (FD)
	call	next		;call for input/status (FF)
	ora	a
	jz	retmon1
	jmp	conin1
	;
	;input function
	;
conin:
	call	exit		;make the call
conin1:	mov	e,a		;put character in E
	push	psw		;save character
	call	conout		;put character into file
	pop	psw		;character in A
	;
	;	RETURN FROM FUNCTION TRAP ROUTINE
	;
	cpi	cr
	jnz	retmon1

retmon2:
	;output linefeed before returning
	push	psw		;save character
	lda	echo
	ora	a		;no echo mode
	mvi	e,lf
	mvi	c,coutf
	cz	next		;output lf if so
	lda	input
	ora	a
	cnz	conout
	pop	psw		;restore character

retmon1:
	;return to calling program
	lhld	old$stack
	sphl
	mov	l,a
retmon0:
	ret			;to calling program
	;
retmon:
	;echo before returning?
	lda	echo
	ora	a
	jz	retmon1		;return to program if no echo
	;otherwise continue 
	;
	;	PERFORM INTERCEPTED BDOS CALL
	;
skip:
	;restore BDOS call and stack
	pop	b		;restore BDOS function no.
	pop	d		;restore BDOS parameter
	lhld	old$stack
	sphl
	jmp	exit		;goto BDOS

;******************************************************************
;		BIOS FUNCTIONS (REDIRECTION ROUTINES)
;******************************************************************
;
putchr:
	;put out character in E unless putting input
	lda input! ora a! rnz  	;return (retmon) if input redirection
listf:
conout:
conoutf:	
ctlout:
	;send E character with possible preceding up-arrow
	mov a,e! cpi ctlz! jz ctlout1 	;always convert ^Z
	call echoc 	;cy if not graphic (or special case)
	jnc putc 	;skip if graphic, tab, cr, lf, or ctlh

	ctlout1:
		;send preceding up arrow
		push psw! mvi e,ctl! call putc ;up arrow
		pop psw! ori 40h ;becomes graphic letter
		mov e,a ;ready to print
		;(drop through to PUTC)
;
;
;	put next character into file
;
;
putc:	;write sector if full, close in each physical block
	;abort PUT if any disk error occurs
	;character in E
	lxi	h,cbufp
	mov	a,m		; A = cbufp
	push 	h
	inx	h		;HL = .cbuf
	call	addhla		;HL = .char
	mov	m,e		;store character
	pop	h
	inr	m		;next chr position
	rp			;minus flag set after 128 chars
;
;	WRITE NEXT RECORD
;
write:
	mvi	c,writef
	call	putdos
	cnz	restor		;abort RSX if error
	xra	a
	sta	cbufp		;reset buffer position to 0
	lxi	h,record
	dcr	m		;did we cross the block boundary?
	rp			;return if not
	call	close		;close the file if so
	cnz	restor		;abort RSX if error
	lxi	h,blm		;HL = .blm
	mov	a,m		
	dcx	h
	mov	m,a		;set record = blm
	ret
;
;	CLOSE THE FILE
;
close:
	mvi 	c,closef
;
;	PUT FILE OPERATION
;
putdos:	
	push	b		;function no. in C
	lxi	d,cbuf
	call	setdma		;set DMA to our buffer
	pop	b		;function no. in C
	lhld	scbadr
	push	h		;save for restore
	lxi	d,sav$area	;10 byte save area
	push	d		;save for restore
	call	mov7		;save hash info in save area
	mvi	l,usrcode	;HL = .BDOS user number in SCB
	call	mov7		;save user, dcnt, search addr, len &
	dcx	h		; multi-sector count
	mvi	m,1		;set multi-sector count=1
	mvi	l,usrcode	;HL = .BDOS user number
	lxi	d,putusr
	ldax	d
	mov	m,a		;set BDOS user = putusr
	inx	d		;DE = .putfcb
	call	next		;write next record or close file
	pop	h		;HL = .sav$area
	pop	d		;DE = .scb
	push	psw		;save A (non-zero if error)
	call	mov7		;restore hash info
	mvi	e,usrcode	;DE = .user num in scb
	call	mov7		;restore dcnt search addr & len
	lhld	udma
	xchg
	call	setdma		;restore DMA to program's buffer
	pop	psw
	ora	a
	ret			;zero flag set if successful
;
;	CLOSE FILE AND TERMINATE RSX
;
restor:
	call	close
	lxi	d,close$err
	cnz	msg		;print message if close error
	lxi	h,0ffffh
	shld	rsxfunctions	;set killf and fcbf to inactive
	;
	;set RSX aborted flag
	;
	lxi	h,kill		;0=active, 0ffh=aborted
	mvi	m,0ffh		;set to 0ffh (in-active)
	;are we the bottom RSX, if so remove ourselves immediately
	;to save memory
	lda	bdosl+1		;get high byte of top of tpa
	CMP	H		;Does location 6 point to us

if remove$rsx
	jnz	bios$fixup	;done, if not
	lhld	next+1
	shld	bdosl
	xchg
	lhld	scbadr
	mvi	l,bdosoff	;HL = "BDOS" address in SCB
	mov	m,e		;put next address into SCB
	inx	h
	mov	m,d
	xchg
	mvi	l,0ch		;HL = .previous RSX field in next RSX
	mvi	m,7
	inx	h
	mvi	m,0		;put previous into previous
else
	mvi	c,loadf
	lxi	d,0
	cz	next		;fixup RSX chain, if this RSX on bottom
endif

if bios$functions

bios$fixup:
	;
	;restore bios jumps
	lda	restore$mode		;may be FF, 7f or 0
	inr	a	
	rz				; FF = no bios interception
	lhld	wmsta			;real warm start routine
	xchg
	lhld	wmjmp			;wboot jump in bios
	mov	m,e
	inx	h
	mov	m,d			;restore real routine in jump
	lhld	biosout			;conin,conout or list jmp
	xchg
	lhld	biosjmp			;address of real bios routine
	mov	m,e
	inx	h
	mov	m,d
	rm				; 7f = RESBDOS jmps not changed
	lhld	wmfix
	mvi	m,jmp			;replace jmp for warm start
	lhld	biosfix
	mvi	m,jmp			;replace jmp for other trapped jump
endif
	ret				; 0  = everything done
;
;	set DMA address in DE
;
setdma:	mvi	c,dmaf
	jmp	next
;
;	print message to console
;
msg:	mvi	c,pbuff
	jmp	next
;
;	move routine
;
mov7:	mvi	b,7
	;			HL = source
	;			DE = destination
	;	 		B = count
move:	mov	a,m
	stax	d
	inx	h
	inx	d
	dcr	b
	jnz	move
	ret
;
;	add a to hl
;
addhla:	add	l
	mov	l,a
	rnc
	inr	h
	ret

;
;	check if CCP is calling
;
ckccp:
	;returns zero flag set if not CCP
	lhld	scbadr
	mvi	l,ccpflg+1	;HL = .ccp flag 2
	mov	a,m
	ani	ccpres		;is it the CCP?
	ret
;
;******************************************************************
;		BDOS FUNCTION HANDLERS
;******************************************************************
;
;
;	FUNCTION 26 - SET DMA ADDRESS
;
dmafunc:
	xchg			;dma to hl
	shld	udma		;save it
	xchg
	jmp	next
;
;
;	BIOS WARM START TRAP FUNCTION
;
warmtrap:
	lxi	sp,stack
	call	close		;close if wboot originated below RSX
	jmp	wstart
;
;	BDOS FUNCTION 60 - RSX FUNCTION CALL	
;
rsxfunc:			;check for initialize or delete RSX functions
	ldax	d		;get sub-function number
	cpi	pinitf		;is it a PUT initialization
	lxi	h,init$table
	rz			;return to caller if init call
	;check for FCB display functions
	mov	b,a
	lda	fcbf		;is it a a PUT fcb request
	cmp	b
	lxi	h,putfcb
	rz			;return if so
	;check for kill function
	lda	killf		;local kill (kill only this one)
	cmp	b
	jz	puteof		;kill and return to caller
	jmp	exit		;abort any higher PUTs

;
;
;******************************************************************
;		BDOS OUTPUT ROUTINES
;******************************************************************
;
;
;       July 1982
;
;
;	Console handlers
;
echoc:
	;are we in cooked or raw mode?
	lda cooked! ora a! mov a,e! rz ;return if raw
	;echo character if graphic
	;cr, lf, tab, or backspace
	cpi cr! rz ;carriage return?
	cpi lf! rz ;line feed?
	cpi tab! rz ;tab?
	cpi ctlh! rz ;backspace?
	cpi ' '! ret ;carry set if not graphic
;
;
print:
	;print message until M(DE) = '$'
	lhld scbadr
	mvi l,OUTDEL
	ldax d! CMP M! rz ;stop on delimiter
		;more to print
		inx d! push d! mov e,a ;char to E
		call conout ;another character printed
		pop d! jmp print
;
;
read:	
	;put prompt if in no echo mode
	lda echo! ora a! jnz read1
	push d			
	lxi d,prompt! call msg		;output prompt
	pop d! mvi c,creadf		;set for read call
read1:
	;read console buffer
	pop h				;throw away return address
        push d
	call next			;make the call
	pop h! inx h! mov b,m! inr b	;get the buffer length
putnxt:		dcr b! jz read2
		inx h! mov e,m! push b! push h
		call conout! pop h! pop b	;put character
		jmp putnxt

read2:	lda input! ora a! push psw
	mvi e,cr! cnz conout			;call if putting input
	pop psw! mvi e,lf! cnz conout		;call if putting input
	jmp retmon1


;
func1:	equ	conin
;
func2:	equ	conout
	;write console character 
;
func5:	equ	listf
	;write list character
	;write to list device
;
func6:	equ	rawio
;
func9:	equ	print
 	;write line until $ encountered
;
func10:	equ	read
;
func11:	equ	retmon0
;
func13:	equ	close
;
func37:	equ	close
;
func98:	equ	close
;
FUNC111:			;PRINT BLOCK TO CONSOLE
FUNC112:			;LIST BLOCK
	XCHG! MOV E,M! INX H! MOV D,M! INX H
	MOV C,M! INX H! MOV B,M! XCHG
	;HL = ADDR OF STRING
	;BC = LENGTH OF STRING
BLK$OUT:
	MOV A,B! ORA C! RZ	;is length 0, return if so
	PUSH B! PUSH H
	mov e,m! call conout	;put character
	POP H! INX H! POP B! DCX B
	JMP BLK$OUT

;	end of BDOS Console module

;******************************************************************
;		DATA AREA
;******************************************************************

exit$off	db	0	;offset in exit$table of destination

trap$tbl:
	;function dispatch table (must match func$tbl below)
;	db	lchrf, lblkf, coutf, cstatf, crawf
;	db	pbuff, cinf, creadf, resetf, resdvf
;	db	resalvf, pblkf, eot

	dw	func5		;function 5   - list output
	dw	func112		;function 112 - list block
	dw	func2		;function 2   - console output
	dw	func11		;function 11  - console status
	dw	func6		;function 6   - raw console I/O
	dw	func9		;function 9   - print string
	dw	func1		;function 1   - console input
	dw	func10		;function 10  - read console buffer
	dw	func13		;function 13  - disk reset (close first)
	dw	func37		;function 37  - drive reset (close first)
	dw	func98		;function 98  - reset allocation vector
	dw	func111		;function 111 - print block

;******************************************************************
;	Following variables and entry points are used by PUT.COM
;	Their order and contents must not be changed without also
;	changing PUT.COM.
;******************************************************************

movstart:
init$table:			;addresses used by PUT.COM for initial.
scbadr:				;address of System Control Block
	dw	kill		;kill flag for error on file make
				;(passed to PUT.COM by RSX init function)
	;
	if bios$functions	;PUT.RSX initialization
	;
gobios:	mov	c,e
	db	jmp
biosout	dw	bios$output	;set to real BIOS routine
				;(passed to PUT.COM by RSXFUNC)
biosjmp
	dw	warm$trap	;address of bios jmp initialized by COM
biosfix
	dw	0		;address of jmp in resbdos to restore
				;restore only if changed when removed.
wstart:	db	jmp
wmsta:	dw	0		;address of real warm start routine
wmjmp:	dw	0		;address of jmp in bios to restore
wmfix:	dw	0		;address of jmp in resbdos to restore
bdosfunc:
	db	coutf
restore$mode
	db	0		;0FFh = no bios restore, 07fh = restore
				;only bios jmp, 0 = restore bios jump and
				;resbdos jmp when removed.
	endif
;
;	equates function table
;
eot	equ	0ffh	; end of function table
skipf	equ	0feh	; skip this function
;
;
func$tbl:		;no trapping until initialized by PUT.COM
	db	eot,0,0,0,0,0,0,0,0,0,0,0,0
;	db	lchrf, lblkf, coutf, cstatf, crawf
;	db	pbuff, cinf, creadf, resetf, resdvf
;	db	resalvf, pblkf, eot
	;
input	db	0		;put console input to a file
list	db	0		;intercept list functions
echoflg:
	db	1		;echo output to device
cooked:				;must be next after echo
	db	0		;TRUE if ctrl chars (except ^Z) placed 
				;in the output file
rsxfunctions:
killf:	db	0ffh		;not used until PUT initialized
fcbf:	db	0ffh		;not used until PUT initialized
record:	db	0		;counts down records to block boundary
blm:	db	0		;block mask = records per block (rel 0)
program:			;this flag must be @ .PUTFCB-2
	db	0		;true if put program output only
putusr:	db	0		;user number for redirection file
putfcb:	db	0ffh		;preset to 0ffh to indicate not active
	db	'SYSOUT  '
	db	'$$$'
	db	0,0
putmod:	db	0
putrc:	ds	1
	ds	16		;map
putcr:	ds	1
	;
cbufp	db	0		;current character position in cbuf
movend:
;*******************************************************************

cbuf:				;128 byte buffer (could be ds 128)

	db	3,3,3,3, 3,3,3,3, 3,3,3,3, 3,3,3,3
	db	3,3,3,3, 3,3,3,3, 3,3,3,3, 3,3,3,3
	db	3,3,3,3, 3,3,3,3, 3,3,3,3, 3,3,3,3
	db	3,3,3,3, 3,3,3,3, 3,3,3,3, 3,3,3,3

	db	3,3,3,3, 3,3,3,3, 3,3,3,3, 3,3,3,3
	db	3,3,3,3, 3,3,3,3, 3,3,3,3, 3,3,3,3
	db	3,3,3,3, 3,3,3,3, 3,3,3,3, 3,3,3,3
	db	3,3,3,3, 3,3,3,3, 3,3,3,3, 3,3,3,3

	;
	if bios$functions
	;
exit$table:			;addresses to go to on exit
	dw	next		;BDOS
	dw	gobios
	endif
	;
udma:	dw	buf		;user dma
user:	db	0		;user user number
echo:	db	0		;echo output to console flag
ccpcnt:	db	1		;start at 1 (decremented each CCP)
sav$area:			;14 byte save area
	db	68h,68h,68h,68h,68h, 68h,68h,68h,68h,68h
	db	68h,68h,68h,68h
close$err:	
	db	cr,lf,'PUT ERROR: FILE ERASED',cr,lf,'$'
prompt:	db	cr,lf,'PUT>$'
	;
patch$area:
	ds	30h
	db	' 151282 '
	db	' COPYR ''82 DRI '

	db	67h,67h,67h,67h, 67h,67h,67h,67h, 67h,67h,67h,67h
	db	67h,67h,67h,67h, 67h,67h,67h,67h, 67h,67h,67h,67h
	db	67h,67h,67h,67h, 67h,67h,67h,67h
	;
stack:				;16 level stack
oldstack:
	dw	0
	end
