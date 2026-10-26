title	'GET.RSX 3.0 - CP/M 3.0 Input Redirection - August 1982'
;******************************************************************
;
;	get  'Input Redirection Facility'  version 3.0
;
; 	11/30/82 - Doug Huskey
;	This RSX redirects console input and status from a file.
;******************************************************************
;
;
true		equ	0ffffh
false		equ	00000h
;
submit		equ	false	;true if submit RSX
remove$rsx	equ	false	;true if RSX removes itself
;				;false if LOADER does removes
;
;
;	generation procedure
;
;	rmac getrsx
;	xref getrsx
;	link getrsx[op]
;	ERA get.RSX
;	REN get.RSX=getRSX.PRL
;	GENCOM $1.COM get.RSX 		($1 is either SUBMIT or GET)
;
;
;	initialization procedure
;
;	GETF makes a RSX function 60 call with a sub-function of
;	128.  GETRSX returns the address of a data table containing:
;
;	init$table:	
;		dw	kill		;RSX remove flag addr in GET
;		dw	bios$constat	;bios entry point in GET
;		dw	bios$conin	;bios entry point in GET
;
;	GETF initializes the data are between movstart: and movend:
;	and moves it into GET.RSX.  This means that data should not
;	be reordered without also changing GETF.ASM.
;
bios$functions	equ	true	;intercept BIOS console functions
;
;	low memory locations
;
wboot	equ	0000h
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
	if submit
ctlz	equ	0ffh
	else
ctlz	equ	1ah	; end of file
	endif
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
pchrf	equ	5	;print character
pbuff	equ	9	;print buffer
openf	equ	15	;open file
closef	equ	16	;close file
delf	equ	19	;delete file
dreadf	equ	20	;disk read
dmaf	equ	26	;set dma function
userf	equ	32	;set/get user number
scbf	equ	49	;set/get system control block word
loadf	equ	59	;loader function call
rsxf	equ	60	;RSX function call
ginitf	equ	128	;GET initialization sub-function no.
gkillf	equ	129	;GET delete sub-function no.
gfcbf	equ	130	;GET file display sub-function no.
pinitf	equ	132	;PUT initialization sub-funct no.
pckillf	equ	133	;PUT CON: delete sub-function no.
pcfcbf	equ	134	;return PUT CON: fcb address
plkillf	equ	137	;PUT LST: delete sub-function no.
plfcbf	equ	138	;return PUT LST:fcb address
gsigf	equ	140	;signal GET without [SYSTEM] option
jinitf	equ	141	;JOURNAL initialization sub-funct no.
jkillf	equ	142	;JOURNAL delete sub-function no.
jfcbf	equ	143	;return JOURNAL fcb address
;
;	System Control Block definitions
;
scba	equ	03ah	;offset of scbadr from SCB base
ccpflg	equ	0b3h	;offset of ccpflags word from page boundary
ccpres	equ	020h	;ccp resident flag = bit 5
bdosoff equ	0feh	;offset of BDOS address from page boundary
errflg	equ	0ach	;offset of error flag from page boundary
pg$mode	equ	0c8h	;offset of page mode byte from pag. bound.
pg$def	equ	0c9h	;offset of page mode default from pag. bound.
conmode	equ	0cfh	;offset of console mode word from pag. bound.
listcp	equ	0d4h	;offset of ^P flag from page boundary
dmaad	equ	0d8h	;offset of DMA address from pg bnd.
usrcode	equ	0e0h	;offset of user number from pg bnd.
dcnt	equ	0e1h	;offset of dcnt, searcha & searchl from pg bnd.
constfx	equ	06eh	;offset of constat JMP from page boundary
coninfx	equ	074h	;offset of conin JMP from page boundary


;******************************************************************
;		RSX HEADER 
;******************************************************************

serial:	db	0,0,0,0,0,0

trapjmp:
	jmp	trap		;trap read buff and DMA functions
next:	jmp	0		;go to BDOS
prev:	dw	bdos
kill:	db	0FFh		;0FFh => remove RSX at wstart
nbank:	db	0
rname:	db	'GET     '	;RSX name
space:	dw	0
patch:	db	0

;******************************************************************
;		START OF CODE
;******************************************************************

;
;	ABORT ROUTINE
;
getout:
	;
if bios$functions
	;
	;restore bios jumps
	lda	restore$mode		;may be FF, 7f, 80 or 0
	inr	a	
	rz				; FF = no bios interception
	lhld	biosin
	xchg
	lhld	biosta
	call	restore$bios		;restore BIOS constat & conin jmps
	rm				; 7f = RESBDOS jmps not changed
	lhld	scbadr
	mvi	l,constfx
	mvi	m,jmp
	rpe				; 80 = conin jmp not changed
	mvi	l,coninfx
	mvi	m,jmp
endif
	ret				; 0  = everything done
;
;	ARRIVE HERE ON EACH BIOS CONIN OR CONSTAT CALL
;
;
bios$constat:
	;
if bios$functions
	;
	;enter here from BIOS constat
	lxi	b,4*256+cstatf	;b=offset in exit table
	jmp	bios$trap
endif
;
bios$conin:
	;
if bios$functions
	;
	;enter here from BIOS conin
	lxi	b,6*256+crawf	;b=offset in exit table
	mvi	e,0fdh
	jmp	biostrap
endif
;
;	ARRIVE HERE AT EACH BDOS CALL
;
trap:
	;
	;
	lxi	h,excess
	mvi	b,0
	mov	m,b
biostrap:
	;enter here on BIOS calls

	pop	h		;return address
	push	h		;back to stack
	lda	trapjmp+2	;GET.RSX page address
	cmp	h		;high byte of return address
	jc	exit		;skip calls on bdos above here
	mov	a,c		;function number
	;
	;
	cpi	cstatf		;status	
	jz	intercept
	cpi	crawf
	jz	intercept	;raw I/O
	lxi	h,statflg	;zero conditional status flag
	mvi	m,0
	cpi	cinf
	jz	intercept	;read character
	cpi	creadf
	jz	intercept	;read buffer
	cpi	rsxf
	jz	rsxfunc		;rsx function
	cpi	dmaf
	jnz	exit		;skip if not setting DMA
	xchg
	shld	udma		;save user's DMA address
	xchg
;
exit:
	;go to real BDOS

if not bios$functions
	;
	jmp	next		;go to next RSX or BDOS

else
	mov	a,b		;get type of call:
	lxi	h,exit$table	;0=BDOS call, 4=BIOS CONIN, 6=BIOS CONSTAT
	call	addhla
	mov	b,m		;low byte to b
	inx	h
	mov	h,m		;high byte to h
	mov	l,b		;HL = .exit routine
	pchl			;gone to BDOS or BIOS
endif
;	
;
rsxfunc:			;check for initialize or delete RSX functions
	ldax	d		;get RSX sub-function number
	lxi	h,init$table	;address of area initialized by COM file
	cpi	ginitf
	rz
	lda	kill
	ora	a
	jnz	exit
	ldax	d
	cpi	gfcbf	
	lxi	h,subfcb
	rz
cksig:
	cpi	gsigf
	jnz	ckkill
	lxi	h,get$active
	mvi	a,gkillf
	sub	m		;toggle get$active flag
	mov	m,a		;gkillf->0    0->gkillf

ckkill:
	cpi	gkillf		;remove this instance of GET?
	jnz	exit		;jump if not
	

restor:
	lda	get$active
	ora	a
	rz
	call	getout		;bios jump fixup

if submit
	mvi	c,closef
	call	subdos
	mvi	c,delf
	call	subdos		;delete SYSIN??.$$$ if not
endif
	lxi	h,kill
	dcr	m		;set to 0ffh, so we are removed 
	xchg			; D = base of this RSX
	lhld	scbadr
	mvi	l,ccpflg+1	;hl = .ccp flag 2 in SCB
	mov	a,m
	ani	0bfh
	mov	m,a		;turn off redirection flag
	;we must remove this RSX if it is the lowest one
	lda	bdosl+1		;location 6 high byte
	cmp	d		;Does location 6 point to us
	RNZ			;return if not
if remove$rsx
	xchg			;D = scb page
	lhld	next+1
	shld	bdosl
	xchg			;H = scb page
	mvi	l,bdosoff	;HL = "BDOS" address in SCB
	mov	m,e		;put next address into SCB
	inx	h
	mov	m,d
	xchg
	mvi	l,0ch		;HL = .previous RSX field in next RSX
	mvi	m,7
	inx	h
	mvi	m,0		;put previous into previous
	ret
else
	;	CP/M 3 loader does RSX removal if DE=0
	mvi	c,loadf
	lxi	d,0
	jmp	next		;ask loader to remove me
endif

;
;
;	INTERCEPT EACH BDOS CONSOLE INPUT FUNCTION CALL HERE
;
;	enter with funct in A, info in DE
;
intercept:
;
	lda	kill
	ora	a
	jnz	exit		;skip if remove flag turned on
	;
	;switch stacks
	lxi	h,0
	dad	sp
	shld	old$stack	
	lxi	sp,stack
	push	b		;save function #
	push	d		;save info
	;check redirection mode
	call	getmode		;returns with H=SCB page
	cpi	2
	jz	skip		;skip if no redirection flag on
	
if submit	
;
;	SUBMIT PROCESSOR
;
	;check if CCP is calling
ckccp:	mvi	l,pg$mode
	mov	m,H		;set to non-zero for no paging
	mvi	l,ccpflg+1	;CCP FLAG 2 in SCB
	mov	a,m		;ccp flag byte 2 to A
	ori	040h
	mov	m,a		;set redirection flag on
	ani	ccpres		;zero flag set if not CCP calling
	lda	ccp$line
	jz	not$ccp
	;yes, CCP is calling
	ora	a
	jnz	redirect	;we have a CCP line
	;CCP & not a CCP line
	push	h
	call 	coninf		;throw away until next CCP line
	lxi	h,excess
	mov	a,m
	ora	a		;is this the first time?
	mvi	m,true
	lxi	d,garbage
	mvi	c,pbuff
	cz	next		;print the warning if so
	pop	h
	lda	kill
	ora	a
	jz	ckccp		;get next character (unless eof)
	mov	a,m
	ani	7fh		;turn off disk reset (CCP) flag
	mov	m,a
	jmp	wboot		;skip if remove flag turned on
;
not$ccp:
	;no, its not the CCP
	ora	a
	jnz	skip		;skip if no program line

else
	lda	program
	ora	a		;program input only?
	mvi	l,ccpflg+1	;CCP FLAG 2 in SCB
	mov	a,m		;ccp flag byte 2 to A
	jz	set$no$page	;jump if [system] option
	;check if CCP is calling
	ani	ccpres		;zero flag set if not CCP calling
	jz	redirect	;jump if not the CCP
	lxi	h,ccpcnt	;decrement once for each
	dcr	m		;time CCP active
	cm	restor		;if 2nd CCP appearance
	lxi	d,cksig+1
	mvi	c,rsxf		;terminate any GETs waiting for
	call	next		;us to finish
	jmp	skip
	;
set$no$page:
	ori	40h		;A=ccpflag2, HL=.ccpflag2
	mov	m,a		;set redirection flag on
	mvi	l,pg$mode
	mov	m,h		;set to non-zero for no paging
endif
	;
	;	REDIRECTION PROCESSOR
	;
redirect:
	;break if control-C typed on console
	call	break
	pop	d
	pop	b		;recover function no. & info
	push	b		;save function
	push	d		;save info
	mov	a,c		;function no. to A
	lxi	h,retmon	;program return routine
	push	h		;push on stack 
	;
	;
	cpi	creadf
	jz	func10		;read buffer (returns to retmon)
	cpi	cinf
	jz	func1		;read character (returns to retmon)
	cpi	cstatf
	jz	func11		;status	(returns to retmon)
;
func6:
	;direct console i/o - read if 0ffh
	;returns to retmon
	mov 	a,e	
	inr 	a
	jz 	dirinp 		;0ffh in E for status/input
	inr 	a
	jz 	CONBRK		;0feh in E for status
	lxi	h,statflg
	mvi	m,0
	inr 	a		
	jz	coninf		;0fdh in E for input
	;
	;direct output function
	;
	jmp	skip1
	;
break:	;
	;quit if ^C typed
	mvi	c,cstatf
	call	real$bdos
	ora	a		;was ^C typed?
	rz
	pop	h		;throw away return address
	call	restor		;remove this RSX, if so
	mvi	c,crawf
	mvi	e,0ffh
	call	next		;eat ^C if not nested
	;
skip:	;
	;reset ^C status mode
	call	getmode		;returns .conmode+1
	dcx	h		;hl = .conmode in SCB
	mov	a,m
	ani	0feh		;turn off control C status
	mov	m,a
	;restore the BDOS call 
	pop	d		;restore BDOS function no.
	pop	b		;restore BDOS parameter
	;restore the user's stack
skip1:	lhld	old$stack
	sphl
	jmp	exit		;goto BDOS

;
retmon:
	;normal entry point, char in A
	cpi	ctlz
	jz	skip
	lhld	old$stack
	sphl
	mov	l,a
	ret			;to calling program


;******************************************************************
;		BIOS FUNCTIONS (REDIRECTION ROUTINES)
;******************************************************************
;
;	;direct console input
dirinp:
	call	conbrk
	ora	a
	rz
;
;
;	get next character from file
;
	;
coninf:	
getc:	;return ^Z if end of file
	xra	a
	lxi	h,cbufp		;cbuf index
	inr	m		;next chr position
	cm	readf		;read a new record
	ora	a		
	mvi	b,ctlz		;EOF indicator
	jnz	getc1		;jump if end of file
	lda	cbufp
	lxi	h,cbuf
	call	addhla		;HL = .char
	;one character look ahead
	;new char in B, current char in nextchr
	mov	b,m		;new character in B
getc1:	mov	a,b
	cpi	ctlz
	push	b
	cz	restor
	pop	b
	lxi	h,nextchr
	mov	a,m		;current character
	cpi	cr
	mov	m,b		;save next character
	rnz
	mov	a,b		;A=character after CR
	cpi	lf		;is it a line feed
	cz	getc		;eat line feeds after a CR
				;this must return from above
				;rnz because nextchr = lf
	;
if submit
	;
	mov	a,b		;get nextchr
	sui	'<'		;program line?
	sta	ccp$line	;zero if so
	cz	getc		;eat '<' char
				;this must return from above
				;rnz because nextchr = <
endif
	mvi	a,cr		;get back the cr
	ret			;with character in a
;
;	set DMA address in DE
;
setdma:	mvi	c,dmaf
	jmp	next
;
;	read next record
;
readf:	mvi	c,dreadf	;read next record of input to cbuf
subdos:	push	b
	lxi	d,cbuf
	call	setdma		;set DMA to our buffer
	lhld	scbadr
	lxi	d,sav$area	;10 byte save area
	pop	b		;C  = function no.
	push	h		;save for restore
	push	d		;save for restore
	call	mov7		;save hash info in save area
	mvi	l,usrcode	;HL = .dcnt in SCB
	call	mov7		;save dcnt, searcha & l, user# &
	dcx	h		;multi-sector I/O count
	mvi	m,1		;set multi-sector count = 1
	lxi	d,subusr	;DE = .submit user #
	mvi	l,usrcode	;HL = .BDOS user number
	ldax	d
	mov	m,a
	inx	d
	call	next		;read next record
	pop	h		;HL = .sav$area
	pop	d		;DE = .scb
	push	psw		;save A (non-zero if error)
	call	mov7		;restore hash info
	mvi	e,usrcode	;DE = .dcnt in scb
	call	mov7		;restore dcnt search addr & len
	lhld	udma
	xchg
	call	setdma		;restore DMA to program's buffer
	xra	a
	sta	cbufp		;reset buffer position to 0
	pop	psw
	ora	a
	ret			;zero flag set, if successful
;
;	reboot from ^C
;
rebootx:
	;store 0fffeh in clp$errcode in SCB
	lhld	scbadr
	mvi	l,errflg
	mvi	m,0feh
	inx	h
	mvi	m,0ffh
	jmp	wboot
;
;
;	get input redirection mode to A 
;	turn on ^C status mode for break
;	return .conmode+1 in HL
;	preserve registers BC and DE
;
getmode:
	lhld	scbadr
	mvi	l,conmode
	mov	a,m
	ori	1		;turn on ^C status
	mov	m,a
	inx	h
	mov	a,m
	ani	3		;mask off redirection bits
	dcr	a		;255=false, 0=conditional, 1=true,
	ret			;  2=don't redirect input
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
;******************************************************************
;		BDOS CONSOLE INPUT ROUTINES
;******************************************************************

;
;       February 3, 1981
;
;
;	console handlers

conin:	equ	coninf
;
conech:
	;read character with echo
	call conin! call echoc! rc 	;echo character?
        ;character must be echoed before return
	push psw! call conout! pop psw
	ret 				;with character in A
;
echoc:
	;are we in cooked or raw mode?
	lxi h,cooked! dcr m! inr m! rz	;return if raw
	;echo character if graphic
	;cr, lf, tab, or backspace
	cpi cr! rz 		;carriage return?
	cpi lf! rz 		;line feed?
	cpi tab! rz 		;tab?
	cpi ctlh! rz 		;backspace?
	cpi ' '! ret 		;carry set if not graphic
;
conbrk:	;STATUS - check for character ready
	lxi h,statflg
	mov b,m! mvi m,0ffh	;set conditional status flag true
	call getmode		;check input redirection status mode  
	cpi 1! rz		;actual status mode => return true
	ora a! rz		;false status mode  => return false
	;conditional status mode => false unless prev func was status
	mov a,b! ret		; return false if statflg false
				; return true if statflg true
;
;
ctlout:
	;send character in A with possible preceding up-arrow
	call echoc 		;cy if not graphic (or special case)
	jnc conout 		;skip if graphic, tab, cr, lf, or ctlh
		;send preceding up arrow
		push psw! mvi a,ctl! call conout ;up arrow
		pop psw! ori 40h 	;becomes graphic letter
		;(drop through to conout)
;
;
;	send character in A to console
;
conout:
	mov	e,a
	lda	echo
	ora	a
	rz
	mvi	c,coutf
	jmp	next
;
;
read:	;read to buffer address (max length, current length, buffer)
	xchg					;buffer address to HL
	mov c,m! inx h! push h! mvi b,0		;save .(current length)
	;B = current buffer length,
	;C = maximum buffer length,
	;HL= next to fill - 1
	readnx:
		;read next character, BC, HL active
		push b! push h 				;blen, cmax, HL saved
		readn0:
			call conin 			;next char in A
			pop h! pop b 			;reactivate counters
			cpi ctlz! jnz noteof  		;end of file?
			dcr b! inr b! jz readen		;skip if buffer empty
			mvi a,cr			;otherwise return
		noteof:
			cpi cr!   jz readen		;end of line?
			cpi lf!   jz readen		;also end of line
			cpi ctlp! jnz notp 		;skip if not ctlp
			;list toggle - change parity
			push h!	push b			;save counters
			lhld scbadr! mvi l,listcp	;hl =.listcp 
			mvi a,1! sub m			;True-listcp
			mov m,a 			;listcp = not listcp
			pop b! pop h! jmp readnx 	;for another char
		notp:
			;not a ctlp
			;place into buffer
		rdecho:
			inx h! mov m,a 		;character filled to mem
			inr b 			;blen = blen + 1
		rdech1:
			;look for a random control character
			push b! push h 		;active values saved
			call ctlout 		;may be up-arrow C
			pop h! pop b! mov a,m 	;recall char
			cpi ctlc 		;set flags for reboot test
			mov a,b 		;move length to A
			jnz notc 		;skip if not a control c
			cpi 1 			;control C, must be length 1
			jz rebootx 		;reboot if blen = 1
			;length not one, so skip reboot
		notc:
			;not reboot, are we at end of buffer?
			cmp c! jc readnx 	;go for another if not
		readen:
			;end of read operation, store blen
			pop h! mov m,b 		;M(current len) = B
			push psw		;may be a ctl-z
			mvi a,cr! call conout	;return carriage
			pop psw			;restore character
			ret
;
func1:	equ	conech
	;return console character with echo
;
;func6:	see intercept routine at front of module
;
func10:	equ	read
	;read a buffered console line
;
func11: equ	conbrk
	;check console status
;
;

;******************************************************************
;		DATA AREA
;******************************************************************

statflg:	db	0	;non-zero if prev funct was status
	;
	;	

;******************************************************************
;	Following variables and entry points are used by GET.COM
;	Their order and contents must not be changed without also
;	changing GET.COM.
;******************************************************************
	;
	if bios$functions
	;
exit$table:			;addresses to go to on exit
	dw	next		;BDOS
	endif
	;
movstart:
init$table:			;addresses used by GET.COM for 
scbadr:	dw	kill		;address of System Control Block
	;
	if bios$functions	;GET.RSX initialization
	;
biosta	dw	bios$constat	;set to real BIOS routine
biosin	dw	bios$conin	;set to real BIOS routine
	;
				;restore only if changed when removed.
restore$mode
	db	0		;if non-zero change LXI @jmpadr to JMP
				;when removed.
restore$bios:
	;hl = real constat routine
	;de = real conin routine
	shld	0		;address of const jmp initialized by COM
	xchg
	shld	0		;address of conin jmp initialized by COM
	ret
	endif
	;
real$bdos:
	jmp	bdos		;address filled in by COM
	;
	;
echo:	db	1
cooked:	db	0
	;
program:
	db	0		;true if program input only	
subusr:	db	0		;user number for redirection file
subfcb:	db	1		;a:
	db	'SYSIN   '
	db	'SUB'
	db	0,0
submod:	db	0
subrc:	ds	1
	ds	16		;map
subcr:	ds	1
	;
movend:
;*******************************************************************

cbufp	db	128		;current character position in cbuf
nextchr	db	cr		;next character (1 char lookahead)

	if submit
ccp$line:
	db	false		;nonzero if line is for CCP
	endif

cbuf:				;128 byte record buffer

	db	3,3,3,3, 3,3,3,3, 3,3,3,3, 3,3,3,3
	db	3,3,3,3, 3,3,3,3, 3,3,3,3, 3,3,3,3
	db	3,3,3,3, 3,3,3,3, 3,3,3,3, 3,3,3,3
	db	3,3,3,3, 3,3,3,3, 3,3,3,3, 3,3,3,3

	db	3,3,3,3, 3,3,3,3, 3,3,3,3, 3,3,3,3
	db	3,3,3,3, 3,3,3,3, 3,3,3,3, 3,3,3,3
	db	3,3,3,3, 3,3,3,3, 3,3,3,3, 3,3,3,3
	db	3,3,3,3, 3,3,3,3, 3,3,3,3, 3,3,3,3

udma:	dw	buf		;user dma address
get$active:
	db	gkillf
	;
sav$area:			;14 byte save area (searchn)
	db	68h,68h,68h,68h,68h, 68h,68h,68h,68h,68h
	db	68h,68h,68h,68h
excess:	db	0
old$stack:
	dw	0
	if	submit
garbage:
;	db	cr,lf
	db	'WARNING: PROGRAM INPUT IGNORED',cr,lf,'$'
	else
ccpcnt:	db	1
	endif
patch$area:
	ds	30h
	db	' 151282 '
	db	' COPYR ''82 DRI '
	db	67h,67h,67h,67h,67h, 67h,67h,67h,67h,67h
	db	67h,67h,67h,67h,67h, 67h,67h,67h,67h,67h
	db	67h,67h,67h,67h,67h, 67h,67h,67h,67h,67h
	;
stack:				;15 level stack
	end
