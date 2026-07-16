$title('GETF  - CP/M 3.0 Input Redirection - August 1982')
	name	getf
;******************************************************************
;
;	get  'Input Redirection Initializer'  version 3.0
;
; 	11/30/82 - Doug Huskey
;******************************************************************
;
;
;	Copyright (c) 1982
;	Digital Research
;	P.O. Box 579
;	Pacific Grove, Ca.
;	93950
;
;
;	generation procedure
;
;	seteof get.plm
;	seteof getscan.dcl
;	seteof getf.asm
;	seteof getscan.plm
;	seteof parse.asm
;	is14
;	asm80 getf.asm debug
;	asm80 mcd80a.asm debug
;	asm80 parse.asm debug
;	plm80 get.plm pagewidth(100) debug optimize
;	link mcd80a.obj,get.obj,parse.obj,getf.obj,plm80.lib to get.mod
;	locate get.mod code(0100H) stacksize(100)
;	era get.mod
;	cpm
;	objcpm get
;	rmac getrsx
;	link getrsx[op]
;	era get.rsx
;	ren get.rsx=getrsx.prl
;	gencom get.com
;	gencom get.com get.rsx 
;	
;
;
;	This module is called as an external routine by the
;	PL/M routines GET and SUBMIT.  It is passed a structure
;	with the following format:
;
;
;		  declare getpb structure
;		    (input$type   byte,
;		     echo$flag     byte,
;		     filtered$flag byte,
;		     program$flag  byte);
;
;	input$type 	= 0     > console input (default)
;		    	= 1     > auxiliary output
;
;	echo		= true  > echo input to real device
;				  (default)
;			= false > don't echo input (output is 
;				  still echoed)
;	filtered 	= true  > convert control characters
;				  to a printable form 
;			          preceeded by an ^ in echo
;				  (default)
;			= false > no character conversions
;	program	 	= false > continue until EOF or 
;				  GET INPUT FROM CONSOLE
;			          command
;			= true  > active only until program
;				  termination
;
	public	getf
	extrn	mon1,fcb,memsiz
;
;
true		equ	0ffffh
false		equ	00000h
;
biosfunctions	equ	true		;intercept BIOS conin & constat 
;
;
;	low memory locations
;
wboot	equ	0000h
wboota	equ	wboot+1
;
;	equates for non graphic characters
;
cr	equ	0dh	; carriage return
lf	equ	0ah	; line feed
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
curdrv	equ	25
userf	equ	32	;set/get user number
scbf	equ	49	;set/get system control block word
rsxf	equ	60	;RSX function call
initf	equ	128	;GET initialization sub-function no.
killf	equ	129	;GET delete sub-function no.
jkillf	equ	141	;JOURNAL delete sub-function no.
;
;	System Control Block definitions
;
scba	equ	03ah	;offset of scbadr from SCB base
ccpflg2	equ	0b4h	;offset of 2nd ccp flag byte from pg bound
errflg	equ	0aah	;offset of error flag from page boundary
conmode	equ	0cfh	;offset of console mode from page boundary
listcp	equ	0d4h	;offset of ^P flag from page boundary
common	equ	0f9h	;offset of common memory base from pg. bound
wbootfx	equ	068h	;offset of warm boot jmp from page. bound
constfx	equ	06eh	;offset of constat jmp from page. bound
coninfx	equ	074h	;offset of conin jmp from page. bound
conoufx	equ	07ah	;offset of conout jmp from page. bound
listfx	equ	080h	;offset of list jmp from page. bound
realdos	equ	098h	;offset of real BDOS entry from pg. bound
;
;	Restore mode equates    (used with inr a, rz, rm, rpe, ret)
;
norestore	equ	0ffh	;no BIOS interception
biosonly	equ	07fh	;restore BIOS jump table only
stfix	equ	080h	;restore BIOS jump table and
				;restore JMP in RESBDOS for constat
everything	equ	0	;restore BIOS jump table and jmps in
				;RESBDOS (default mode)
;
;	Instructions
;
lxih		equ	21h	;LXI H, instruction
jmpi		equ	0c3h	;JMP instruction
shldi		equ	22h	;SHLD instruction
;
;******************************************************************
;		START OF INITIALIZATION CODE
;******************************************************************

		cseg

getf:
	;get parameters
	mov	h,b
	mov	l,c		;HL = .(parameter block)
	mov	a,m		;input type 0=con:,1=aux:
	cpi	1		;is it aux?
	jz	notimp		;error if so
	inx	h
	mov	a,m		;echo/noecho mode
	sta	echo
	inx	h
	mov	a,m		;cooked/raw mode	
	sta	cooked	
	inx	h
	mov	a,m
	sta	program
	;
	;check if enough memory
	;
	lhld	memsiz
	mov	a,h
	cpi	20h
	jc	nomem
	;
	;close to get those blocks in the directory
	;
	lxi	d,fcb
	mvi	c,closef
	call	mon1
	;
	;check if drive specified
	lxi	h,fcb
	mov	a,m		;drive code
	ora	a		;default?
	jnz	movfcb
	;
	;set to current drive, if  not
	;
	push	h		;save .fcb
	mvi	c,curdrv
	call	mon1
	pop	h		;a=current drive, hl=.fcb
	inr	a
	mov	m,a		;set fcb to force drive select
	;
movfcb:	;copy default fcb up into data area for move to RSX
	;
	lxi	d,subfcb
	lxi	b,32		;length of fcb
	call	ldir		;move it to subfcb
	;
	;initialize other variables to be moved to RSX
	;
	call	getusr		;get current user number
	sta	subusr		;save for redirection file I/O
	call	getscbadr
	shld	scbadr		;System Control Block address
	;
	;get real BDOS address (bypass chain to check for user break)
	;
	mvi	l,realdos
	mov	e,m
	inx	h
	mov	d,m
	xchg
	shld	realbdos+1
	;
	;check for user abort
	;
	xchg
	mvi	l,conmode
	mov	a,m
	ori	1		;set ^C status mode
	mov	m,a
	mvi	c,cstatf
	call	realbdos	;check for user abort
	ora	a
	jnz	error1		;abort if so
	;
	;get address of initialization table in RSX
	;
	mvi	c,rsxf
	lxi	d,journkill
	call	mon1		;terminate any PUT INPUT commands
	mvi	c,rsxf
	lxi	d,rsxinit
	call	mon1		;call GET.RSX initialization routine
	push	h		;save for move at end of setup
	mov	e,m
	inx	h
	mov	d,m		;DE = .RSXKILL flag 
	push	d		;set flag to zero if successfull
	inx	h		;HL = .(real bios status routine)
	push	h
	;
if biosfunctions
	;
	;check if BIOS jump table looks valid (jmp in right places)
	lhld	wboota
	lxi	d,3
	dad	d		;HL = .(jmp constat address)
	mov	a,m
	cpi	jmpi		;should be a jump
	jnz	bioserr		;skip bios redirection if not
	dad	d		;HL = .(jmp conin address)
	mov	a,m
	cpi	jmpi
	jnz	bioserr		;skip bios redirection if not
	;
	;fix up RESBDOS to do BIOS calls to intercepted functions
	;
	lhld	scbadr
	mvi	l,common+1
	mov	a,m		;get high byte of common base
	ora	a		
	jnz	fix0		;high byte = zero if non-banked
	mvi	a,biosonly
	sta	biosmode
	jmp	trap		;skip code that fixes resbdos
	;fix BIOS constat
fix0:	mvi	l,constfx		;hl = .constfx in SCB
	mov	a,m
	cpi	jmpi		;is it a jump instruction?
	jz	fix1		;jump if so
	mvi	a,biosonly	;whoops already changed
	sta	biosmode	;restore jump table only
fix1:	mvi	m,lxih
	;fix BIOS conin
	mvi	l,coninfx	;hl = .coninfx in SCB
	mov	a,m
	cpi	jmpi		;is it a jump instruction?
	lda	biosmode
	jz	fix2		;jump if so
	cpi	biosonly
	jnz	bioserr		;error if conin is LXI but not constat
	xra	a		;zero accumulator to jnz below
fix2:	cpi	biosonly	;was const already an LXI h?
	jnz	fix3		;jmp if not
	mvi	a,stfix	;restore constat jmp but not conin
	sta	biosmode
fix3:	mvi	m,lxih
	;get addresses of RSX const and conin traps
trap:	pop	h
	mov  	c,m		;HL = .(.bios constat trap)
	inx	h
	mov	b,m		;BC = .bios constat trap in RSX
	inx	h
	push	h		;save for CONIN setup
	;
	;patch RSX constat entry into BIOS jump table 
	;save real constat address in RSX exit table
	;
	lhld	wboota
	lxi	d,4
	dad	d		;HL = .(jmp constat address)
	shld	constjmp	;save for RSX restore at end
	mov	e,m
	mov	m,c
	inx	h
	mov	d,m		;DE = constat address
	mov	m,b		;BIOS constat jumps to RSX
	xchg
	shld	biosta		;save real constat address
	;
	;get address of RSX bios conin entry point
	;
	pop	h		;HL = .(RSX BIOS conin trap)
	mov	c,m
	inx	h
	mov	b,m
	;
	;patch RSX conin entry into BIOS jump table 
	;save real conin address in RSX exit table
	;
	xchg
	inx	h		;past jmp instruction
	inx	h		;HL = .(conin address)
	shld	coninjmp
	mov	e,m
	mov	m,c
	inx	h
	mov	d,m		;DE = conin address
	mov	m,b		;BIOS conin jumps to RSX
	xchg
	shld	biosin		;save real conin address
endif
	;
	;move data area to RSX
	;
rsxmov:
	pop	h		;HL = .Kill flag in RSX
	inr	m		;switch from FF to 0
	lxi	h,movstart
	pop	d		;RSX data area address
	lxi	b,movend-movstart
	call	ldir
	mvi	c,crawf
	mvi	e,0fdh		;raw console input
	call	mon1		;prime RSX by reading a char
	jmp	wboot

if biosfunctions
;
;	can't do BIOS redirection
;
bioserr:
	lxi	d,nobios
	mvi	c,pbuff
	call	mon1
	lxi	h,biosmode
	mvi	m,norestore	;no bios redirection 
	pop	h		;throw away bios constat trap adr
	jmp	rsxmov
endif
;
;	auxiliary redirection
;
notimp:
	lxi	d,notdone
error:
	mvi	c,pbuff
	call	mon1	
error1:	mvi	c,closef
	lxi	d,fcb
	call	mon1
	mvi	c,delf
	lxi	d,fcb
	call	mon1
	jmp	wboot
;
;	insufficient memory
;
nomem:	lxi	d,memerr
	jmp	error
	
;
;	get/set user number
;
getusr:	mvi	a,0ffh		;get current user number
setusr:	mov	e,a		;set current user number (in A)
	mvi	c,userf
	jmp	mon1
;
;	get system control block address 
;	(BDOS function #49)
;
;	exit:	hl = system control block address
;
getscbadr:
	mvi	c,scbf
	lxi	d,data49
	jmp	mon1
;
data49:	db	scba,0		;data structure for getscbadd
;
;	copy memory bytes (emulates z80 ldir instruction)
;
ldir:	mov	a,m		;get byte
	stax	d		;store it at destination
	inx	h		;advance pointers
	inx	d
	dcx	b		;decrement byte count
	mov	a,c		;loop if non-zero
	ora	b
	jnz	ldir
	ret
;
;******************************************************************
;		DATA AREA
;******************************************************************

	;
journkill:	db	jkillf
rsxinit:	db	initf
nobios:		db	'WARNING: Cannot redirect from BIOS',cr,lf,'$'
notdone:
	db 'ERROR: Auxiliary device redirection not implemented',cr,lf,'$'
memerr:
	db 'ERROR: Insufficient Memory',cr,lf,'$'
	;	
;******************************************************************
;	Following variables are initialized by GET.COM
;	and moved to the GET RSX - Their order must not be changed
;******************************************************************
	;
	;
	;
movstart:
inittable:			;addresses used by GET.COM for 
scbadr:	dw	0		;address of System Control Block
	;
	if biosfunctions	;GET.RSX initialization
	;
biosta:	dw	0		;set to real BIOS routine
biosin:	dw	0		;set to real BIOS routine
	;
				;restore only if changed when removed.
biosmode:
	db	0		;if non-zero change LXI @jmpadr to JMP
				;when removed.
restorebios:
	;hl = real constat routine
	;de = real conin routine
	db	shldi
constjmp:
	dw	0		;address of const jmp initialized by COM
	xchg
	db	shldi
coninjmp:
	dw	0		;address of conin jmp initialized by COM
	ret
	endif
	;
realbdos:
	jmp	0		;address filled in by COM
	;
echo:	db	1
cooked:	db	0
	;
program:
	db	0		;true if only program input 
subusr:	db	0		;user number for redirection file
subfcb:	db	1		;a:
	db	'SYSIN   '
	db	'SUB'
	db	0,0
submod:	db	0
subrc:	db	0
	ds	16		;map
subcr:	db	0
	;
movend:
;*******************************************************************
	end
EOF

