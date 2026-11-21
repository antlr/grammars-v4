$title	('PUTF - CP/M 3.0 Output Redirection - August 1982')
;******************************************************************
;
;	PUT  'Redirection Initializer'  version 3.0
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
;	seteof put.plm
;	seteof getscan.dcl
;	seteof putf.asm
;	seteof getscan.plm
;	seteof parse.asm
;	is14
;	asm80 putf.asm debug
;	asm80 mcd80a.asm debug
;	asm80 parse.asm debug
;	plm80 put.plm pagewidth(100) debug optimize
;	link mcd80a.obj,put.obj,parse.obj,putf.obj,plm80.lib to put.mod
;	locate put.mod code(0100H) stacksize(100)
;	era put.mod
;	cpm
;	objcpm put
;	rmac putrsx
;	link putrsx[op]
;	era put.rsx
;	ren put.rsx=putrsx.prl
;	gencom put.com
;	gencom put.com put.rsx 
;
;
;	This module is called as an external routine by the
;	PL/M program PUT.  The address of a the following 
;	structure is passed:
;		
;		  declare putpb structure
;		    (output$type   byte,
;		     echo$flag     byte,
;		     filtered$flag byte,
;		     system$flag   byte);
;
;	output$type 	= 0     > console output (default)
;		    	= 1     > auxiliary output
;	            	= 2     > list output
;	                = 3     > console input
;
;	echo		= true  > echo output to real device
;				  (default)
;			= false > don't echo output (input is 
;				  still echoed)
;	filtered 	= true  > convert control characters
;				  to a printable form 
;			          preceeded by an ^
;			= false > no character conversions
;	program	 	= true  > continue until user uses 
;				  PUT command to revert to 
;			          console
;			= false > active only until program
;				  termination
	public	putf
	extrn	mon1,fcb,memsiz
;
;
true		equ	0ffffh
false		equ	00000h
;
biosfunctions	equ	true		;intercept BIOS list or conout
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
lchrf	equ	5	;list character
pbuff	equ	9	;print buffer
resetf	equ	13	;disk reset
selectf	equ	14	;select disk
openf	equ	15	;open file
closef	equ	16	;close file
delf	equ	19	;delete file
dreadf	equ	20	;disk read
makef	equ	22	;make file
dmaf	equ	26	;set dma function
curdrv	equ	25	;get current drive
dpbf	equ	31	;get dpb address
userf	equ	32	;set/get user number
resdvf	equ	37	;reset drive
scbf	equ	49	;set/get system control block word
rsxf	equ	60	;RSX function call
resalvf	equ	99	;reset allocation vector
pblkf	equ	111	;print block to console
lblkf	equ	112	;print block to list device
ginitf	equ	128	;GET initialization sub-function no.
gkillf	equ	129	;GET delete sub-function no.
gfcbf	equ	130	;GET file display sub-function no.
pinitf	equ	132	;PUT initialization sub-funct no.
pckillf	equ	133	;PUT CON: delete sub-function no.
pcfcbf	equ	134	;return PUT CON: fcb address
plkillf	equ	137	;PUT LST: delete sub-function no.
plfcbf	equ	138	;return PUT LST:fcb address
jinitf	equ	140	;JOURNAL initialization sub-funct no.
jkillf	equ	141	;JOURNAL delete sub-function no.
jfcbf	equ	142	;return JOURNAL fcb address
skillf	equ	144	;SUBMIT delete sub-function no.
sfcbf	equ	145	;SUBMIT fcb address function
svkillf	equ	160	;SAVE delete sub-function no.
;
;	System Control Block definitions
;
scba	equ	03ah	;offset of scbadr from SCB base
ccpflg1	equ	0b3h	;offset of ccpflags word from page boundary
submit	equ	040h	;mask for active submit or get test
errflg	equ	0aah	;offset of error flag from page boundary
conmode	equ	0cfh	;offset of console mode from page boundary
listcp	equ	0d4h	;offset of ^P flag from page boundary
common	equ	0f9h	;offset of common memory base from pg. bound
wbootfx	equ	068h	;offset of warm boot jmp from page. bound
constfx	equ	06eh	;offset of constat jmp from page. bound
coninfx	equ	074h	;offset of conin jmp from page. bound
conoufx	equ	07ah	;offset of conout jmp from page. bound
listfx	equ	080h	;offset of list jmp from page. bound
cstjmp	equ	003h	;offset of console status jmp from warm boot
cinjmp	equ	006h	;offset of console input jmp from warm boot
coujmp	equ	009h	;offset of console output jmp from warm boot
lstjmp	equ	00ch	;offset of list output jmp from warm boot

;
;	Restore mode equates    (used with inr a, rz, rm, ret)
;
norestore	equ	0ffh	;no BIOS interception
biosonly	equ	07fh	;restore BIOS jump table only
everything	equ	0	;restore BIOS jump table and jmps in
				;RESBDOS (default mode)
;
;	Instructions
;
lxih		equ	21h	;LXI H, instruction
jmpi		equ	0c3h	;jump instruction
;
;******************************************************************
;		START OF INITIALIZATION CODE
;******************************************************************
		cseg

putf:
	;get parameters
	mov	h,b
	mov	l,c		;HL = .(parameter block)
	mov	a,m		;output type 0=con:,1=aux:,2=lst:,3=conin:
	cpi	1		;is it aux?
	jz	notimp		;error if so
	cpi	3		;is it console input only
	jnz	setlst
	sta	input		;non-zero => console input
	xra	a
setlst:	sta	list		;non-zero => list device
	inx	h
	mov	a,m		;echo/noecho mode
	sta	echo
	inx	h
	mov	a,m		;cooked/raw mode	
	sta	cooked	
	inx	h
	mov	a,m		;system/program mode
	sta	program
	;
	;check if enough memory
	;
	lhld	memsiz
	mov	a,h
	cpi	20h
	lxi	d,memerr
	jc	error
	;
	;check if drive specified
	lxi	h,fcb
	mov	a,m		;drive code
	dcr	a		;drive specified?
	jp	movfcb		;jump if so
	;
	;set to current drive, if  not
	;
	mvi	c,curdrv
	push	h		;save .fcb
	call	mon1
	pop	h		;a=current drive, hl=.fcb
	mov	m,a		;set fcb to force drive select
	inr	m		;must be relative to 1
	;
movfcb:	;copy default fcb up into data area for move to RSX
	;
	mov	e,a
	mvi	c,selectf	;make sure drive is selected
	push	h		;save .fcb
	call	mon1		;so we get the right DPB
	pop	h	
	lxi	d,putfcb
	lxi	b,32		;length of fcb
	call	ldir		;move it to putfcb
	;
	;initialize other variables to be moved to RSX
	;
	call	getusr		;get current user number
	sta	putusr		;save for redirection file I/O
	call	getscbadr
	shld	scbadr		;System Control Block address
	;
	;initialize records per block (BLM)
	;
	mvi	c,dpbf
	call	mon1		;HL = .disk parameter block
	inx	h
	inx	h
	inx	h		;HL = .blm
	mov	a,m
	sta	blm
	;
	;initialize function table (functions to be intercepted)
	;
	lda	list
	ora	a
	lxi	b,funcend-functbl		;count
	lxi	d,functbl			;destination
	lxi	h,pcfcbf*256+pckillf		;rsx function codes
	jz	ckinput
	lxi	h,listfunc			;list function table
	call	ldir
	mvi	a,lchrf
	sta	bdosfunc			;use list output for bios trap
	mvi	a,listfx
	sta 	resoff				;offset of fixup for bios list
	mvi	a,lstjmp
	sta	biosoff				;offset of bios lst jmp 
	lxi	h,plfcbf*256+plkillf
	jmp	getrsxadr
ckinput:
	lda	input
	ora	a
	jz	getrsxadr
	lxi	h,inputfunc
	call	ldir
	mvi	a,cinf
	sta	bdosfunc			;use console input
	mvi	a,coninfx
	sta	resoff				;offset of fixup for bios conin
	mvi	a,cinjmp
	sta	biosoff
	sta	echo				;must be non-zero for input
	lhld	scbadr
	mvi	l,ccpflg+1
	mov	a,m
	ani	submit				;SUBMIT or GET active?
	lxi	d,noget
	jnz	error				;error if so
	lxi	h,jfcbf*256+jkillf
	;
	;get address of initialization table in RSX
	;
getrsxadr:
	shld	rsxfun
	mvi	c,rsxf		;PUT is not compatible with SAVE.RSX
	lxi	d,savkill	;as both SAVE & PUT trap warm starts
	call	mon1		;eliminate SAVE.RSX if active
	mvi	c,rsxf
	lxi	d,rsxinit
	call	mon1		;call PUT.RSX initialization routine
	push	h		;save address of destination for move
	mov	e,m
	inx	h		
	mov	d,m		;DE = .kill flag 
	push	d		;save for later set
	;
if biosfunctions
	;
	inx	h
	inx	h
	inx	h		;HL = .(.(bios entry in RSX))
	push	h		;save for getting RSX entry point
				;later (in trap:)
	;check if BIOS jump table looks valid (jmp in right places)
check:	lhld	biosoff
	xchg
	lhld	wboota
	mov	a,m
	cpi	jmpi		;should be a jump
	dad	d		;HL = .(jmp address)
	mov	a,m
	cpi	jmpi		;should be a jump
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
	;fix warmboot BIOS jmp in resbdos
fix0:	mvi	l,wbootfx	;HL = .warm boot fix in SCB
	shld	wmfix		;save for RSX restore at end
	mov	a,m
	cpi	jmpi		;is it a jump instruction?
	jz	fix1		;jump if so
	mvi	a,biosonly	;whoops already traped
	sta	biosmode
fix1:	mvi	m,lxih		;change jump to an lxi h,
	;fix list bios jmp in resbdos
	lda	resoff
	mov	l,a
	shld	biosfix
	mov	a,m
	cpi	jmpi		;is it a jump instruction?
	jz	biosck		;jump if so
	mvi	a,biosonly	;whoops already changed
	sta	biosmode	;restore jump table only
fix3:	mvi	m,lxih
	;
	;get address of list entry point
	;
trap:	pop	h		;.(.(bios entry point in RSX))
	mov	c,m
	inx	h
	mov	b,m
	push	h
	lhld	biosoff
	xchg	
	lhld	wboota
	dad	d		;HL = .(jmp address)
	inx	h		;move past jmp instruction
	shld	biosjmp		;save for RSX restore at end
	mov	e,m
	mov	m,c
	inx	h
	mov	d,m		;DE = bios routine address
	mov	m,b		;BIOS jmp jumps to RSX
	xchg
	shld	biosout		;save bios routine address
	;get addresses of RSX bios trap
	pop	h
	inx	h
	mov  	c,m		;HL = .(.(bios warm start in RSX))
	inx	h
	mov	b,m		;BC = .bios warmstart entry in RSX
	;
	;patch RSX wmboot entry into BIOS jump table 
	;save real wmboot address in RSX exit table
	;
	lhld	wboota
	inx	h
	shld	wmjmp		;save for RSX restore at end
	mov	e,m
	mov	m,c
	inx	h
	mov	d,m
	mov	m,b
	xchg
	shld	wmsta		;save real bios warm start routine
endif
	;
	;move data area to RSX
	;
rsxmov:
	pop	h		;HL = .(kill flag = 0FFh)
	inr	m		;set to zero for redirection active
	lxi	h,movstart
	pop	d		;RSX data area address
	lxi	b,movend-movstart
	call	ldir
	jmp	wboot
;
;	auxiliary redirection
;
notimp:
	lxi	d,notdone
error:
	mvi	c,pbuff
	call	mon1
	mvi	c,closef
	lxi	d,fcb
	call	mon1
	mvi	c,delf
	lxi	d,fcb
	call	mon1
	jmp	wboot
	

if biosfunctions
;
;	check if warm boot was fixed up by someone
;	and list or console output was not
;
biosck:	lda	biosmode
	cpi	biosonly
	jnz	fix3			;warm boot not fixed up
;
;	can't do BIOS redirection
;
bioserr:
	lxi	d,nobios
	mvi	c,pbuff
	call	mon1
	lxi	h,biosmode
	mvi	m,norestore
	pop	h		;throw away stacked bios entry 
	jmp	rsxmov
endif
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
;	equates function table
;
eot	equ	0ffh	; end of function table
skipf	equ	0feh	; skip this function
;
listfunc:
		db	lchrf, lblkf, coutf, cstatf, crawf
		db	pbuff, cinf, creadf, resetf, resdvf
		db	resalvf, pblkf, eot

;		Note that the list routines precede the console
;		routines so that the CKLIST: routine in PUTRSX
;		can distinquish list functions from console 
;		functions.

inputfunc:			;preset for console input
		db	skipf, skipf, skipf, skipf, crawf
		db	skipf, cinf, creadf, resetf, resdvf
		db	resalvf, eot, skipf


;
savkill:	db	svkillf
rsxinit:	db	Pinitf
nobios:		db	cr,lf,'WARNING: Cannot redirect from BIOS',cr,lf,'$'
notdone:
	db cr,lf
	db 'ERROR: Auxiliary device redirection not implemented',cr,lf,'$'
memerr:
	db cr,lf	
	db 'ERROR: Insufficient Memory',cr,lf,'$'
noget:
	db cr,lf
	db 'ERROR: You cannot PUT INPUT to a file',cr,lf
	db '       when using GET or SUBMIT.',cr,lf,'$'
resoff:		db	conoufx
biosoff:	dw	coujmp
aux:		db	0
	;	
;******************************************************************
;	Following variables are initialized by PUT.COM
;	and moved to the PUT RSX - Their order must not be changed
;******************************************************************
	;
	;
movstart:
inittable:			;addresses used by PUT.COM for 
scbadr:	dw	0		;address of System Control Block
	;
	if biosfunctions	;PUT.RSX initialization
	;
gobios:	mov	c,e
	db	jmpi
biosout:
	dw	0		;set to real BIOS routine
	;
				;restore only if changed when removed.
biosjmp:
	dw	0		;address of bios jmp initialized by COM
biosfix:
	dw	0		;address of jmp in resbdos to restore
	db	jmpi
wmsta:	dw	0		;address of real warm start routine
wmjmp:	dw	0		;address of jmp in bios to restore
wmfix:	dw	0		;address of jmp in resbdos to restore
bdosfunc:
	db	coutf
biosmode:
	db	0		;0FFh = no bios restore, 07fh = restore
				;only bios jmp, 0 = restore bios jump and
				;resbdos jmp when removed.
	endif

functbl:			;preset for console output
	db	skipf, skipf, coutf, cstatf, crawf, pbuff
	db	cinf, creadf, resetf, resdvf, resalvf, pblkf, eot

funcend:
	;
input:	db	0		;non-zero if putting input to a file
list:	db	0		;TRUE if list output redirection
echo:	db	1		;echo output to device
cooked:				;must be next after echo
	db	0		;TRUE if ctrl chars displayed with ^
rsxfun:
pkillf:	db	255		;put abort routine code
pfcbf:	db	255		;put FCB display function no.
	;	**********  remaining variables must be in this order
record:	db	0		;counts down records to block boundary
blm:	db	0		;block mask = records per block (rel 0)
program:			;This must be @ .putfcb-2
	db	0
putusr:	db	0		;user number for redirection file
putfcb:	db	1		;a
	db	'SYSOUT  '
	db	'$$$'
	db	0,0
putmod:	db	0
putrc:	db	0
	ds	16		;map
putcr:	db	0
	;
cbufp:	db	0
movend:
;*******************************************************************
	end

