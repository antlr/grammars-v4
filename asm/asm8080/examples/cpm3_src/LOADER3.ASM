title	'CP/M 3 - PROGRAM LOADER RSX - November 1982'
;	version 3.0b  Nov 04 1982 - Kathy Strutynski
;	version 3.0c  Nov 23 1982 - Doug Huskey
;	              Dec 22 1982 - Bruce Skidmore
;
;
;	copyright (c) 1982
;	digital research
;	box 579
;	pacific grove, ca.
;	93950
;
 	****************************************************
 	*****  The following values must be placed in    ***
 	*****  equates at the front of CCP3.ASM.         ***
 	*****                                            ***
 	*****  Note: Due to placement at the front these ***
 	*****  equates cause PHASE errors which can be   ***
 	*****  ignored.                                  ***
equ1	equ	rsxstart +0100h  ;set this equate in the CCP
equ2	equ	fixchain +0100h  ;set this equate in the CCP
equ3	equ	fixchain1+0100h  ;set this equate in the CCP
equ4	equ	fixchain2+0100h  ;set this equate in the CCP
equ5	equ	rsx$chain+0100h  ;set this equate in the CCP
equ6	equ	reloc    +0100h  ;set this equate in the CCP
equ7	equ	calcdest +0100h  ;set this equate in the CCP
equ8	equ	scbaddr	 +0100h  ;set this equate in the CCP
equ9	equ	banked	 +0100h  ;set this equate in the CCP
equ10	equ	rsxend	 +0100h  ;set this equate in the CCP
ccporg	equ	CCP		 ;set origin to this in CCP
patch	equ	patcharea+0100h  ;LOADER patch area

CCP	equ	41Ah		 ;ORIGIN OF CCP3.ASM


 	****************************************************

;	conditional assembly toggles:

true		equ	0ffffh
false		equ	0h
spacesaver	equ	true

stacksize	equ	32		;16 levels of stack
version		equ	30h
tpa		equ	100h
ccptop		equ	0Fh		;top page of CCP
osbase		equ	06h		;base page in BDOS jump
off$nxt		equ	10		;address in next jmp field
currec		equ	32		;current record field in fcb
ranrec		equ	33		;random record field in fcb



;
;
;     dsect for SCB
;
bdosbase	equ	98h		; offset from page boundary
ccpflag1	equ	0b3h		; offset from page boundary
multicnt	equ	0e6h		; offset from page boundary
rsx$only$clr	equ	0FDh		;clear load RSX flag
rsx$only$set	equ	002h
rscbadd		equ	3ah		;offset of scbadd in SCB
dmaad		equ	03ch		;offset of DMA address in SCB
bdosadd		equ	62h		;offset of bdosadd in SCB
;
loadflag	equ	02H		;flag for LOADER in memory
;
;     dsect for RSX
entry		equ	06h		;RSX contain jump to start
;
nextadd		equ	0bh		;address of next RXS in chain
prevadd		equ	0ch		;address of previous RSX in chain
warmflg		equ	0eh		;remove on wboot flag
endchain	equ	18h		;end of RSX chain flag
;
;
readf	equ	20	;sequential read
dmaf	equ	26	;set DMA address
scbf	equ	49	;get/set SCB info
loadf	equ	59	;load function
;
;
maxread	equ	64	;maximum of 64 pages in MULTIO
;
;
wboot	equ	0000h	;BIOS warm start
bdos	equ	0005h	;bdos entry point
print	equ	9	;bdos print function
vers	equ	12	;get version number
module	equ	200h	;module address
;
;	DSECT for COM file header
;
comsize	equ	tpa+1h
scbcode	equ	tpa+3h
rsxoff	equ	tpa+10h
rsxlen	equ	tpa+12h
;
;
cr	equ	0dh
lf	equ	0ah
;
;
	cseg
;
;
;     ********* LOADER  RSX HEADER ***********
;
rsxstart:
	jmp	ccp		;the ccp will move this loader to 
	db	0,0,0		;high memory, these first 6 bytes
				;will receive the serial number from
				;the 6 bytes prior to the BDOS entry
				;point
tojump:
	jmp	begin
next	db	0c3h		;jump to next module
nextjmp	dw	06
prevjmp	dw	07
	db	0		;warm start flag
	db	0		;bank flag
	db	'LOADER  '	;RSX name
	db	0ffh		;end of RSX chain flag
	db	0		;reserved
	db	0		;patch version number

;     ********* LOADER  RSX ENTRY POINT ***********

begin:
	mov	a,c
	cpi	loadf
	jnz	next
beginlod:
	pop	b
	push	b		;BC = return address
	lxi	h,0		;switch stacks
	dad	sp
	lxi	sp,stack	;our stack
	shld	ustack		;save user stack address
	push	b		;save return address
	xchg			;save address of user's FCB
	shld	usrfcb
	mov	a,h		;is .fcb = 0000h
	ora	l
	push	psw
	cz	rsx$chain	;if so , remove RSXs with remove flag on
	pop	psw
	cnz	loadfile
	pop	d		;return address
	lxi	h,tpa
	mov	a,m
	cpi	ret
	jz	rsxfile
	mov	a,d		;check return address
	dcr	a		; if CCP is calling 
	ora	e		; it will be 100H
	jnz	retuser1	;jump if not CCP
retuser:
	lda	prevjmp+1	;get high byte
	ora	a		;is it the zero page (i.e. no RSXs present)
	jnz	retuser1	;jump if not
	lhld	nextjmp		;restore five....don't stay arround
	shld	osbase
 	shld	newjmp
	call	setmaxb
retuser1:
	lhld	ustack		;restore the stack
	sphl
	xra	a
	mov	l,a
	mov	h,a		;A,HL=0 (successful return)
	ret			;CCP pushed 100H on stack
;
;
;	BDOS FUNC 59 error return
;
reterror:
	lxi	d,0feh
reterror1:
	;DE = BDOS error return
	lhld	ustack
	sphl
	pop	h		;get return address
	push	h
	dcr	h		;is it 100H?
	mov	a,h
	ora	l
	xchg			;now HL = BDOS error return
	mov	a,l
	mov	b,h
	rnz			;return if not the CCP
;
;
loaderr:
	mvi	c,print
	lxi	d,nogo		;cannot load program
	call	bdos		;to print the message
	jmp	wboot		;warm boot

;
;
;;
;************************************************************************
;
;	MOVE RSXS TO HIGH MEMORY
;
;************************************************************************
;
;
;      RSX files are present
;
	
rsxf1:	inx	h
	mov	c,m
	inx	h
	mov	b,m		;BC contains RSX length
	lda	banked
	ora	a		;is this the non-banked system?
	jz	rsxf2		;jump if so
	inx	h		;HL = banked/non-banked flag
	inr	m		;is this RSX only for non-banked?
	jz	rsxf3		;skip if so
rsxf2:	push	d		;save offset
	call	calcdest	;calculate destination address and bias
	pop	h		;rsx offset in file
	call	reloc		;move and relocate file
	call	fixchain	;fix up rsx address chain
rsxf3:	pop	h		;RSX length field in header


rsxfile:
	;HL = .RSX (n-1) descriptor 
	lxi	d,10h		;length of RSX descriptor in header
	dad	d		;HL = .RSX (n) descriptor
	push	h		;RSX offset field in COM header
	mov	e,m
	inx	h
	mov	d,m		;DE = RSX offset
	mov	a,e
	ora 	d
	jnz	rsxf1		;jump if RSX offset is non-zero
;
;
;
comfile:
	;RSXs are in place, now call SCB setting code 
	call	scbcode		;set SCB flags for this com file
	;is there a real COM file?
	lda	module		;is this an RSX only
	cpi	ret
	jnz	comfile2	;jump if real COM file
	lhld	scbaddr
	mvi	l,ccpflag1
	mov	a,m
	ori	rsx$only$set	;set if RSX only
 	mov	m,a
comfile2:
	lhld	comsize		;move COM module to 100H
	mov	b,h
	mov	c,l		;BC contains length of COM module
	lxi	h,tpa+100h	;address of source for COM move to 100H
	lxi	d,tpa		;destination address
	call	move
	jmp	retuser1		;restore stack and return
;;
;************************************************************************
;
;	ADD AN RSX TO THE CHAIN
;
;************************************************************************
;
;
fixchain:
	lhld	osbase		;next RSX link
	mvi	l,0
	lxi	b,6
	call	move		;move serial number down
	mvi	e,endchain
	stax	d		;set loader flag=0
	mvi	e,prevadd+1
	stax	d		;set previous field to 0007H
	dcx	d
	mvi	a,7
	stax	d		;low byte = 7H
	mov	l,e		;HL address previous field in next RSX
	mvi	e,nextadd	;change previous field in link
	mov	m,e
	inx	h
	mov	m,d		;current <-- next
;
fixchain1:
	;entry:	H=next RSX page, 
	;	DE=.(high byte of next RSX field) in current RSX
	xchg			;HL-->current  DE-->next
	mov	m,d		;put page of next RSX in high(next field)
	dcx	h
	mvi	m,6
;
fixchain2:
	;entry:	H=page of lowest active RSX in the TPA
	;this routine resets the BDOS address @ 6H and in the SCB
	mvi	l,6
	shld	osbase		;change base page BDOS vector
	shld	newjmp		;change SCB value for BDOS vector
;
;
setmaxb:
	lxi	d,scbadd2
scbfun:
	mvi	c,scbf
	jmp	bdos
;
;
;;
;************************************************************************
;
;	REMOVE TEMPORARY RSXS
;
;************************************************************************
;
;
;
rsx$chain:
	;
	;	Chase up RSX chain, removing RSXs with the
	;	remove flag on (0FFH)
	;
	lhld	osbase			;base of RSX chain
	mov	b,h

rsx$chain1:
	;B  = current RSX
	mov	h,b
	mvi	l,endchain
	inr	m
	dcr	m			;is this the loader?
	rnz				;return if so (m=0ffh)
	mvi	l,nextadd		;address of next node
	mov	b,m			;DE -> next link
;
;
check$remove:
;
	mvi	l,warmflg		;check remove flag
 	mov	a,m			;warmflag in A
	ora	a			;FF if remove on warm start
	jz	rsx$chain1		;check next RSX if not
;
remove:
		;remove this RSX from chain
;
	;first change next field of prior link to point to next RSX
	;HL = current  B = next
;
	mvi	l,prevadd
	mov	e,m			;address of previous RSX link
	inx	h
	mov	d,m
	mov	a,b			;A = next (high byte)
	stax	d			;store in previous link
	dcx	d			;previous RSX chains to next RSX
	mvi	a,6			;initialize low byte to 6
	stax	d			;
	inx	d			;DE = .next (high byte)
;
	;now change previous field of next link to address previous RSX
	mov	h,b			;next in HL...previous in DE
	mvi	l,prevadd
	mov	m,e
	inx	h
	mov	m,d			;next chained back to previous RSX
	mov	a,d			;check to see if this is the bottom
	ora	a			;RSX...
	push	b
	cz	fixchain2		;reset BDOS BASE to page in H
	pop	b
	jmp	rsx$chain1		;check next RSX in the chain
;
;
;;
;************************************************************************
;
;	PROGRAM LOADER
;
;************************************************************************
;
;
;
loadfile:
;	entry: HL = .FCB
	push	h
	lxi	d,scbdma		
	call	scbfun
	xchg
	pop	h			;.fcb
	push	h			;save .fcb
	lxi	b,currec
	dad	b
	mvi	m,0			;set current record to 0
	inx	h
	mov	c,m			;load address 
	inx	h
	mov	h,m
	mov	l,c
	dcr	h
	inr	h	
	jz	reterror		;Load address < 100h
	push	h			;now save load address
	push	d			;save the user's DMA
	push	h
	call	multio1			;returns A=multio
	pop	h
	push	psw			;save A = user's multisector I/O
	mvi	e,128			;read 16k

	;stack:		|return address|
	;		|.FCB          |
	;		|Load address  |
	;		|users DMA     |
	;		|users Multio  |
	;

loadf0:
	;HL= next load address (DMA)
	; E= number of records to read
	lda	osbase+1		;calculate maximum number of pages
	dcr	a
	sub	h
	jc	endload			;we have used all we can
	inr	a
	cpi	maxread			;can we read 16k?
	jnc	loadf2
	rlc				;change to sectors
	mov	e,a			;save for multi i/o call
	mov	a,l			;A = low(load address)
	ora	a
	jz	loadf2			;load on a page boundary
	mvi	b,2			;(to subtract from # of sectors)
	dcr	a			;is it greater than 81h?
	jm	subtract		;080h < l(adr) <= 0FFh (subtract 2)
	dcr	b			;000h < l(adr) <= 080h (subtract 1)
subtract:
	mov	a,e			;reduce the number of sectors to
	sub	b			;compensate for non-page aligned
					;load address
	jz	endload			;can't read zero sectors
	mov	e,a
;
loadf2:
	;read the file
	push	d			;save number of records to read
	push	h			;save load address
	call	multio			;set multi-sector i/o
	pop	h
	push	h
	call	readb			;read sector
	pop	h
	pop	d			;restore number of records
	push	psw			;zero flag set if no error
	mov	a,e			;number of records in A
	inr	a
	rar				;convert to pages
	add	h
	mov	h,a			;add to load address
	shld	loadtop			;save next free page address
	pop	psw
	jz	loadf0			;loop if more to go

loadf4:
	;FINISHED load  A=1 if successful (eof)
	;		A>1 if a I/O error occured
	;
	pop	b			;B=multisector I/O count
	dcr	a			;not eof error?
	mov	e,b			;user's multisector count
	call	multio
	mvi	c,dmaf			;restore the user's DMA address
	pop	d	
	push	psw			;zero flag => successful load
	call	bdos			; user's DMA now restored
	pop	psw
	lhld	bdosret			;BDOS error return
	xchg
	jnz	reterror1
	pop	d			;load address	
	pop	h			;.fcb
	lxi	b,9			;is it a PRL?
	dad	b			;.fcb(type)
	mov	a,m
	ani	7fh			;get rid of attribute bit
	cpi	'P'			;is it a P?
	rnz				;return if not
	inx	h
	mov	a,m
	ani	7fh
	cpi	'R'			;is it a R
	rnz				;return if not
	inx	h
	mov	a,m
	ani	7fh
	sui	'L'			;is it a L?
	rnz				;return if not
	;load PRL file
	mov	a,e
	ora	a			;is load address on a page boundary
	jnz	reterror		;error, if not
	mov	h,d
	mov	l,e			;HL,DE = load address
	inx	h
	mov	c,m
	inx	h
	mov	b,m
	mov	l,e			;HL,DE = load address BC = length
;	jmp	reloc			;relocate PRL file at load address
;
;;
;************************************************************************
;
;	PAGE RELOCATOR
;
;************************************************************************
;
;
reloc:
;	HL,DE = load address (of PRL header)
;	BC    = length of program (offset of bit map)
	inr	h		;offset by 100h to skip header
	push	d		;save destination address
	push	b		;save length in bc
	call	move		;move rsx to correct memory location
	pop	b
	pop	d
	push	d		;save DE for fixchain...base of RSX
	mov	e,d		;E will contain the BIAS from 100h
	dcr	e		;base address is now 100h
				;after move HL addresses bit map
	;
	;storage moved, ready for relocation
	;	HL addresses beginning of the bit map for relocation
	;	E contains relocation bias
	;	D contain relocation address
	;	BC contains length of code
rel0:	push	h	;save bit map base in stack
	mov	h,e	;relocation bias is in e
	mvi	e,0
;
rel1:	mov	a,b	;bc=0?
	ora	c
	jz	endrel
;
;	not end of the relocation, may be into next byte of bit map
 	dcx	b	;count length down
	mov	a,e
	ani	111b	;0 causes fetch of next byte
	jnz	rel2
;	fetch bit map from stacked address
	xthl
	mov	a,m	;next 8 bits of map
	inx	h
	xthl		;base address goes back to stack
	mov	l,a	;l holds the map as we process 8 locations
rel2:	mov	a,l
	ral		;cy set to 1 if relocation necessary
	mov	l,a	;back to l for next time around
	jnc	rel3	;skip relocation if cy=0
;
;	current address requires relocation
	ldax	d
	add	h	;apply bias in h
	stax	d
rel3:	inx	d	;to next address
	jmp	rel1	;for another byte to relocate
;
endrel:	;end of relocation
	pop	d	;clear stacked address
	pop	d	;restore DE to base of PRL
	ret


;
;;
;************************************************************************
;
;	PROGRAM LOAD TERMINATION
;
;************************************************************************
;
;;	
;;
endload:
	call	multio1		;try to read after memory is filled
	lxi	h,80h		;set load address = default buffer
	call	readb
	jnz	loadf4		;eof => successful
	lxi	h,0feh		;set BDOSRET to indicate an error
	shld	bdosret
	jmp	loadf4		;unsuccessful (file to big)
;
;;
;
;;
;************************************************************************
;
;	SUBROUTINES
;
;************************************************************************
;
;
;
;	Calculate RSX base in the top of the TPA
;
calcdest:
;
;	calcdest returns destination in DE
;	BC contains length of RSX
;
	lda	osbase+1	;a has high order address of memory top
	dcr	a		;page directly below bdos
	dcx	b		;subtract 1 to reflect last byte of code
	sub	b		;a has high order address of reloc area
	inx	b		;add 1 back get bit map offset
	cpi	ccptop		;are we below the CCP
	jc	loaderr
	lhld	loadtop
	cmp	h		;are we below top of this module
	jc	loaderr
	mov	d,a
	mvi	e,0		;d,e addresses base of reloc area
	ret
;
;;
;;-----------------------------------------------------------------------
;;
;;	move memory routine

move:
;	move source to destination
;	where source is in HL and destination is in DE
;	and length is in BC
;
	mov	a,b	;bc=0?
	ora	c
	rz
	dcx	b	;count module size down to zero
	mov	a,m	;get next absolute location
	stax	d	;place it into the reloc area
	inx	d
	inx	h
	jmp	move
;;
;;-----------------------------------------------------------------------
;;
;;	Multi-sector I/O 
;;	(BDOS function #44)
;
multio1:
	mvi	e,1		;set to read 1 sector
;
multio:
	;entry: E = new multisector count
	;exit:	A = old multisector count
	lhld	scbaddr
	mvi	l,multicnt
	mov	a,m
	mov	m,e
	ret	
;;
;;-----------------------------------------------------------------------
;;
;;	read file 
;;	(BDOS function #20)
;;
;;	entry:	hl = buffer address (readb only)
;;	exit	z  = set if read ok
;;
readb:	xchg
setbuf:	mvi	c,dmaf
	push	h		;save number of records
	call	bdos
	mvi	c,readf
	lhld	usrfcb
	xchg
	call	bdos
	shld	bdosret		;save bdos return
	pop	d		;restore number of records
	ora	a
	rz				;no error on read
	mov	e,h		;change E to number records read
	ret
;
;
;************************************************************************
;
;	DATA AREA
;
;************************************************************************
;

nogo	db	cr,lf,'Cannot load Program$'

patcharea:
	ds	36			;36 byte patch area

scbaddr	dw	0
banked	db	0

scbdma	db	dmaad
	db	00h			;getting the value
scbadd2	db	bdosadd			;current top of TPA
	db	0feh			;set the value
;

	if not spacesaver

newjmp	ds	2			;new BDOS vector
loadtop	ds	2			;page above loaded program
usrfcb	ds	2			;contains user FCB add
ustack:	ds	2			; user stack on entry
bdosret	ds	2			;bdos error return
;
rsxend	:
stack	equ	rsxend+stacksize

	else

rsxend:
newjmp	equ	rsxend
loadtop	equ	rsxend+2
usrfcb	equ	rsxend+4
ustack	equ	rsxend+6
bdosret	equ	rsxend+8
stack	equ	rsxend+10+stacksize

	endif
	end
