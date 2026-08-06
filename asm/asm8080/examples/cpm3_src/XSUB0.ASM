;        xsub relocator     version 2.2
version	equ	20h
;	xsub relocator program, included with the module
;	to perform the move from 200h to the destination address
;
;	copyright (c) 1979, 1980
;	digital research
;	box 579
;	pacific grove, ca.
;	93950
;
	org	100h
	db	(lxi or (b shl 3))	;lxi b,module size
	org	$+2			;skip address field
	jmp	start
	db	' Extended Submit Vers '
	db	version/16+'0','.',version mod 16+'0'
nogo:	db	'Xsub Already Present$'
badver:	db	'Requires CP/M Version 2.0 or later$'
;
bdos	equ	0005h	;bdos entry point
print	equ	9	;bdos print function
vers	equ	12	;get version number
ccplen	equ	0800h	;size of ccp
module	equ	200h	;module address
;
start:
;	ccp's stack used throughout
	push	b	;save the module's length
	lda	bdos+1	;xsub already present?
	cpi	06h	;low address must be 06h
	jnz	loaderr
	lhld	bdos+1
	inx	h
	inx	h
	inx	h
	lxi	d,xsubcon
	mvi	c,4    
present:
	ldax	d
	cmp	m
	jnz	continue
	inx	h
	inx	d
	dcr	c
	jz	loaderr
	jmp	present
;
loaderr:
;	bdos or xsub not lowest module in memory, return to ccp
	mvi	c,print
	lxi	d,nogo	;already present message
	call	bdos	;to print the message
	pop	b	;recall length
	ret		;to the ccp
;
continue:
	mvi	c,vers
	call	bdos	;version number?
	cpi	version	;2.0 or greater
	jnc	versok
;
;	wrong version
	mvi	c,print
	lxi	d,badver
	call	bdos
	pop	b
	ret		;to ccp
;
versok:
	lxi	h,bdos+2;address field of jump to bdos (top memory)
	mov	a,m	;a has high order address of memory top
	dcr	a	;page directly below bdos
	sui	(ccplen shr 8)	;-ccp pages
	pop	b	;recall length of module
	push	b	;and save it again
	sub	b	;a has high order address of reloc area
	mov	d,a
	mvi	e,0	;d,e addresses base of reloc area
	push	d	;save for relocation below
;
	lxi	h,module;ready for the move
move:	mov	a,b	;bc=0?
	ora	c
	jz	reloc
	dcx	b	;count module size down to zero
	mov	a,m	;get next absolute location
	stax	d	;place it into the reloc area
	inx	d
	inx	h
	jmp	move
;
reloc:	;storage moved, ready for relocation
;	hl addresses beginning of the bit map for relocation
	pop	d	;recall base of relocation area
	pop	b	;recall module length
	push	h	;save bit map base in stack
	mov	h,d	;relocation bias is in d
;
rel0:	mov	a,b	;bc=0?
	ora	c
	jz	endrel
;
;	not end of the relocation, may be into next byte of bit map
	dcx	b	;count length down
	mov	a,e
	ani	111b	;0 causes fetch of next byte
	jnz	rel1
;	fetch bit map from stacked address
	xthl
	mov	a,m	;next 8 bits of map
	inx	h
	xthl		;base address goes back to stack
	mov	l,a	;l holds the map as we process 8 locations
rel1:	mov	a,l
	ral		;cy set to 1 if relocation necessary
	mov	l,a	;back to l for next time around
	jnc	rel2	;skip relocation if cy=0
;
;	current address requires relocation
	ldax	d
	add	h	;apply bias in h
	stax	d
rel2:	inx	d	;to next address
	jmp	rel0	;for another byte to relocate
;
endrel:	;end of relocation
	pop	d	;clear stacked address
;	h has the high order 8-bits of relocated module address
	mvi	l,0
	pchl		;go to relocated program
xsubcon:
	db	'xsub'
	end
