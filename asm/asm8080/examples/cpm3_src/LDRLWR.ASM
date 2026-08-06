$title	('CP/M V3.0  Relocate and Fix Up File')
	name	relfix
;
;/*
;  Copyright (C) 1979,1980,1981,1982
;  Digital Research
;  P.O. Box 579
;  Pacific Grove, CA 93950
;
;  Revised:
;    05 Aug 82 by Bruce Skidmore
;*/

	cseg

	extrn	mon1	;BDOS entry point
	extrn	FCBin	;FCB for input
	extrn	sctbfr	;sector buffer
	extrn	offset	;relocation offset
	extrn	prgsiz	;program size
	extrn	bufsiz	;buffer size
	extrn	bnkpg	;bnkbdos page
	extrn	respg	;resbdos page
	extrn	scbpg	;System Control Block page
	extrn	biospg	;Bios page
	extrn	reslen	;Resident System length
	extrn	bnkoff	;Banked System offset
	extrn	nonbnk	;Non Banked CP/M flag

	public	bitmap	;bitmap buffer

RelFix:
	public	RelFix
	lxi	d,bitmap
	mvi	c,26
	call	mon1	;set DMA address to bit map
;
	;file loaded, ready for relocation
	lhld	prgsiz
	mov	b,h
	mov	c,l		;BC = program size
	mov	a,l
	ani	127
	mov	l,a
	jnz	nofill		;if program size is an even number
	push	h		;of sectors prefill the bitmap buffer
	push	b
	lhld	fcbin
	xchg
	mvi	c,20
	call	mon1
	pop	b
	pop	h
	ora	a
	jnz	errtn
nofill:
	mov	e,l		;L = offset into bitmap buffer
	mvi	d,0
	lxi	h,bitmap
	dad	d		;HL = bit map base
	mvi	a,low(bitmap+128)
	sta	btmptp		;save number of relocation bytes
				;in left in bitmap buffer
	lxi	d,sctbfr	;DE = base of program
	push	h		;save bit map base in stack
	lda	offset
	mov	h,a		;H = relocation offset
pgrel0:
	mov	a,b		;bc=0?
	ora	c
	jz	ExitRelFix
;
;	not end of the relocation,
;	  may be into next byte of bit map
	dcx	b	;count length down
	mov	a,e
	sui	low(sctbfr)
	ani	111b	;0 causes fetch of next byte
	jnz	pgrel3
;	fetch bit map from stacked address
	xthl
	lda	btmptp
	cmp	l
	jnz	pgrel2
	push	b
	push	d
	lhld	FCBin
	xchg
	mvi	c,20
	call	mon1
	pop	d
	pop	b
	lxi	h,bitmap
	ora	a
	jnz	errtn	;return with error condition
pgrel2:
	mov	a,m	;next 8 bits of map
	inx	h
	xthl		;base address goes back to stack
	mov	l,a	;l holds map as 8 bytes done
pgrel3:
	mov	a,l
	ral		;cy set to 1 if reloc necessary
	mov	l,a	;back to l for next time around
	jnc	pgrel4	;skip relocation if cy=0
;
;	current address requires relocation
;
	push	h
	ldax	d		;if page = 0ffh
	inr	a
	jnz	test2
	lda	biospg		;then page = bios$page
	jmp	endt
test2:				;else
	inr	a		;if page = 0feh
	jnz	test3
	lda	scbpg		;then page = SCB$page
	push	psw
	dcx	d		;add 9ch to the offset(low byte)
	ldax	d
	adi	09ch
	stax	d
	inx	d
	pop	psw
	jmp	endt
test3:				;else
	inr	a		;if page = 0fdh
	jnz	test4
	lda	respg		;then page = resbdos$page
	jmp	endt
test4:				;else
	inr	a		;if page = 0fch
	jnz	test5
	lda	bnkpg		;then page = bnkbdos$page
	jmp	endt
test5:				;else
	inr	a		;if page = 0fbh
	jnz	test6
	lda	scbpg		;then page = scb$page
	jmp	endt
test6:				;else
	lda	reslen
	mov	h,a		;if non$banked and page >= reslen
	lda	nonbnk
	ora	a
	jz	test7
	ldax	d
	sub	h
	jc	default		;then do;
	dcx	d		;page$adr = page$adr - 1;
	mvi	a,09ah
	stax	d		;page = 9ah;
	inx	d		;page$adr = page$adr + 1;
	lda	scbpg		;page = scb$pg;
	jmp	endt		;end;
test7:				;else
	lda	bnkoff
	mov	l,a		;if page >= reslen
	ldax	d
	sub	h
	jc	default
	add	l		;then page = page - reslen
	jmp	endt
default:			;else
	lda	offset		;page = page + offset
	mov	h,a
	ldax	d
	add	h
endt:
	stax	d
	pop	h
pgrel4:
	inx	d	;to next address
	jmp	pgrel0	;for another byte to relocate

ExitRelFix:
	pop	h
	lxi	h,0
	mov	a,h
	ret

errtn:
	pop	h	;discard return address
	lxi	h,0ffffh
	mov	a,h
	ret		;return with error condition
;
;	Local Data Segment
;
bitmap:	ds	128	;bit map buffer
btmptp:	ds	1	;bit low (bitmap+128)

	end
