;	assembly language version of mem$move for ed speedup
;	version 2.0 of ED
;
mem$move	equ	13cah
moveflag	equ	1d34h
direction	equ	1d20h
front		equ	1d22h
back		equ	1d24h
first		equ	1d26h
last		equ	1d28h
baseline	equ	1c10h
memory		equ	1d4dh
;
forward		equ	1
lf		equ	0ah
;
	org	mem$move
	lxi	h,moveflag
	mov	m,c	;1 = move data
	lxi	d,memory
	lhld	front
	dad	d	;memory+front
	push	h
	lhld	back
	dad	d
	push	h
	lda	direction
	cpi	forward
	jnz	moveback
	lhld	last
	mov	a,c	;moveflag to a
	rar
	jc	moveforw
;	set back to last
	shld	back
	pop	h
	pop	h
	ret
;
moveforw:
	dad	d	;memory+last
	mov	b,h
	mov	c,l
	pop	h
	pop	d	;bc=last, de=front, hl=back
movef:	mov	a,l	;back < last?
	sub	c
	mov	a,h
	sbb	b	;cy if true
	jnc	emove
	inx	h	;back=back+1
	mov	a,m	;char to a
	cpi	lf	;end of line?
	jnz	notlff
	push	h
	lhld	baseline
	inx	h	;baseline=baseline+1
	shld	baseline
	pop	h
notlff:
	stax	d	;to front
	inx	d	;front=front+1
	jmp	movef

moveback:
	lhld	first
	dad	d	;memory+first
	mov	b,h
	mov	c,l
	pop	h
	pop	d	;bc=first, de=front, hl=last
moveb:	mov	a,c	;first > front?
	sub	e
	mov	a,b
	sbb	d	;cy if true
	jnc	emove
	dcx	d	;front=front-1
	ldax	d	;char to a
	cpi	lf
	jnz	notlfb
	push	h
	lhld	baseline
	dcx	h	;baseline=baseline-1
	shld	baseline
	pop	h
notlfb:	push	psw	;save char
	lda	moveflag
	rar
	jnc	nomove
	pop	psw
	mov	m,a	;store to back
	dcx	h
	jmp	moveb
nomove:	pop	psw
	jmp	moveb
;
emove:	push	d
	lxi	d,-memory
	dad	d	;relative value of back
	shld	back
	pop	h
	dad	d	;relative value of front
	shld	front
	ret
	end
