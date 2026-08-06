;      xsub  'Extended Submit Facility'  version 2.2
;
;
;
;	xsub loads below ccp, and feeds command lines to
;	programs which read buffered input
;
bias	equ	0000h	;bias for relocation
base	equ	0ffffh	;no intercepts below here
wboot	equ	0000h
bdos	equ	0005h
bdosl	equ	bdos+1
dbuff	equ	0080h
;
cr	equ	0dh	;carriage return
lf	equ	0ah	;line feed
modnum	equ	14	;module number position
pbuff	equ	9	;print buffer
rbuff	equ	10	;read buffer
openf	equ	15	;open file
closef	equ	16	;close file
delf	equ	19	;delete file
dreadf	equ	20	;disk read
dmaf	equ	26	;set dma function
;
;
	org	0000h+bias
;	initialize jmps to include xsub module
	jmp	start
	ds	3
trapjmp:
	jmp	trap
	db	'xsub'
start:
	lhld	wboot+1
	shld	savboot
	lxi	h,wstart
	shld	wboot+1
	lhld	bdosl
	shld	rbdos+1	;real bdos entry
	lxi	h,trapjmp	;address to fill
	shld	bdosl	;jmp @0005 leads to trap
	pop	h	;ccp return address
	shld	ccpret
	pchl		;back to ccp
;
rbdos:	jmp	0000h	;filled in at initialization
savboot:
	ds	2	;warm boot saved and restored at end
			;of submit file
;
wstart:
	lxi	sp,stack
	mvi	c,pbuff	;print message
	lxi	d,actmsg
	call	rbdos
	lxi	h,dbuff	;restore default buffer
	shld	udma
	call	rsetdma
	lxi	h,trapjmp
	shld	bdosl	;fixup low jump address
	lhld	ccpret	;back to ccp
	pchl
actmsg:	db	cr,lf,'(xsub active)$'
;
trap:	;arrive here at each bdos call
	pop	h	;return address
	push	h	;back to stack
	mov	a,h	;high address
	cpi	base shr 8
	jnc	rbdos	;skip calls on bdos above here
	mov	a,c	;function number
	cpi	rbuff
	jz	rnbuff	;read next buffer
	cpi	dmaf	;set dma address?
	jnz	rbdos	;skip if not
	xchg		;dma to hl
	shld	udma	;save it
	xchg
	jmp	rbdos
;
setdma:
	mvi	c,dmaf
	lxi	d,combuf
	call	rbdos
	ret
;
rsetdma:
	mvi	c,dmaf
	lhld	udma
	xchg
	call	rbdos
	ret
;
fbdos:
	push	b
	push	d
	call	setdma
	pop	d
	pop	b
	call	rbdos
	push	psw
	call	rsetdma
	pop	psw
	ret
;
cksub:	;check for sub file present
	mvi	c,openf
	lxi	d,subfcb
	call	fbdos	;submit file present?
	inr	a	;00 if not present
	ret
;
rnbuff:
	push	d	;command address
	call	cksub	;sub file present?
	pop	d
	mvi	c,rbuff
	jz	restor	;no sub file 
;
	push	d
	lda	subrc	;length of file
	ora	a	;zero?
	jz	rbdos	;skip if so
	dcr	a	;length - 1
	sta	subcr	;next to read
	mvi	c,dreadf
	lxi	d,subfcb
	call	fbdos	;read record
;	now print the buffer with cr,lf
	lxi	h,combuf
	mov	e,m	;length
	mvi	d,0	;high order 00
	dad	d	;to last character position
	inx	h
	mvi	m,cr
	inx	h
	mvi	m,lf
	inx	h
	mvi	m,'$'
	mvi	c,pbuff
	lxi	d,combuf+1
	call	rbdos	;to print it
	pop	h	;.max length
	lxi	d,combuf
	ldax	d	;how long?
	cmp	m	;cy if ok
	jc	movlin
	mov	a,m	;max length
	stax	d	;truncate length
movlin:
	mov	c,a	;length to c
	inr	c	;+1
	inx	h	;to length of line
rdloop:
	ldax	d	;next char
	mov	m,a
	inx	h
	inx	d
	dcr	c
	jnz	rdloop	;loop til copied
	mvi	c,closef
	lxi	d,subfcb
	lxi	h,modnum
	dad	d	;hl=fcb(modnum)
	mvi	m,0	;=0 so acts as if written
	lda	subcr	;length of file
	dcr	a	;incremented by read op
	sta	subrc	;decrease file length
	ora	a	;at zero?
	jnz	fileop
	mvi	c,delf	;delete if at end
fileop:	call	fbdos
	ret
restor:
	lhld	savboot
	shld	wboot+1
	jmp	rbdos
;
subfcb:
	db	1	;a:
	db	'$$$     '
	db	'SUB'
	db	0,0,0
subrc:
	ds	1
	ds	16	;map
subcr:	ds	1
;
combuf:	ds	131
udma:	dw	dbuff
ccpret:	ds	2	;ccp return address
	ds	32	;16 level stack
stack:
	end
