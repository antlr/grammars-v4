	name	'BIOSMOD'
	title	'Direct BIOS Calls From PL/I-80 for CP/M 3.0'
;
;***********************************************************
;*                                                         *
;*	bios calls from pl/i for track, sector io          *
;*                                                         *
;***********************************************************
	public	settrk	;set track number
	public	setsec	;set sector number
	public	rdsec	;read sector
	public	wrsec	;write sector
	public	seldsk	;select disk & return the addr(DPH)
	public	sectrn	;translate sector # given translate table
	public  bstdma	;set dma
;
;
	extrn	?boot	;system reboot entry point
	extrn	?bdos	;bdos entry point
;
;	utility functions
;
;***********************************************************
;***********************************************************
;*                                                         *
;*       general purpose routines used upon entry          *
;*                                                         *
;***********************************************************
;
;
getp2:	;get single word value to DE
	mov 	e,m
	inx 	h
	mov	d,m
	inx	h
	push 	h
	xchg
	mov	e,m
	inx	h
	mov 	d,m
	pop	h
	ret
;
;
;***********************************************************
;*                                                         *
;***********************************************************
settrk:	;set track number 0-76, 0-65535 in BC
	;1-> track #
	call getp2
	xchg
	shld	BCREG
	mvi 	a,0ah
	jmp gobios
;
;***********************************************************
;*                                                         *
;***********************************************************
setsec:	;set sector number 1 - sectors per track
	;1-> sector #
	call getp2
	xchg
	shld	BCREG
	mvi	a,0bh
	jmp gobios
;
;***********************************************************
;*                                                         *
;***********************************************************
rdsec:	;read current sector into sector at dma addr
	;returns 	0 if no errors
	;		1 non-recoverable error
	mvi	a,0dh
	jmp gobios
;***********************************************************
;*                                                         *
;***********************************************************
wrsec:	;writes contents of sector at dma addr to current sector
	;returns	0 errors occured
	;		1 non-recoverable error
	call getp2
	xchg
	shld	BCREG
	mvi	a,0eh
	jmp gobios
;
;***********************************************************
;*                                                         *
;***********************************************************
;
seldsk:	; selects disk

	call getp2
	mov	a,e
	sta 	BCREG
	mvi	a,9
	jmp 	gobios
;
;***********************************************************
;*                                                         *
;***********************************************************
;
sectrn:	;translate sector #
	call	getp2
	xchg
	shld	BCREG
	xchg
	call 	getp2
	xchg
	shld	DEREG
	mvi	a,10h
	jmp	gobios
;
bstdma:	;set dma
	call	getp2
	xchg
	shld	BCREG
	mvi	a,0ch
;	jmp	gobios
;
;***********************************************************
;***********************************************************
;***********************************************************
;*                                                         *
;*       call BDOS					   *
;*                                                         *
;***********************************************************
;
;
gobios:
	sta	FUNC	;load BIOS function #
	lxi	h,FUNC
	xchg		; address of BIOSPB in DE
	mvi 	c,032h	; BDOS function 50 call
	jmp	?bdos
;
;
BIOSPB:		dw	FUNC
FUNC:		db	0
AREG:		db	0
BCREG:		dw	0
DEREG:		dw	0
HLREG:		dw	0
;
	end

