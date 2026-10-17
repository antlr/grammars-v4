;*****************************************************
;*                                                   *
;*      Sector Deblocking Algorithms for CP/M 2.0    *
;*                                                   *
;*****************************************************
;
;	utility macro to compute sector mask
smask	macro	hblk
;;	compute log2(hblk), return @x as result
;;	(2 ** @x = hblk on return)
@y	set	hblk
@x	set	0
;;	count right shifts of @y until = 1
	rept	8
	if	@y = 1
	exitm
	endif
;;	@y is not 1, shift right one position
@y	set	@y shr 1
@x	set	@x + 1
	endm
	endm
;
;*****************************************************
;*                                                   *
;*         CP/M to host disk constants               *
;*                                                   *
;*****************************************************
blksiz	equ	2048		;CP/M allocation size
hstsiz	equ	512		;host disk sector size
hstspt	equ	20		;host disk sectors/trk
hstblk	equ	hstsiz/128	;CP/M sects/host buff
cpmspt	equ	hstblk * hstspt	;CP/M sectors/track
secmsk	equ	hstblk-1	;sector mask
	smask	hstblk		;compute sector mask
secshf	equ	@x		;log2(hstblk)
;
;*****************************************************
;*                                                   *
;*        BDOS constants on entry to write           *
;*                                                   *
;*****************************************************
wrall	equ	0		;write to allocated
wrdir	equ	1		;write to directory
wrual	equ	2		;write to unallocated
;
;*****************************************************
;*                                                   *
;*	The BDOS entry points given below show the   *
;*      code which is relevant to deblocking only.   *
;*                                                   *
;*****************************************************
;
;	DISKDEF macro, or hand coded tables go here
dpbase	equ	$		;disk param block base
;
boot:
wboot:
	;enter here on system boot to initialize
	xra	a		;0 to accumulator
	sta	hstact		;host buffer inactive
	sta	unacnt		;clear unalloc count
	ret
;
home:
	;home the selected disk
home:
	lda	hstwrt	;check for pending write
	ora	a
	jnz	homed
	sta	hstact	;clear host active flag
homed:
	ret
;
seldsk:
	;select disk
	mov	a,c		;selected disk number
	sta	sekdsk		;seek disk number
	mov	l,a		;disk number to HL
	mvi	h,0
	rept	4		;multiply by 16
	dad	h
	endm
	lxi	d,dpbase	;base of parm block
	dad	d		;hl=.dpb(curdsk)
	ret
;
settrk:
	;set track given by registers BC
	mov	h,b
	mov	l,c
	shld	sektrk		;track to seek
	ret
;
setsec:
	;set sector given by register c 
	mov	a,c
	sta	seksec		;sector to seek
	ret
;
setdma:
	;set dma address given by BC
	mov	h,b
	mov	l,c
	shld	dmaadr
	ret
;
sectran:
	;translate sector number BC
	mov	h,b
	mov	l,c
	ret
;
;*****************************************************
;*                                                   *
;*	The READ entry point takes the place of      *
;*	the previous BIOS defintion for READ.        *
;*                                                   *
;*****************************************************
read:
	;read the selected CP/M sector
	xra	a
	sta	unacnt
	mvi	a,1
	sta	readop		;read operation
	sta	rsflag		;must read data
	mvi	a,wrual
	sta	wrtype		;treat as unalloc
	jmp	rwoper		;to perform the read
;
;*****************************************************
;*                                                   *
;*	The WRITE entry point takes the place of     *
;*	the previous BIOS defintion for WRITE.       *
;*                                                   *
;*****************************************************
write:
	;write the selected CP/M sector
	xra	a		;0 to accumulator
	sta	readop		;not a read operation
	mov	a,c		;write type in c
	sta	wrtype
	cpi	wrual		;write unallocated?
	jnz	chkuna		;check for unalloc
;
;	write to unallocated, set parameters
	mvi	a,blksiz/128	;next unalloc recs
	sta	unacnt
	lda	sekdsk		;disk to seek
	sta	unadsk		;unadsk = sekdsk
	lhld	sektrk
	shld	unatrk		;unatrk = sectrk
	lda	seksec
	sta	unasec		;unasec = seksec
;
chkuna:
	;check for write to unallocated sector
	lda	unacnt		;any unalloc remain?
	ora	a
	jz	alloc		;skip if not
;
;	more unallocated records remain
	dcr	a		;unacnt = unacnt-1
	sta	unacnt
	lda	sekdsk		;same disk?
	lxi	h,unadsk
	cmp	m		;sekdsk = unadsk?
	jnz	alloc		;skip if not
;
;	disks are the same
	lxi	h,unatrk
	call	sektrkcmp	;sektrk = unatrk?
	jnz	alloc		;skip if not
;
;	tracks are the same
	lda	seksec		;same sector?
	lxi	h,unasec
	cmp	m		;seksec = unasec?
	jnz	alloc		;skip if not
;
;	match, move to next sector for future ref
	inr	m		;unasec = unasec+1
	mov	a,m		;end of track?
	cpi	cpmspt		;count CP/M sectors
	jc	noovf		;skip if no overflow
;
;	overflow to next track
	mvi	m,0		;unasec = 0
	lhld	unatrk
	inx	h
	shld	unatrk		;unatrk = unatrk+1
;
noovf:
	;match found, mark as unnecessary read
	xra	a		;0 to accumulator
	sta	rsflag		;rsflag = 0
	jmp	rwoper		;to perform the write
;
alloc:
	;not an unallocated record, requires pre-read
	xra	a		;0 to accum
	sta	unacnt		;unacnt = 0
	inr	a		;1 to accum
	sta	rsflag		;rsflag = 1
;
;*****************************************************
;*                                                   *
;*	Common code for READ and WRITE follows       *
;*                                                   *
;*****************************************************
rwoper:
	;enter here to perform the read/write
	xra	a		;zero to accum
	sta	erflag		;no errors (yet)
	lda	seksec		;compute host sector
	rept	secshf
	ora	a		;carry = 0
	rar			;shift right
	endm
	sta	sekhst		;host sector to seek
;
;	active host sector?
	lxi	h,hstact	;host active flag
	mov	a,m
	mvi	m,1		;always becomes 1
	ora	a		;was it already?
	jz	filhst		;fill host if not
;
;	host buffer active, same as seek buffer?
	lda	sekdsk
	lxi	h,hstdsk	;same disk?
	cmp	m		;sekdsk = hstdsk?
	jnz	nomatch
;
;	same disk, same track?
	lxi	h,hsttrk
	call	sektrkcmp	;sektrk = hsttrk?
	jnz	nomatch
;
;	same disk, same track, same buffer?
	lda	sekhst
	lxi	h,hstsec	;sekhst = hstsec?
	cmp	m
	jz	match		;skip if match
;
nomatch:
	;proper disk, but not correct sector
	lda	hstwrt		;host written?
	ora	a
	cnz	writehst	;clear host buff
;
filhst:
	;may have to fill the host buffer
	lda	sekdsk
	sta	hstdsk
	lhld	sektrk
	shld	hsttrk
	lda	sekhst
	sta	hstsec
	lda	rsflag		;need to read?
	ora	a
	cnz	readhst		;yes, if 1
	xra	a		;0 to accum
	sta	hstwrt		;no pending write
;
match:
	;copy data to or from buffer
	lda	seksec		;mask buffer number
	ani	secmsk		;least signif bits
	mov	l,a		;ready to shift
	mvi	h,0		;double count
	rept	7		;shift left 7
	dad	h
	endm
;	hl has relative host buffer address
	lxi	d,hstbuf
	dad	d		;hl = host address
	xchg			;now in DE
	lhld	dmaadr		;get/put CP/M data
	mvi	c,128		;length of move
	lda	readop		;which way?
	ora	a
	jnz	rwmove		;skip if read
;
;	write operation, mark and switch direction
	mvi	a,1
	sta	hstwrt		;hstwrt = 1
	xchg			;source/dest swap
;
rwmove:
	;C initially 128, DE is source, HL is dest
	ldax	d		;source character
	inx	d
	mov	m,a		;to dest
	inx	h
	dcr	c		;loop 128 times
	jnz	rwmove
;
;	data has been moved to/from host buffer
	lda	wrtype		;write type
	cpi	wrdir		;to directory?
	lda	erflag		;in case of errors
	rnz			;no further processing
;
;	clear host buffer for directory write
	ora	a		;errors?
	rnz			;skip if so
	xra	a		;0 to accum
	sta	hstwrt		;buffer written
	call	writehst
	lda	erflag
	ret
;
;*****************************************************
;*                                                   *
;*	Utility subroutine for 16-bit compare        *
;*                                                   *
;*****************************************************
sektrkcmp:
	;HL = .unatrk or .hsttrk, compare with sektrk
	xchg
	lxi	h,sektrk
	ldax	d		;low byte compare
	cmp	m		;same?
	rnz			;return if not
;	low bytes equal, test high 1s
	inx	d
	inx	h
	ldax	d
	cmp	m	;sets flags
	ret
;
;*****************************************************
;*                                                   *
;*	WRITEHST performs the physical write to      *
;*	the host disk, READHST reads the physical    *
;*	disk.					     *
;*                                                   *
;*****************************************************
writehst:
	;hstdsk = host disk #, hsttrk = host track #,
	;hstsec = host sect #. write "hstsiz" bytes
	;from hstbuf and return error flag in erflag.
	;return erflag non-zero if error
	ret
;
readhst:
	;hstdsk = host disk #, hsttrk = host track #,
	;hstsec = host sect #. read "hstsiz" bytes
	;into hstbuf and return error flag in erflag.
	ret
;
;*****************************************************
;*                                                   *
;*	Unitialized RAM data areas		     *
;*                                                   *
;*****************************************************
;
sekdsk:	ds	1		;seek disk number
sektrk:	ds	2		;seek track number
seksec:	ds	1		;seek sector number
;
hstdsk:	ds	1		;host disk number
hsttrk:	ds	2		;host track number
hstsec:	ds	1		;host sector number
;
sekhst:	ds	1		;seek shr secshf
hstact:	ds	1		;host active flag
hstwrt:	ds	1		;host written flag
;
unacnt:	ds	1		;unalloc rec cnt
unadsk:	ds	1		;last unalloc disk
unatrk:	ds	2		;last unalloc track
unasec:	ds	1		;last unalloc sector
;
erflag:	ds	1		;error reporting
rsflag:	ds	1		;read sector flag
readop:	ds	1		;1 if read operation
wrtype:	ds	1		;write operation type
dmaadr:	ds	2		;last dma address
hstbuf:	ds	hstsiz		;host buffer
;
;*****************************************************
;*                                                   *
;*	The ENDEF macro invocation goes here	     *
;*                                                   *
;*****************************************************
	end
