	title	'CP/M V3.0 Loader'


;	Copyright (C) 1982
;	Digital Research
;	Box 579, Pacific Grove
;	California, 93950

;  Revised:
;    01 Nov 82  by Bruce Skidmore

base	equ	$
abase	equ	base-0100h

cr	equ	0dh
lf	equ	0ah

fcb	equ	abase+005ch	;default FCB address
buff	equ	abase+0080h	;default buffer address

;
;	System Equates
;
resetsys	equ	13	;reset disk system
printbuf	equ	09	;print string
open$func	equ	15	;open function
read$func	equ	20	;read sequential
setdma$func	equ	26	;set dma address
;
;	Loader Equates
;
comtop	equ	abase+80h
comlen	equ	abase+81h
bnktop	equ	abase+82h
bnklen	equ	abase+83h
osentry	equ	abase+84h

	cseg

	lxi	sp,stackbot

	call	bootf		;first call is to Cold Boot

	mvi	c,resetsys	;Initialize the System
	call	bdos

	mvi	c,printbuf	;print the sign on message
	lxi	d,signon
	call	bdos

	mvi	c,open$func	;open the CPM3.SYS file
	lxi	d,cpmfcb
	call	bdos
	cpi	0ffh
	lxi	d,openerr
	jz	error

	lxi	d,buff
	call	setdma$proc

	call	read$proc	;read the load record

	lxi	h,buff
	lxi	d,mem$top
	mvi	c,6
cloop:
	mov	a,m
	stax	d
	inx	d
	inx	h
	dcr	c
	jnz	cloop
	
	call	read$proc	;read display info

	mvi	c,printbuf	;print the info
	lxi	d,buff
	call	bdos
	
;
;	Main System Load
;

;
;	Load Common Portion of System
;
	lda	res$len
	mov	h,a
	lda	mem$top
	call	load
;
;	Load Banked Portion of System
;
	lda	bank$len
	ora	a
	jz	execute
	mov	h,a
	lda	bank$top
	call	load
;
;	Execute System
;
execute:
	lxi	h,fcb+1
	mov	a,m
	cpi	'$'
	jnz	execute$sys
	inx	h
	mov	a,m
	cpi	'B'
	cz	break
execute$sys:
	lxi	sp,osentry$adr
	ret

;
;	Load Routine
;
;	Input:   A = Page Address of load top
;		 H = Length in pages of module to read
;
load:
	ora	a	;clear carry
	mov	d,a
	mvi	e,0
	mov	a,h
	ral
	mov	h,a	;h = length in records of module
loop:
	xchg
	lxi	b,-128
	dad	b	;decrement dma address by 128
	xchg
	push	d
	push	h
	call	setdma$proc
	call	read$proc
	pop	h
	pop	d
	dcr	h
	jnz	loop
	ret

;
;	Set DMA Routine
;
setdma$proc:
	mvi	c,setdma$func
	call	bdos
	ret

;
;	Read Routine
;
read$proc:
	mvi	c,read$func	;Read the load record
	lxi	d,cpmfcb	;into address 80h
	call	bdos
	ora	a
	lxi	d,readerr
	rz
;
;	Error Routine
;
error:
	mvi	c,printbuf	;print error message
	call	bdos
	di
	hlt

break:
	db	0ffh
	ret

cpmfcb:
	db	0,'CPM3    SYS',0,0,0,0,0,0
	dw	0,0,0,0,0,0,0,0,0

openerr:
	db	cr,lf
	db	'CPMLDR error:  failed to open CPM3.SYS'
	db	cr,lf,'$'

readerr:
	db	cr,lf
	db	'CPMLDR error:  failed to read CPM3.SYS'
	db	cr,lf,'$'

signon:
	db	cr
	db	lf,lf,lf,lf,lf,lf,lf,lf,lf,lf,lf,lf
	db	lf,lf,lf,lf,lf,lf,lf,lf,lf,lf,lf,lf
	db	'CP/M V3.0 Loader',cr,lf
	db	'Copyright (C) 1982, Digital Research'
	db	cr,lf,'$'

	db	'021182',0,0,0,0
stackbot:

mem$top:
	ds	1
res$len:
	ds	1
bank$top:
	ds	1
bank$len:
	ds	1
osentry$adr:
	ds	2

;	title	'CP/M 3.0 LDRBDOS Interface, Version 3.1 Nov, 1982'
;*****************************************************************
;*****************************************************************
;**                                                             **
;**   B a s i c    D i s k   O p e r a t i n g   S y s t e m    **
;**								**
;**            I n t e r f a c e   M o d u l e                  **
;**                                                             **
;*****************************************************************
;*****************************************************************
;
;	Copyright (c) 1978, 1979, 1980, 1981, 1982
;	Digital Research
;	Box 579, Pacific Grove
;	California
;
;       Nov 1982
;
;
;	equates for non graphic characters
;

rubout	equ	7fh	; char delete
tab	equ	09h	; tab char
cr	equ	0dh	; carriage return
lf	equ	0ah	; line feed
ctlh	equ	08h	; backspace


;
serial: db	0,0,0,0,0,0
;
;	Enter here from the user's program with function number in c,
;	and information address in d,e
;

bdos:
bdose:					; Arrive here from user programs
	xchg! shld info! xchg 		; info=de, de=info

	mov a,c! cpi 14! jc bdose2
	sta fx 				; Save disk function #
	xra a! sta dircnt 
	lda seldsk! sta olddsk 		; Save seldsk

bdose2:
	mov a,e! sta linfo 		; linfo = low(info) - don't equ
	lxi h,0! shld aret 		; Return value defaults to 0000
	shld resel 			; resel = 0
				; Save user's stack pointer, set to local stack
	dad sp! shld entsp 		; entsp = stackptr

	lxi sp,lstack 			; local stack setup

	lxi h,goback 			; Return here after all functions
	push h 				; jmp goback equivalent to ret
	mov a,c! cpi nfuncs! jnc high$fxs ; Skip if invalid #
	mov c,e 			; possible output character to c
	lxi h,functab! jmp bdos$jmp

	; look for functions 100 ->
high$fxs:
	sbi 100! jc lret$eq$ff 		; Skip if function < 100

bdos$jmp:

	mov e,a! mvi d,0 		; de=func, hl=.ciotab
	dad d! dad d! mov e,m! inx h! mov d,m ; de=functab(func)
	lhld info 			; info in de for later xchg	
	xchg! pchl 			; dispatched


;	dispatch table for functions

functab:
	dw	func$ret, func1, func2, func3
	dw	func$ret, func$ret, func6, func$ret
	dw	func$ret, func9, func10, func11
diskf	equ	($-functab)/2	; disk funcs
	dw	func12,func13,func14,func15
	dw	func16,func17,func18,func19
	dw	func20,func21,func22,func23
	dw	func24,func25,func26,func27
	dw	func28,func29,func30,func31
	dw	func32,func33,func34,func35
	dw	func36,func37,func38,func39
	dw	func40,func42,func43
	dw	func44,func45,func46,func47
	dw	func48,func49,func50
nfuncs	equ	($-functab)/2


entsp:	ds	2	; entry stack pointer

	;	40 level stack

	dw	0c7c7h,0c7c7h,0c7c7h,0c7c7h,0c7c7h,0c7c7h,0c7c7h,0c7c7h
	dw	0c7c7h,0c7c7h,0c7c7h,0c7c7h,0c7c7h,0c7c7h,0c7c7h,0c7c7h
	dw	0c7c7h,0c7c7h,0c7c7h,0c7c7h,0c7c7h,0c7c7h,0c7c7h,0c7c7h
	dw	0c7c7h,0c7c7h,0c7c7h,0c7c7h,0c7c7h,0c7c7h,0c7c7h,0c7c7h
	dw	0c7c7h,0c7c7h,0c7c7h,0c7c7h,0c7c7h,0c7c7h,0c7c7h,0c7c7h
lstack:


page
	title	'CP/M 3.0 LDRBDOS Interface, Version 3.1 July, 1982'
;*****************************************************************
;*****************************************************************
;**                                                             **
;**   B a s i c    D i s k   O p e r a t i n g   S y s t e m    **
;**								**
;**               C o n s o l e   P o r t i o n                 **
;**                                                             **
;*****************************************************************
;*****************************************************************
;
;       July, 1982
;
;
;	console handlers

conout:
			;compute character position/write console char from C
			;compcol = true if computing column position
	lda compcol! ora a! jnz compout
			;write the character, then compute the column
			;write console character from C
	push b 				;recall/save character
	call conoutf 			;externally, to console
	pop b 				;recall the character
  compout:
		mov a,c 		;recall the character
					;and compute column position
		lxi h,column 		;A = char, HL = .column
		cpi rubout! rz 		;no column change if nulls
		inr m 			;column = column + 1
		cpi ' '! rnc 		;return if graphic
					;not graphic, reset column position
		dcr m 			;column = column - 1
		mov a,m! ora a! rz 	;return if at zero
					;not at zero, may be backspace or eol
		mov a,c 		;character back to A
		cpi ctlh! jnz notbacksp
					;backspace character
		dcr m 			;column = column - 1
		ret

  notbacksp:
					;not a backspace character, eol?
		cpi lf! rnz 		;return if not
					;end of line, column = 0
		mvi m,0 		;column = 0
		ret
;
;
tabout:
					;expand tabs to console
	mov a,c! cpi tab! jnz conout 	;direct to conout if not
					;tab encountered, move to next tab pos
  tab0:
	mvi c,' '! call conout 		;another blank
	lda column! ani 111b 		;column mod 8 = 0 ?
	jnz tab0 			;back for another if not
	ret
;
print:
					;print message until M(BC) = '$'
	LXI H,OUTDELIM
	ldax b! CMP M! rz 		;stop on $
					;more to print
	inx b! push b! mov c,a 		;char to C
	call tabout 			;another character printed
	pop b! jmp print
;
;
func2:	equ	tabout
			;write console character with tab expansion
;
func9:
					;write line until $ encountered
	xchg				;was lhld info	
	mov c,l! mov b,h 		;BC=string address
	jmp print 			;out to console	
;
sta$ret:
					;store the A register to aret
	sta aret
func$ret:
	ret 			;jmp goback (pop stack for non cp/m functions)
;
setlret1:
					;set lret = 1
	mvi a,1! jmp sta$ret
;
func1:	equ 	func$ret
;
func3:	equ 	func$ret
;
func6:	equ 	func$ret
;
func10:	equ	func$ret
func11:	equ	func$ret
;
;	data areas
;


compcol:db	0	;true if computing column position
;	end of BDOS Console module

;**********************************************************************
;*****************************************************************
;
;	Error Messages

md	equ	24h

err$msg:	db	cr,lf,'BDOS ERR: ',md
err$select:	db	'Select',md
err$phys:	db	'Perm.',md

;*****************************************************************
;*****************************************************************
;
;	common values shared between bdosi and bdos


aret:	ds	2	; address value to return
lret	equ	aret	; low(aret)

;*****************************************************************
;*****************************************************************
;**                                                             **
;**   b a s i c    d i s k   o p e r a t i n g   s y s t e m    **
;**                                                             **
;*****************************************************************
;*****************************************************************

;	literal constants

true	equ	0ffh	; constant true
false	equ	000h	; constant false
enddir	equ	0ffffh	; end of directory
byte	equ	1	; number of bytes for "byte" type
word	equ	2	; number of bytes for "word" type

;	fixed addresses in low memory

tbuff	equ	0080h	; default buffer location

;	error message handlers

sel$error:
				; report select error
	lxi b,err$msg
	call print
	lxi b,err$select
	jmp goerr1

goerr:
	lxi b,err$msg
	call print
	lxi b,err$phys
goerr1:
	call print
	di ! hlt

bde$e$bde$m$hl:
	mov a,e! sub l! mov e,a
	mov a,d! sbb h! mov d,a
	rnc! dcr b! ret

bde$e$bde$p$hl:
	mov a,e! add l! mov e,a
	mov a,d! adc h! mov d,a
	rnc! inr b! ret

shl3bv:
	inr c
shl3bv1:
	dcr c! rz
	dad h! adc a! jmp shl3bv1

compare:
	ldax d! cmp m! rnz
	inx h! inx d! dcr c! rz
	jmp compare

;
;	local subroutines for bios interface
;

move:
	; Move data length of length c from source de to
	; destination given by hl
	inr c ; in case it is zero
	move0:
		dcr c! rz ; more to move
		ldax d! mov m,a ; one byte moved
		inx d! inx h ; to next byte
		jmp move0

selectdisk:
			; Select the disk drive given by register D, and fill
			; the base addresses curtrka - alloca, then fill
			; the values of the disk parameter block
	mov c,d 			; current disk# to c
					; lsb of e = 0 if not yet logged - in
	call seldskf 			; hl filled by call
				; hl = 0000 if error, otherwise disk headers
	mov a,h! ora l! rz 	; Return with C flag reset if select error
					; Disk header block address in hl
	mov e,m! inx h! mov d,m! inx h ; de=.tran
	inx h ! inx h
	shld curtrka! inx h! inx h ; hl=.currec
	shld curreca! inx h! inx h ; hl=.buffa
	inx h! inx h
	inx h! inx h
					; de still contains .tran
	xchg! shld tranv 		; .tran vector
	lxi h,dpbaddr 			; de= source for move, hl=dest
	mvi c,addlist! call move 	; addlist filled
					; Now fill the disk parameter block
	lhld dpbaddr! xchg 		; de is source
	lxi h,sectpt 			; hl is destination
	mvi c,dpblist! call move 	; data filled
					; Now set single/double map mode
	lhld maxall 			; largest allocation number
	mov a,h 			; 00 indicates < 255
	lxi h,single! mvi m,true 	; Assume a=00
	ora a! jz retselect
				; high order of maxall not zero, use double dm
	mvi m,false
  retselect:
				; C flag set indicates successful select
		stc
		ret

home:
			; Move to home position, then offset to start of dir
	call homef
	xra a 				; constant zero to accumulator
	lhld curtrka! mov m,a! inx h! mov m,a ; curtrk=0000
	lhld curreca! mov m,a! inx h! mov m,a ; currec=0000
	inx h! mov m,a 			; currec high byte=00

	ret

pass$arecord:
	lxi h,arecord
	mov e,m! inx h! mov d,m! inx h! mov b,m
	ret

rdbuff:
			; Read buffer and check condition
	call pass$arecord
	call readf 			; current drive, track, sector, dma


diocomp: 		; Check for disk errors
	ora a! rz
	mov c,a
	cpi 3! jc goerr
	mvi c,1! jmp goerr

seekdir:
			; Seek the record containing the current dir entry

	lhld dcnt 			; directory counter to hl
	mvi c,dskshf! call hlrotr 	; value to hl

	mvi b,0! xchg

	lxi h,arecord
	mov m,e! inx h! mov m,d! inx h! mov m,b
	ret

seek:
			; Seek the track given by arecord (actual record)

	lhld curtrka! mov c,m! inx h! mov b,m 	; bc = curtrk
	push b 					; s0 = curtrk 
	lhld curreca! mov e,m! inx h! mov d,m
	inx h! mov b,m 				; bde = currec
	lhld arecord! lda arecord+2! mov c,a 	; chl = arecord
seek0:
	mov a,l! sub e! mov a,h! sbb d! mov a,c! sbb b
	push h 					; Save low(arecord)
	jnc seek1 			; if arecord >= currec then go to seek1
	lhld sectpt! call bde$e$bde$m$hl 	; currec = currec - sectpt
	pop h! xthl! dcx h! xthl 		; curtrk = curtrk - 1
	jmp seek0
seek1:
	lhld sectpt! call bde$e$bde$p$hl 	; currec = currec + sectpt
	pop h 					; Restore low(arecord)
	mov a,l! sub e! mov a,h! sbb d! mov a,c! sbb b
	jc seek2 			; if arecord < currec then go to seek2
	xthl! inx h! xthl 			; curtrk = curtrk + 1
	push h 					; save low (arecord)
	jmp seek1
seek2:
	xthl! push h 			; hl,s0 = curtrk, s1 = low(arecord)
	lhld sectpt! call bde$e$bde$m$hl 	; currec = currec - sectpt
	pop h! push d! push b! push h 		; hl,s0 = curtrk, 
			; s1 = high(arecord,currec), s2 = low(currec), 
			; s3 = low(arecord)
	xchg! lhld offset! dad d
	mov b,h! mov c,l! shld track
	call settrkf 				; call bios settrk routine
						; Store curtrk
	pop d! lhld curtrka! mov m,e! inx h! mov m,d
						; Store currec
	pop b! pop d!
	lhld curreca! mov m,e! inx h! mov m,d
	inx h! mov m,b 				; currec = bde
	pop b 				; bc = low(arecord), de = low(currec)
	mov a,c! sub e! mov l,a 		; hl = bc - de
	mov a,b! sbb d! mov h,a
	call shr$physhf
	mov b,h! mov c,l

	lhld tranv! xchg 			; bc=sector#, de=.tran
	call sectran 				; hl = tran(sector)
	mov c,l! mov b,h 			; bc = tran(sector)
	shld sector
	call setsecf 				; sector selected
	lhld curdma! mov c,l! mov b,h! jmp setdmaf

shr$physhf:
	lda physhf! mov c,a! jmp hlrotr


;	file control block (fcb) constants

empty	equ	0e5h	; empty directory entry
recsiz	equ	128	; record size
fcblen	equ	32	; file control block size
dirrec	equ	recsiz/fcblen	; directory fcbs / record
dskshf	equ	2	; log2(dirrec)
dskmsk	equ	dirrec-1
fcbshf	equ	5	; log2(fcblen)

extnum	equ	12	; extent number field
maxext	equ	31	; largest extent number
ubytes	equ	13	; unfilled bytes field

namlen	equ	15	; name length
reccnt	equ	15	; record count field
dskmap	equ	16	; disk map field
nxtrec	equ	fcblen

;	utility functions for file access

dm$position:
	; Compute disk map position for vrecord to hl
	lxi h,blkshf! mov c,m ; shift count to c
	lda vrecord ; current virtual record to a
	dmpos0:
		ora a! rar! dcr c! jnz dmpos0
	; a = shr(vrecord,blkshf) = vrecord/2**(sect/block)
	mov b,a ; Save it for later addition
	mvi a,8! sub m ; 8-blkshf to accumulator
	mov c,a ; extent shift count in register c
	lda extval ; extent value ani extmsk
	dmpos1:
		; blkshf = 3,4,5,6,7, c=5,4,3,2,1
		; shift is 4,3,2,1,0
		dcr c! jz dmpos2
		ora a! ral! jmp dmpos1
	dmpos2:
	; Arrive here with a = shl(ext and extmsk,7-blkshf)
	add b ; Add the previous shr(vrecord,blkshf) value
	; a is one of the following values, depending upon alloc
	; bks blkshf
	; 1k   3     v/8 + extval * 16
	; 2k   4     v/16+ extval * 8
	; 4k   5     v/32+ extval * 4
	; 8k   6     v/64+ extval * 2
	; 16k  7     v/128+extval * 1
	ret ; with dm$position in a

getdma:
	lhld info! lxi d,dskmap! dad d! ret

getdm:
	; Return disk map value from position given by bc
	call getdma
	dad b ; Index by a single byte value
	lda single ; single byte/map entry?
	ora a! jz getdmd ; Get disk map single byte
		mov l,m! mov h,b! ret ; with hl=00bb
	getdmd:
		dad b ; hl=.fcb(dm+i*2)
		; double precision value returned
		mov a,m! inx h! mov h,m! mov l,a! ret

index:
	; Compute disk block number from current fcb
	call dm$position ; 0...15 in register a
	sta dminx
	mov c,a! mvi b,0! call getdm ; value to hl
	shld arecord! mov a,l! ora h! ret

atran:
	; Compute actual record address, assuming index called

;	arecord = shl(arecord,blkshf)

	lda blkshf! mov c,a
	lhld arecord! xra a! call shl3bv
	shld arecord! sta arecord+2

	shld arecord1 ; Save low(arecord)

;	arecord = arecord or (vrecord and blkmsk)

	lda blkmsk! mov c,a! lda vrecord! ana c
	mov b,a ; Save vrecord & blkmsk in reg b & blk$off
	sta blk$off
	lxi h,arecord! ora m! mov m,a! ret


getexta:
	; Get current extent field address to hl
	lhld info! lxi d,extnum! dad d ; hl=.fcb(extnum)
	ret

getrcnta:
	; Get reccnt address to hl
	lhld info! lxi d,reccnt! dad d! ret

getfcba:
	; Compute reccnt and nxtrec addresses for get/setfcb
	call getrcnta! xchg ; de=.fcb(reccnt)
	lxi h,(nxtrec-reccnt)! dad d ; hl=.fcb(nxtrec) 
	ret

getfcb:
	; Set variables from currently addressed fcb
	call getfcba ; addresses in de, hl
	mov a,m! sta vrecord ; vrecord=fcb(nxtrec)
	xchg! mov a,m! sta rcount ; rcount=fcb(reccnt)
	call getexta ; hl=.fcb(extnum)
	lda extmsk ; extent mask to a
	ana m ; fcb(extnum) and extmsk
	sta extval
	ret

setfcb:
					; Place values back into current fcb
	call getfcba 			; addresses to de, hl
	mvi c,1

	lda vrecord! add c! mov m,a 	; fcb(nxtrec)=vrecord+seqio
	xchg! lda rcount! mov m,a 	; fcb(reccnt)=rcount
	ret

hlrotr:
					; hl rotate right by amount c
	inr c 				; in case zero
	hlrotr0: dcr c! rz 		; return when zero

	mov a,h! ora a! rar! mov h,a 	; high byte
	mov a,l! rar! mov l,a 		; low byte
	jmp hlrotr0

hlrotl:
				 	; Rotate the mask in hl by amount in c
 	inr c 				; may be zero
 	hlrotl0: dcr c! rz 		; return if zero

	dad h! jmp hlrotl0

set$cdisk:
				; Set a "1" value in curdsk position of bc
	lda seldsk
	push b 				; Save input parameter
	mov c,a 			; Ready parameter for shift
	lxi h,1 			; number to shift
	call hlrotl 			; hl = mask to integrate
	pop b 				; original mask
	mov a,c! ora l! mov l,a
	mov a,b! ora h! mov h,a 	; hl = mask or rol(1,curdsk)
	ret

test$vector:
	lda seldsk
	mov c,a! call hlrotr
	mov a,l! ani 1b! ret 		; non zero if curdsk bit on

getdptra:
			; Compute the address of a directory element at
			; positon dptr in the buffer

	lhld buffa! lda dptr
					; hl = hl + a
	add l! mov l,a! rnc
					; overflow to h
	inr h! ret

clr$ext:
			; fcb ext = fcb ext & 1fh

	call getexta! mov a,m! ani 0001$1111b! mov m,a!
	ret


subdh:
			; Compute hl = de - hl
	mov a,e! sub l! mov l,a! mov a,d! sbb h! mov h,a
	ret

get$buffa:
	push d! lxi d,10! dad d
	mov e,m! inx h! mov d,m
	xchg! pop d! ret


rddir:
			; Read a directory entry into the directory buffer
	call seek$dir
	lda phymsk! ora a! jz rddir1
	mvi a,3
	call deblock$dir! jmp setdata

rddir1:
	call setdir 				; directory dma
	shld buffa! call seek
	call rdbuff 				; directory record loaded

setdata:
			; Set data dma address
	lhld dmaad! jmp setdma 			; to complete the call

setdir:
			; Set directory dma address

	lhld dirbcba
	call get$buffa

setdma:
			; hl=.dma address to set (i.e., buffa or dmaad)
	shld curdma! ret

end$of$dir:
			; Return zero flag if at end of directory, non zero
			; if not at end (end of dir if dcnt = 0ffffh)
	lxi h,dcnt
	mov a,m 			; may be 0ffh
	inx h! cmp m 			; low(dcnt) = high(dcnt)?
	rnz 				; non zero returned if different
					; high and low the same, = 0ffh?
	inr a 				; 0ffh becomes 00 if so
	ret

set$end$dir:
			; Set dcnt to the end of the directory
	lxi h,enddir! shld dcnt! ret


read$dir:
		; Read next directory entry, with c=true if initializing

	lhld dirmax! xchg 		; in preparation for subtract
	lhld dcnt! inx h! shld dcnt 	; dcnt=dcnt+1

					; while(dirmax >= dcnt)
	call subdh 			; de-hl
	jc set$end$dir
				; not at end of directory, seek next element
				; initialization flag is in c

		lda dcnt! ani dskmsk 	; low(dcnt) and dskmsk
		mvi b,fcbshf 		; to multiply by fcb size

	read$dir1:
		add a! dcr b! jnz read$dir1
					; a = (low(dcnt) and dskmsk) shl fcbshf
		sta dptr 		; ready for next dir operation
		ora a! rnz 		; Return if not a new record

		push b 			; Save initialization flag c
		call rd$dir 		; Read the directory record
		pop b 			; Recall initialization flag
		ret
compext:
			; Compare extent# in a with that in c, return nonzero
			; if they do not match
	push b 				; Save c's original value
	push psw! lda extmsk! cma! mov b,a
					; b has negated form of extent mask
	mov a,c! ana b! mov c,a 	; low bits removed from c
	pop psw! ana b 			; low bits removed from a
	sub c! ani maxext 		; Set flags
	pop b 				; Restore original values
	ret

get$dir$ext:
			; Compute directory extent from fcb
			; Scan fcb disk map backwards
	call getfcba 	; hl = .fcb(vrecord)
	mvi c,16! mov b,c! inr c! push b
			; b=dskmap pos (rel to 0)
get$de0:
	pop b
	dcr c
	xra a 				; Compare to zero
get$de1:
	dcx h! dcr b			; Decr dskmap position
	cmp m! jnz get$de2 		; fcb(dskmap(b)) ~= 0
	dcr c! jnz get$de1
				; c = 0 -> all blocks = 0 in fcb disk map
get$de2:
	mov a,c! sta dminx
	lda single! ora a! mov a,b
	jnz get$de3
	rar 				; not single, divide blk idx by 2
get$de3:
	push b! push h 			; Save dskmap position & count
	mov l,a! mvi h,0 		; hl = non-zero blk idx
					; Compute ext offset from last non-zero
					; block index by shifting blk idx right
					; 7 - blkshf
	lda blkshf! mov d,a! mvi a,7! sub d
	mov c,a! call hlrotr! mov b,l
					; b = ext offset
	lda extmsk! cmp b! pop h! jc get$de0
				; Verify computed extent offset <= extmsk
	call getexta! mov c,m
	cma! ani maxext! ana c! ora b
		; dir ext = (fcb ext & (~ extmsk) & maxext) | ext offset
	pop b 				; Restore stack
	ret 				; a = directory extent


search:
			; Search for directory element of length c at info
	lhld info! shld searcha 	; searcha = info
	mov a,c! sta searchl 		; searchl = c

	call set$end$dir 		; dcnt = enddir
	call home 			; to start at the beginning

searchn:
			; Search for the next directory element, assuming
			; a previous call on search which sets searcha and
			; searchl

	mvi c,false! call read$dir 	; Read next dir element
	call end$of$dir! jz lret$eq$ff
					; not end of directory, scan for match
	lhld searcha! xchg 		; de=beginning of user fcb

		call getdptra 		; hl = buffa+dptr
		lda searchl! mov c,a 	; length of search to c
		mvi b,0 		; b counts up, c counts down

		mov a,m! cpi empty! jz searchn

  searchloop:
			mov a,c! ora a! jz endsearch
					; Scan next character if not ubytes
			mov a,b! cpi ubytes! jz searchok
					; not the ubytes field, extent field?
			cpi extnum 	; may be extent field
			jz searchext 	; Skip to search extent
			ldax d
			sub m! ani 7fh 	; Mask-out flags/extent modulus
			jnz searchn 	; Skip if not matched
			jmp searchok 	; matched character
		searchext:
			ldax d
					; Attempt an extent # match
			push b 		; Save counters
			mov c,m 	; directory character to c
			call compext 	; Compare user/dir char
			pop b 		; Recall counters
			ora a 		; Set flag
			jnz searchn 	; Skip if no match
		searchok:
					; current character matches
			inx d! inx h! inr b! dcr c
			jmp searchloop
		endsearch:
				; entire name matches, return dir position
			xra a
			sta lret 	; lret = 0
					; successful search -
					; return with zero flag reset
			mov b,a! inr b
			ret
		lret$eq$ff:
					; unsuccessful search -
					; return with zero flag set
					; lret,low(aret) = 0ffh
			mvi a,255 ! mov b,a ! inr b ! jmp sta$ret

open:
			; Search for the directory entry, copy to fcb
	mvi c,namlen! call search
	rz 				; Return with lret=255 if end

			; not end of directory, copy fcb information
open$copy:
	call getexta ! mov a,m ! push a	; save extent to check for extent
					; folding - move moves entire dir FCB
	call getdptra! xchg 		; hl = .buff(dptr)
	lhld info 			; hl=.fcb(0)
	mvi c,nxtrec 			; length of move operation
	call move 			; from .buff(dptr) to .fcb(0)

			; Note that entire fcb is copied, including indicators

	call get$dir$ext! mov c,a
	pop a ! mov m,a			; restore extent

		; hl = .user extent#, c = dir extent#
		; above move set fcb(reccnt) to dir(reccnt)
		; if fcb ext < dir ext then fcb(reccnt) = fcb(reccnt) | 128
		; if fcb ext = dir ext then fcb(reccnt) = fcb(reccnt)
		; if fcb ext > dir ext then fcb(reccnt) = 0

set$rc: 				; hl=.fcb(ext), c=dirext
	mvi b,0
	xchg! lxi h,(reccnt-extnum)! dad d
	ldax d! sub c! jz set$rc2
	mov a,b! jnc set$rc1
	mvi a,128! mov b,m

  set$rc1:
		mov m,a! mov a,b! sta actual$rc! ret 
  set$rc2:
		sta actual$rc
		mov a,m! ora a! rnz 	; ret if rc ~= 0
		lda dminx! ora a! rz 	; ret if no blks in fcb
		lda fx! cpi 15! rz 	; ret if fx = 15
		mvi m,128 		; rc = 128
		ret

restore$rc:
			; hl = .fcb(extnum)
			; if actual$rc ~= 0 then rcount = actual$rc
	push h
	lda actual$rc! ora a! jz restore$rc1
	lxi d,(reccnt-extnum)! dad d
	mov m,a! xra a! sta actual$rc

restore$rc1:
	pop h! ret

open$reel:
		; Close the current extent, and open the next one
		; if possible.

	call getexta
	mov a,m! mov c,a
	inr c! call compext
	jz open$reel3

	mvi a,maxext! ana c! mov m,a 		; Incr extent field
	mvi c,namlen! call search 		; Next extent found?
						; not end of file, open
	call open$copy

  open$reel2:
		call getfcb 			; Set parameters
		xra a! sta vrecord! jmp sta$ret ; lret = 0
  open$reel3:
		inr m 				; fcb(ex) = fcb(ex) + 1
		call get$dir$ext! mov c,a
						; Is new extent beyond dir$ext?
		cmp m! jnc open$reel4 		; no
		dcr m 				; fcb(ex) = fcb(ex) - 1
		jmp set$lret1
  open$reel4:
		call restore$rc
		call set$rc! jmp open$reel2

seqdiskread:
			; Sequential disk read operation
			; Read the next record from the current fcb

	call getfcb 				; sets parameters for the read

	lda vrecord! lxi h,rcount! cmp m 	; vrecord-rcount
						; Skip if rcount > vrecord
	jc recordok

				; not enough records in the extent
				; record count must be 128 to continue
		cpi 128 			; vrecord = 128?
		jnz setlret1 			; Skip if vrecord<>128
		call open$reel 			; Go to next extent if so
						; Check for open ok
		lda lret! ora a! jnz setlret1 	; Stop at eof

  recordok:
			; Arrive with fcb addressing a record to read

		call index 			; Z flag set if arecord = 0

		jz setlret1 			; Reading unwritten data

						; Record has been allocated
		call atran 			; arecord now a disk address

		lda phymsk! ora a		; if not 128 byte sectors
		jnz read$deblock		; go to deblock

		call setdata			; Set curdma = dmaad
		call seek			; Set up for read
		call rdbuff			; Read into (curdma)
		jmp setfcb			; Update FCB

curselect:
	lda seldsk! inr a! jz sel$error
	dcr a! lxi h,curdsk! cmp m! rz

				; Skip if seldsk = curdsk, fall into select
select:
			; Select disk info for subsequent input or output ops
	mov m,a 				; curdsk = seldsk

	mov d,a 		; Save seldsk in register D for selectdisk call
	lhld dlog! call test$vector 	; test$vector does not modify DE
	mov e,a! push d 		; Send to seldsk, save for test below
	call selectdisk! pop h 		; Recall dlog vector
	jnc sel$error			; returns with C flag set if select ok
					; Is the disk logged in?
	dcr l 				; reg l = 1 if so
	rz 				; yes - drive previously logged in

	lhld dlog! mov c,l! mov b,h 	; call ready
	call set$cdisk! shld dlog 	; dlog=set$cdisk(dlog)
	ret

set$seldsk:
	lda linfo! sta seldsk! ret

reselectx:
	xra a! sta high$ext! jmp reselect1
reselect:
			; Check current fcb to see if reselection necessary
	mvi a,80h! mov b,a! dcr a! mov c,a 	; b = 80h, c = 7fh
	lhld info! lxi d,7! xchg! dad d
	mov a,m! ana b
						; fcb(7) = fcb(7) & 7fh
	mov a,m! ana c! mov m,a
						; high$ext = 80h & fcb(8)
	inx h! mov a,m! ana b! sta high$ext
						; fcb(8) = fcb(8) & 7fh
	mov a,m! ana c! mov m,a
						; fcb(ext) = fcb(ext) & 1fh
	call clr$ext

	; if fcb(rc) & 80h 
	;    then fcb(rc) = 80h, actual$rc = fcb(rc) & 7fh
	;    else actual$rc = 0

	call getrcnta! mov a,m! ana b! jz reselect1
	mov a,m! ana c! mov m,b

reselect1:
	sta actual$rc

	lxi h,0
	shld fcbdsk 				; fcbdsk = 0
	mvi a,true! sta resel 			; Mark possible reselect
	lhld info! mov a,m 			; drive select code
	ani 1$1111b 				; non zero is auto drive select
	dcr a 			; Drive code normalized to 0..30, or 255
	sta linfo 				; Save drive code
	cpi 0ffh! jz noselect
				; auto select function, seldsk saved above
	mov a,m! sta fcbdsk 			; Save drive code
	call set$seldsk

  noselect:
		call curselect
		mvi a,0 ! lhld info ! mov m,a
		ret

;
;	individual function handlers
;

func12	equ func$ret

func13:

			; Reset disk system - initialize to disk 0
	lxi h,0! shld dlog

	xra a! sta seldsk
	dcr a! sta curdsk

	lxi h,tbuff! shld dmaad 		; dmaad = tbuff
        jmp setdata 				; to data dma address

func14:	
			; Select disk info
	call set$seldsk 			; seldsk = linfo
	jmp curselect

func15:
			; Open file
	call reselectx
	call open! call openx 		; returns if unsuccessful, a = 0
	ret

openx:
	call end$of$dir! rz
	call getfcba! mov a,m! inr a! jnz openxa
	dcx d! dcx d! ldax d! mov m,a
openxa:
						; open successful
	pop h 					; Discard return address
	mvi c,0100$0000b
	ret

func16	equ func$ret

func17	equ func$ret

func18	equ func$ret

func19	equ func$ret

func20:
				; Read a file
	call reselect
	jmp seqdiskread

func21	equ func$ret

func22	equ func$ret

func23	equ func$ret

func24	equ func$ret

func25: lda seldsk ! jmp sta$ret

func26:	xchg ! shld dmaad
	jmp setdata

func27	equ func$ret

func28:	equ func$ret

func29	equ func$ret

func30	equ func$ret

func31	equ func$ret

func32	equ func$ret

func33	equ func$ret

func34	equ func$ret

func35	equ func$ret

func36	equ func$ret

func37	equ func$ret

func38	equ func$ret

func39	equ func$ret

func40  equ func$ret

func42	equ func$ret

func43	equ func$ret

func44	equ func$ret

func45	equ func$ret

func46	equ func$ret

func47	equ	func$ret

func48	equ func$ret

func49	equ	func$ret

func50	equ	func$ret

func100	equ func$ret	

func101	equ func$ret

func102	equ func$ret

func103	equ func$ret

func104	equ func$ret

func105	equ func$ret

func106	equ func$ret

func107	equ func$ret

func108	equ func$ret

func109	equ func$ret


goback:
			; Arrive here at end of processing to return to user
	lda fx! cpi 15! jc retmon
	lda olddsk! sta seldsk 			; Restore seldsk
	lda resel! ora a! jz retmon

	lhld info! mvi m,0 ; fcb(0)=0
	lda fcbdsk! ora a! jz goback1
						; Restore fcb(0)
	mov m,a 				; fcb(0)=fcbdsk
  goback1:
						; fcb(8) = fcb(8) | high$ext
	inx h! lda high$ext! ora m! mov m,a
						; fcb(rc) = fcb(rc) | actual$rc
	call getrcnta! lda actual$rc! ora m! mov m,a
						; return from the disk monitor
retmon:
	lhld entsp! sphl
	lhld aret! mov a,l! mov b,h
	ret
;
;	data areas
;
dlog:	dw	0	; logged-in disks
curdma	ds	word	; current dma address
buffa:	ds	word	; pointer to directory dma address

;
;	curtrka - alloca are set upon disk select
;	(data must be adjacent, do not insert variables)
;	(address of translate vector, not used)
cdrmaxa:ds	word	; pointer to cur dir max value (2 bytes)
curtrka:ds	word	; current track address (2)
curreca:ds	word	; current record address (3)
drvlbla:ds	word	; current drive label byte address (1)
lsn$add:ds	word	; login sequence # address (1)
			; +1 -> bios media change flag (1)
dpbaddr:ds	word	; current disk parameter block address
checka:	ds	word	; current checksum vector address
alloca:	ds	word	; current allocation vector address
dirbcba:ds	word	; dir bcb list head
dtabcba:ds	word	; data bcb list head
hash$tbla:
	ds	word
	ds	byte

addlist	equ	$-dpbaddr	; address list size

;
; 	       buffer control block format
;
; bcb format : drv(1) || rec(3) || pend(1) || sequence(1) ||
;	       0         1         4          5
;
;	       track(2) || sector(2) || buffer$add(2) ||
;	       6           8            10
;
;	       link(2)
;	       12
;

;	sectpt - offset obtained from disk parm block at dpbaddr
;	(data must be adjacent, do not insert variables)
sectpt:	ds	word	; sectors per track
blkshf:	ds	byte	; block shift factor
blkmsk:	ds	byte	; block mask
extmsk:	ds	byte	; extent mask
maxall:	ds	word	; maximum allocation number
dirmax:	ds	word	; largest directory number
dirblk:	ds	word	; reserved allocation bits for directory
chksiz:	ds	word	; size of checksum vector
offset:	ds	word	; offset tracks at beginning
physhf:	ds	byte	; physical record shift
phymsk:	ds	byte	; physical record mask
dpblist	equ	$-sectpt	; size of area
;
;	local variables
;
blk$off:	ds	byte	; record offset within block
dir$cnt:	ds	byte	; direct i/o count

tranv:	ds	word	; address of translate vector
linfo:	ds	byte	; low(info)
dminx:	ds	byte	; local for diskwrite

actual$rc:
	ds	byte	; directory ext record count

single:	ds	byte	; set true if single byte allocation map


olddsk:	ds	byte	; disk on entry to bdos
rcount:	ds	byte	; record count in current fcb
extval:	ds	byte	; extent number and extmsk

vrecord:ds	byte	; current virtual record

curdsk:

adrive: db	0ffh	; current disk
arecord:ds	word	; current actual record
	ds	byte

arecord1:	ds	word	; current actual block# * blkmsk

;******** following variable order critical *****************

high$ext:	ds	byte	; fcb high ext bits
;xfcb$read$only:	ds	byte

;	local variables for directory access
dptr:	ds	byte	; directory pointer 0,1,2,3

;
;	local variables initialized by bdos at entry
;
fcbdsk:		ds	byte	; disk named in fcb

phy$off:	ds	byte
curbcba:	ds	word

track:		ds	word
sector:		ds	word

read$deblock:
	mvi a,1! call deblock$dta
	jmp setfcb

column		db 	0
outdelim:	db	'$'

dmaad:		dw	0080h
seldsk:		db	0
info:		dw	0
resel:		db	0
fx:		db	0
dcnt:		dw	0
searcha:	dw	0
searchl:	db	0


; 	**************************
; 	Blocking/Deblocking Module
;	**************************

deblock$dir:

	lhld dirbcba

	jmp deblock

deblock$dta:
	lhld dtabcba

deblock:

	; BDOS Blocking/Deblocking routine
	; a = 1 -> read command
	; a = 2 -> write command
	; a = 3 -> locate command
	; a = 4 -> flush command
	; a = 5 -> directory update

	push a 				; Save z flag and deblock fx

					; phy$off = low(arecord) & phymsk
					; low(arecord) = low(arecord) & ~phymsk
	call deblock8
	lda arecord! mov e,a! ana b! sta phy$off
	mov a,e! ana c! sta arecord

	shld curbcba! call getbuffa! shld curdma

	call deblock9
					; Is command flush?
	pop a! push a! cpi 4
	jnc deblock1 			; yes
					; Is referenced physical record 
					;already in buffer?
	call compare! jz deblock45 	; yes
	xra a
deblock1:
	call deblock10
					; Read physical record buffer
	mvi a,2! call deblock$io

	call deblock9 			; phypfx = adrive || arecord
	call move! mvi m,0 		; zero pending flag

deblock45:
					; recadd = phybuffa + phy$off*80h
	lda phy$off! inr a! lxi d,80h! lxi h,0ff80h
deblock5:
	dad d! dcr a! jnz deblock5
	xchg! lhld curdma! dad d
					; If deblock command = locate
					; then buffa = recadd; return
	pop a! cpi 3! jnz deblock6
	shld buffa! ret
deblock6:
	xchg! lhld dmaad! lxi b,80h
					; If deblock command = read
	jmp move$tpa 			; then move to dma

deblock8:
	lda phymsk! mov b,a! cma! mov c,a! ret

deblock9:
	lhld curbcba! lxi d,adrive! mvi c,4! ret

deblock10:
	lxi d,4
deblock11:
	lhld curbcba! dad d! ret

deblock$io:
					; a = 0 -> seek only
					; a = 1 -> write
					; a = 2 -> read
	push a! call seek
	pop a! dcr a
	cp rdbuff
					; Move track & sector to bcb
	call deblock10! inx h! inx h
	lxi d,track! mvi c,4! jmp move

	org	base+((($-base)+255) and 0ff00h)-1
	db	0

; Bios equates

bios$pg		equ	$

bootf		equ	bios$pg+00	; 00. cold boot
conoutf		equ	bios$pg+12	; 04. console output function
homef		equ	bios$pg+24	; 08. disk home function
seldskf		equ	bios$pg+27	; 09. select disk function
settrkf		equ	bios$pg+30	; 10. set track function
setsecf		equ	bios$pg+33	; 11. set sector function
setdmaf		equ	bios$pg+36	; 12. set dma function
sectran		equ	bios$pg+48	; 16. sector translate
movef		equ	bios$pg+75	; 25. memory move function
readf		equ	bios$pg+39	; 13. read disk function
move$out	equ	movef
move$tpa	equ	movef

		end
