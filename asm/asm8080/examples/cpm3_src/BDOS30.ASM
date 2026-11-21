;
;**********************************************************************
;*****************************************************************
;
;	Error Messages

if BANKED

md	equ	0

else

md	equ	24h

endif

dskmsg:	db	'CP/M Error On '
dskerr:	db	' : ',md
permsg:	db	'Disk I/O',md
selmsg:	db	'Invalid Drive',md
rofmsg:	db	'Read/Only File',md
rodmsg:	db	'Read/Only Disk',md

if not MPM

passmsg:

if BANKED
	db 	'Password Error',md
endif

fxstsmsg:
	db	'File Exists',md

wildmsg:
	db	'? in Filename',md

endif
if MPM

setlret1:
	mvi a,1
sta$ret:
	sta	aret
func$ret:
	ret
entsp:	ds	2

endif

;*****************************************************************
;*****************************************************************
;
;	common values shared between bdosi and bdos

if MPM

usrcode:db	0	; current user number

endif

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

tfcb	equ	005ch	; default fcb location
tbuff	equ	0080h	; default buffer location

;	error message handlers

rod$error:
	; report read/only disk error
	mvi c,2! jmp goerr

rof$error:
	; report read/only file error
	mvi c,3! jmp goerr	

sel$error:
	; report select error
	mvi c,4
	; Invalidate curdsk to force select call
	; at next curselect call
	mvi a,0ffh! sta curdsk

goerr:
	; hl = .errorhandler, call subroutine
	mov h,c! mvi l,0ffh! shld aret

if MPM
	call test$error$mode! jnz rtn$phy$errs
	mov a,c! lxi h,pererr-2! jmp bdos$jmp
else

goerr1:
	lda adrive! sta errdrv
	lda error$mode! inr a! cnz error
endif

rtn$phy$errs:

if MPM
	lda lock$shell! ora a! jnz lock$perr
endif

	; Return 0ffffh if fx = 27 or 31

	lda fx 
	cpi 27! jz goback0
	cpi 31! jz goback0
	jmp goback

if MPM

test$error$mode:
	lxi d,pname+4
test$error$mode1:
	call rlr! dad d
	mov a,m! ani 80h! ret
endif

if BANKED

set$copy$cr$only:
	lda copy$cr$init! sta copy$cr$only! ret

reset$copy$cr$only:
	xra a! sta copy$cr$init! sta copy$cr$only! ret

endif

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

incr$rr:
	call get$rra
	inr m! rnz
	inx h! inr m! rnz
	inx h! inr m! ret

save$rr:
	call save$rr2! xchg
save$rr1:
	mvi c,3! jmp move ; ret
save$rr2:
	call get$rra! lxi d,save$ranr! ret

reset$rr:
	call save$rr2! jmp save$rr1 ; ret

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
	mov c,d ; current disk# to c
	; lsb of e = 0 if not yet logged - in
	call seldskf ; hl filled by call
	; hl = 0000 if error, otherwise disk headers
	mov a,h! ora l! rz ; Return with C flag reset if select error
		; Disk header block address in hl
		mov e,m! inx h! mov d,m! inx h ; de=.tran
		shld cdrmaxa! inx h! inx h ; .cdrmax
		shld curtrka! inx h! inx h ; hl=.currec
		shld curreca! inx h! inx h ; hl=.buffa
		inx h! shld drvlbla! inx h
		shld lsn$add! inx h! inx h
		; de still contains .tran
		xchg! shld tranv ; .tran vector
		lxi h,dpbaddr ; de= source for move, hl=dest
		mvi c,addlist! call move ; addlist filled
		; Now fill the disk parameter block
		lhld dpbaddr! xchg ; de is source
		lxi h,sectpt ; hl is destination
		mvi c,dpblist! call move ; data filled
		; Now set single/double map mode
		lhld maxall ; largest allocation number
		mov a,h ; 00 indicates < 255
		lxi h,single! mvi m,true ; Assume a=00
		ora a! jz retselect
		; high order of maxall not zero, use double dm
		mvi m,false
	retselect:
		; C flag set indicates successful select
		stc! ret

home:
	; Move to home position, then offset to start of dir
	call homef
	xra a ; constant zero to accumulator
	lhld curtrka! mov m,a! inx h! mov m,a ; curtrk=0000
	lhld curreca! mov m,a! inx h! mov m,a ; currec=0000
	inx h! mov m,a ; currec high byte=00

if MPM
 	lxi h,0! shld dblk ; dblk = 0000
endif

	ret

rdbuff:
	; Read buffer and check condition
	mvi a,1! sta readf$sw
	call readf ; current drive, track, sector, dma
	jmp diocomp ; Check for i/o errors

wrbuff:
	; Write buffer and check condition
	; write type (wrtype) is in register c
	xra a! sta readf$sw
	call writef ; current drive, track, sector, dma
diocomp: ; Check for disk errors
	ora a! rz
	mov c,a
	call chk$media$flag
	mov a,c
	cpi 3! jc goerr
	mvi c,1! jmp goerr

chk$media$flag:
	; A = 0ffh -> media changed
	inr a! rnz

if BANKED
	; Handle media changes as I/O errors for 
	; permanent drives
	call chksiz$eq$8000h! rz
endif

	; BIOS says media change occurred
	; Is disk logged-in?
	lhld dlog! call test$vector! mvi c,1! rz ; no - return error
	call media$change
	pop h ; Discard return address
	; Was this a flush operation (fx = 48)?
	lda fx! cpi 48! rz ; yes
	; Is this a flush to another drive?
	lxi h,adrive! lda seldsk! cmp m! jnz reset$relog
	; Bail out if fx = read, write, close, or search next
	call chk$exit$fxs
	; Is this a directory read operation?
	lda readf$sw! ora a! rnz ; yes
	; Error - directory write operation
	mvi c,2! jmp goerr ; Return disk read/only error

reset$relog:
	; Reset relog if flushing to another drive
	xra a! sta relog! ret

if BANKED

chksiz$eq$8000h:
	; Return with Z flag set if drive permanent
	; with no checksum vector
	lhld chksiz! mvi a,80h! cmp h! rnz
	xra a! cmp l! ret

endif

seekdir:
	; Seek the record containing the current dir entry

if MPM
	lxi d,0ffffh ; mask = ffff
	lhld dblk! mov a,h! ora l! jz seekdir1
	lda blkmsk! mov e,a! xra a! mov d,a ; mask = blkmsk
	lda blkshf! mov c,a! xra a
	call shl3bv ; ahl = shl(dblk,blkshf)
seekdir1:
	push h! push a ; Save ahl
endif

	lhld dcnt ; directory counter to hl
	mvi c,dskshf! call hlrotr ; value to hl
	shld drec

if MPM

;	arecord = shl(dblk,blkshf) + shr(dcnt,dskshf) & mask

	mov a,l! ana e! mov l,a ; dcnt = dcnt & mask
	mov a,h! ana d! mov h,a
	pop b! pop d! call bde$e$bde$p$hl

else
	mvi b,0! xchg
endif

set$arecord:
	lxi h,arecord
	mov m,e! inx h! mov m,d! inx h! mov m,b
	ret

seek:
	; Seek the track given by arecord (actual record)

	lhld curtrka! mov c,m! inx h! mov b,m ; bc = curtrk
	push b ; s0 = curtrk 
	lhld curreca! mov e,m! inx h! mov d,m
	inx h! mov b,m ; bde = currec
	lhld arecord! lda arecord+2! mov c,a ; chl = arecord
seek0:
	mov a,l! sub e! mov a,h! sbb d! mov a,c! sbb b
	push h ; Save low(arecord)
	jnc seek1 ; if arecord >= currec then go to seek1
	lhld sectpt! call bde$e$bde$m$hl ; currec = currec - sectpt
	pop h! xthl! dcx h! xthl ; curtrk = curtrk - 1
	jmp seek0
seek1:
	lhld sectpt! call bde$e$bde$p$hl ; currec = currec + sectpt
	pop h ; Restore low(arecord)
	mov a,l! sub e! mov a,h! sbb d! mov a,c! sbb b
	jc seek2 ; if arecord < currec then go to seek2
	xthl! inx h! xthl ; curtrk = curtrk + 1
	push h ; save low (arecord)
	jmp seek1
seek2:
	xthl! push h ; hl,s0 = curtrk, s1 = low(arecord)
	lhld sectpt! call bde$e$bde$m$hl ; currec = currec - sectpt
	pop h! push d! push b! push h ; hl,s0 = curtrk, 
	; s1 = high(arecord,currec), s2 = low(currec), 
	; s3 = low(arecord)
	xchg! lhld offset! dad d
	mov b,h! mov c,l! shld track
	call settrkf ; call bios settrk routine
	; Store curtrk
	pop d! lhld curtrka! mov m,e! inx h! mov m,d
	; Store currec
	pop b! pop d!
	lhld curreca! mov m,e! inx h! mov m,d
	inx h! mov m,b ; currec = bde
	pop b ; bc = low(arecord), de = low(currec)
	mov a,c! sub e! mov l,a ; hl = bc - de
	mov a,b! sbb d! mov h,a
	call shr$physhf
	mov b,h! mov c,l

	lhld tranv! xchg ; bc=sector#, de=.tran
	call sectran ; hl = tran(sector)
	mov c,l! mov b,h ; bc = tran(sector)
	shld sector
	call setsecf ; sector selected
	lhld curdma! mov c,l! mov b,h! jmp setdmaf
	; ret
shr$physhf:
	lda physhf! mov c,a! jmp hlrotr

;	file control block (fcb) constants

empty	equ	0e5h	; empty directory entry
lstrec	equ	127	; last record# on extent
recsiz	equ	128	; record size
fcblen	equ	32	; file control block size
dirrec	equ	recsiz/fcblen	; directory fcbs / record
dskshf	equ	2	; log2(dirrec)
dskmsk	equ	dirrec-1
fcbshf	equ	5	; log2(fcblen)

extnum	equ	12	; extent number field
maxext	equ	31	; largest extent number
ubytes	equ	13	; unfilled bytes field
modnum	equ	14	; data module number

maxmod	equ	64	; largest module number

fwfmsk	equ	80h	; file write flag is high order modnum
namlen	equ	15	; name length
reccnt	equ	15	; record count field
dskmap	equ	16	; disk map field
lstfcb	equ	fcblen-1
nxtrec	equ	fcblen
ranrec	equ	nxtrec+1; random record field (2 bytes)

;	reserved file indicators

rofile	equ	9	; high order of first type char
invis	equ	10	; invisible file in dir command

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

get$atts:
	; Get volatile attributes starting at f'5
	; info locates fcb
	lhld info
	lxi d,8! dad d ; hl = .fcb(f'8)
	mvi c,4
get$atts$loop:
	mov a,m! add a! push a
	mov a,d! rar! mov d,a
	pop a! rrc! mov m,a
	dcx h! dcr c! jnz get$atts$loop
	mov a,d! ret

get$s1:
	; Get current s1 field to a
	call getexta! inx h! mov a,m! ret

get$rra:
	; Get current ran rec field address to hl
	lhld info! lxi d,ranrec! dad d ; hl=.fcb(ranrec)
	ret

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
	xchg! mov a,m! ora a! jnz getfcb0
	call get$dir$ext! mov c,a! call set$rc! mov a,m
getfcb0:
	cpi 81h! jc getfcb1
	mvi a,80h
getfcb1:
	sta rcount ; rcount=fcb(reccnt) or 80h
	call getexta ; hl=.fcb(extnum)
	lda extmsk ; extent mask to a
	ana m ; fcb(extnum) and extmsk
	sta extval
	ret

setfcb:
	; Place values back into current fcb
	call getfcba ; addresses to de, hl
	; fcb(cr) = vrecord
	lda vrecord! mov m,a
	; Is fx < 22? (sequential read or write)
	lda fx! cpi 22! jnc $+4 ; no
	; fcb(cr) = fcb(cr) + 1
	inr m
	xchg! mov a,m! cpi 80h! rnc ; dont reset fcb(rc) if > 7fh
	lda rcount! mov m,a ; fcb(reccnt)=rcount
	ret

zero$ext$mod:
	call getexta! mov m,d! inx h! inx h! mov m,d
	ret

zero:
	mov m,b! inx h! dcr c! rz
	jmp zero

hlrotr:
	; hl rotate right by amount c
	inr c ; in case zero
	hlrotr0: dcr c! rz ; return when zero
		mov a,h! ora a! rar! mov h,a ; high byte
		mov a,l! rar! mov l,a ; low byte
		jmp hlrotr0

compute$cs:
	; Compute checksum for current directory buffer
	lhld buffa ; current directory buffer
	lxi b,4 ; b = 0, c = 4
compute$cs0:
	mvi d,32 ; size of fcb
	xra a ; clear checksum value
compute$cs1:
		add m! inx h! dcr d 
		jnz compute$cs1
		xra b! mov b,a! dcr c
		jnz compute$cs0
	ret ; with checksum in a

if MPM

compute$cs:
	; Compute checksum for current directory buffer
	mvi c,recsiz ; size of directory buffer
	lhld buffa ; current directory buffer
	xra a ; Clear checksum value
	computecs0:
		add m! inx h! dcr c ; cs = cs+buff(recsiz-c)
		jnz computecs0
	ret ; with checksum in a

chksum$fcb: ; Compute checksum for fcb
	; Add 1st 12 bytes of fcb + curdsk + 
	;     high$ext + xfcb$read$only + bbh
	lxi h,pdcnt! mov a,m
	inx h! add m ; Add high$ext
	inx h! add m ; Add xfcb$read$only
	inx h! add m ; Add curdsk
	adi 0bbh ; Add 0bbh to bias checksum
	lhld info! mvi c,12! call computecs0
	; Skip extnum
	inx h
	; Add fcb(s1)
	add m! inx h
	; Skip modnum
	inx h
	; Skip fcb(reccnt)
	; Add disk map
	inx h! mvi c,16! call computecs0
	ora a! ret ; Z flag set if checksum valid

set$chksum$fcb:
	call chksum$fcb! rz
	mov b,a! call gets1
	cma! add b! cma
	mov m,a! ret

reset$chksum$fcb:
	xra a! sta comp$fcb$cks
	call chksum$fcb! rnz
	call get$s1! inr m! ret

endif

check$fcb:

if MPM
	xra a! sta check$fcb4
check$fcb1:
	call chek$fcb! rz
check$fcb2:

	ani 0fh! jnz check$fcb3
	lda pdcnt! ora a! jz check$fcb3
	call set$sdcnt! sta dont$close
	call close1
	lxi h,lret! inr m! jz check$fcb3
	mvi m,0! call pack$sdcnt! mvi b,5
	call search$olist! rz
check$fcb3:

	pop h ; Discard return address
check$fcb4:
	nop
	mvi a,10! jmp sta$ret

set$fcb$cks$flag:
	mvi a,0ffh! sta comp$fcb$cks! ret

else
	call gets1! lhld lsn$add
	cmp m! cnz chk$media$fcb
endif

chek$fcb:
	lda high$ext

if MPM

	; if ext & 0110$0000b = 0110$0000b then
	; set fcb(0) to 0 (user 0)

	cpi 0110$0000b! jnz chek$fcb1
else
	ora a! rz
endif

	lhld info! xra a! mov m,a ; fcb(0) = 0
chek$fcb1:

if MPM
	jmp chksum$fcb ; ret
else
	ret

chk$media$fcb:
	; fcb(s1) ~= DPH login sequence # field
	; Is fcb addr < bdosadd?

if banked
	lhld user$info
else
	lhld info
endif

	xchg! lhld bdosadd! call subdh! jnc chk$media1 ; no
	; Is rlog(drive) true?
	lhld rlog! call testvector! rz ; no
chk$media1:
	; Return invalid fcb error code
	pop h! pop h
chk$media2:
	mvi a,10! jmp sta$ret
endif

hlrotl:
 	; Rotate the mask in hl by amount in c
 	inr c ; may be zero
 	hlrotl0: dcr c! rz ; return if zero
 		dad h! jmp hlrotl0

set$dlog:
	lxi d,dlog
set$cdisk:
	; Set a "1" value in curdsk position of bc
	lda curdsk
set$cdisk1:
	mov c,a ; Ready parameter for shift
	lxi h,1 ; number to shift
	call hlrotl ; hl = mask to integrate
	ldax d! ora l! stax d! inx d
	ldax d! ora h! stax d! ret

nowrite:
	; Return true if dir checksum difference occurred
	lhld rodsk

test$vector:
	lda curdsk
test$vector1:
	mov c,a! call hlrotr
	mov a,l! ani 1b! ret ; non zero if curdsk bit on

check$rodir:
	; Check current directory element for read/only status
	call getdptra ; address of element

check$rofile:
	; Check current buff(dptr) or fcb(0) for r/o status
	call ro$test
	rnc ; Return if not set
	jmp rof$error ; Exit to read only disk message

ro$test:
	lxi d,rofile! dad d
	mov a,m! ral! ret ; carry set if r/o

check$write:
	; Check for write protected disk
	call nowrite! rz ; ok to write if not rodsk
	jmp rod$error ; read only disk error

getdptra:
	; Compute the address of a directory element at
	; positon dptr in the buffer

	lhld buffa! lda dptr
addh:
	; hl = hl + a
	add l! mov l,a! rnc
	; overflow to h
	inr h! ret

getmodnum:
	; Compute the address of the module number 
	; bring module number to accumulator
	; (high order bit is fwf (file write flag)
	lhld info! lxi d,modnum! dad d ; hl=.fcb(modnum)
	mov a,m! ret ; a=fcb(modnum)

clrmodnum:
	; Clear the module number field for user open/make
	call getmodnum! mvi m,0 ; fcb(modnum)=0
	ret

clr$ext:
	; fcb ext = fcb ext & 1fh
	call getexta! mov a,m! ani 0001$1111b! mov m,a!
	ret

setfwf:
	call getmodnum ; hl=.fcb(modnum), a=fcb(modnum)
	; Set fwf (file write flag) to "1"
	ori fwfmsk! mov m,a ; fcb(modnum)=fcb(modnum) or 80h
	; also returns non zero in accumulator
	ret

compcdr:
	; Return cy if cdrmax > dcnt
	lhld dcnt! xchg ; de = directory counter
	lhld cdrmaxa ; hl=.cdrmax
	mov a,e! sub m ; low(dcnt) - low(cdrmax)
	inx h ; hl = .cdrmax+1
	mov a,d! sbb m ; hig(dcnt) - hig(cdrmax)
	; condition dcnt - cdrmax  produces cy if cdrmax>dcnt
	ret

setcdr:
	; if not (cdrmax > dcnt) then cdrmax = dcnt+1
	call compcdr
	rc ; Return if cdrmax > dcnt
	; otherwise, hl = .cdrmax+1, de = dcnt
	inx d! mov m,d! dcx h! mov m,e
	ret

subdh:
	; Compute hl = de - hl
	mov a,e! sub l! mov l,a! mov a,d! sbb h! mov h,a
	ret

newchecksum:
	mvi c,0feh ; Drop through to compute new checksum
checksum:
	; Compute current checksum record and update the
	; directory element if c=true, or check for = if not
	; drec < chksiz?
	lhld drec! xchg! lhld chksiz
	mov a,h! ani 7fh! mov h,a ; Mask off permanent drive bit
	call subdh ; de-hl
	rnc ; Skip checksum if past checksum vector size
		; drec < chksiz, so continue
		push b ; Save init flag
		call compute$cs ; Check sum value to a
		lhld checka ; address of check sum vector
		xchg
		lhld drec
		dad d ; hl = .check(drec)
		pop b ; Recall true=0ffh or false=00 to c
		inr c ; 0ffh produces zero flag
		jz initial$cs
		inr c ; 0feh produces zero flag
		jz update$cs

if MPM
		inr c! jz test$dir$cs
endif

			; not initializing, compare
			cmp m ; compute$cs=check(drec)?
			rz ; no message if ok
			; checksum error, are we beyond
			; the end of the disk?
			call nowrite! rnz

media$change:
			call discard$data

if MPM
			call flush$file0
else
			mvi a,0ffh! sta relog! sta hashl
			call set$rlog
endif

			; Reset the drive

			call set$dlog! jmp reset37x

if MPM
		test$dir$cs:
			cmp m! jnz flush$files
			ret
endif

		initial$cs:
			; initializing the checksum
			cmp m! mov m,a! rz
			; or 1 into login seq # if media change
			lhld lsn$add! mvi a,1! ora m! mov m,a! ret

		update$cs:
			; updating the checksum
			mov m,a! ret

set$ro:
	; Set current disk to read/only
	lda seldsk! lxi d,rodsk! call set$cdisk1 ; sets bit to 1
	; high water mark in directory goes to max
	lhld dirmax! inx h! xchg ; de = directory max
	lhld cdrmaxa ; hl = .cdrmax
	mov m,e! inx h! mov m,d ; cdrmax = dirmax
	ret

set$rlog:
	; rlog(seldsk) = true
	lhld olog! call test$vector! rz
	lxi d,rlog! jmp set$cdisk

tst$log$fxs:
	lda chksiz+1! ani 80h! rnz
	lxi h,log$fxs
tst$log0:
	lda fx! mov b,a
tst$log1:
	mov a,m! cmp b! rz
	inx h! ora a! jnz tst$log1
	inr a! ret

test$media$flag:
	lhld lsn$add! inx h! mov a,m! ora a! ret

chk$exit$fxs:
	lxi h,goback! push h
	; does fx = read or write function?
	; and is drive removable?
	lxi h,rw$fxs! call tst$log0! jz chk$media2 ; yes
	; is fx = close or searchn function?
	; and is drive removable?
	lxi h,sc$fxs! call tst$log0! jz lret$eq$ff ; yes
	pop h! ret

tst$relog:
	lxi h,relog! mov a,m! ora a! rz
	mvi m,0
drv$relog:
	call curselect
	lxi h,0! shld dcnt! xra a! sta dptr
	ret

set$lsn:
	lhld lsn$add! mov c,m
	call gets1! mov m,c! ret

discard$data$bcb:
	lhld dtabcba! mvi c,4! jmp discard0

discard$data:
	lhld dtabcba! jmp discard

discard$dir:
	lhld dirbcba

discard:
	mvi c,1
discard0:
	mov a,l! ana h! inr a! rz

if BANKED
	mov e,m! inx h! mov d,m! xchg
discard1:
	push h! push b
	lxi d,adrive! call compare
	pop b! pop h! jnz discard2

	mvi m,0ffh
discard2:
	lxi d,13! dad d
	mov e,m! inx h! mov d,m
	xchg! mov a,l! ora h! rz
	jmp discard1
else
	push h
	lxi d,adrive! call compare
	pop h! rnz
	mvi m,0ffh! ret
endif

get$buffa:
	push d! lxi d,10! dad d
	mov e,m! inx h! mov d,m

if BANKED
	inx h! mov a,m! sta buffer$bank
endif

	xchg! pop d! ret

rddir:
	; Read a directory entry into the directory buffer
	call seek$dir
	mvi a,3! jmp wrdir0

seek$copy:
wrdir:
	; Write the current directory entry, set checksum
	call check$write
	call newchecksum ; Initialize entry
	mvi a,5
wrdir0:
	lxi h,0! shld last$block
	lhld dirbcba

if BANKED
	cpi 5! jnz $+6
	lhld curbcba
endif

	call deblock

setdata:
	; Set data dma address
	lhld dmaad! jmp setdma ; to complete the call

setdir1:
	call get$buffa

setdma:
	; hl=.dma address to set (i.e., buffa or dmaad)
	shld curdma! ret

dir$to$user:

if not MPM
	; Copy the directory entry to the user buffer
	; after call to search or searchn by user code
	lhld buffa! xchg ; source is directory buffer
	lhld xdmaad ; destination is user dma address
	lxi b,recsiz ; copy entire record
	call movef
endif
	; Set lret to dcnt & 3 if search successful
	lxi h,lret! mov a,m! inr a! rz
	lda dcnt! ani dskmsk! mov m,a! ret

make$fcb$inv: ; Flag fcb as invalid
	; Reset fcb write flag
	call setfwf
	; Set 1st two bytes of diskmap to ffh
	inx h! inx h! mvi a,0ffh! mov m,a! inx h! mov m,a
	ret

chk$inv$fcb: ; Check for invalid fcb
	call getdma! jmp test$ffff

tst$inv$fcb: ; Test for invalid fcb
	call chk$inv$fcb! rnz
	pop h! mvi a,9! jmp sta$ret! ; lret = 9

end$of$dir:
	; Return zero flag if at end of directory, non zero
	; if not at end (end of dir if dcnt = 0ffffh)
	lxi h,dcnt
test$ffff:
	mov a,m ; may be 0ffh
	inx h! cmp m ; low(dcnt) = high(dcnt)?
	rnz ; non zero returned if different
	; high and low the same, = 0ffh?
	inr a ; 0ffh becomes 00 if so
	ret

set$end$dir:
	; Set dcnt to the end of the directory
	lxi h,enddir! shld dcnt! ret

read$dir:
	call r$dir! jmp r$dir1

r$dir:
	; Read next directory entry, with c=true if initializing

	lhld dirmax! xchg ; in preparation for subtract
	lhld dcnt! inx h! shld dcnt ; dcnt=dcnt+1
	; Continue while dirmax >= dcnt (dirmax-dcnt no cy)
	call subdh ; de-hl

	jc set$end$dir

	read$dir0:
		; not at end of directory, seek next element
		; initialization flag is in c
		lda dcnt! ani dskmsk ; low(dcnt) and dskmsk
		mvi b,fcbshf ; to multiply by fcb size
		read$dir1:
			add a! dcr b! jnz read$dir1
		; a = (low(dcnt) and dskmsk) shl fcbshf
		sta dptr ; ready for next dir operation
		ora a! rnz ; Return if not a new record
	read$dir2:
		push b ; Save initialization flag c
		call rd$dir ; Read the directory record
		pop b ; Recall initialization flag
		lda relog! ora a! rnz
		jmp checksum ; Checksum the directory elt

r$dir2:
	call read$dir2
r$dir1:
	lda relog! ora a! rz
	call chk$exit$fxs
	call tst$relog! jmp rd$dir

getallocbit:
	; Given allocation vector position bc, return with byte
	; containing bc shifted so that the least significant
	; bit is in the low order accumulator position.  hl is
	; the address of the byte for possible replacement in
	; memory upon return, and d contains the number of shifts
	; required to place the returned value back into position
	mov a,c! ani 111b! inr a! mov e,a! mov d,a
	; d and e both contain the number of bit positions to shift

	mov h,b! mov l,c! mvi c,3 ; bc = bc shr 3
	call hlrotr ; hlrotr does not touch d and e
	mov b,h! mov c,l

	lhld alloca ; base address of allocation vector
	dad b! mov a,m ; byte to a, hl = .alloc(bc shr 3)
	; Now move the bit to the low order position of a
	rotl: rlc! dcr e! jnz rotl! ret

setallocbit:
	; bc is the bit position of alloc to set or reset.  the
	; value of the bit is in register e.
	push d! call getallocbit ; shifted val a, count in d
	ani 1111$1110b ; mask low bit to zero (may be set)
	pop b! ora c ; low bit of c is masked into a
	; jmp rotr ; to rotate back into proper position	
	; ret

rotr:
	; byte value from alloc is in register a, with shift count
	; in register c (to place bit back into position), and
	; target alloc position in registers hl, rotate and replace
	rrc! dcr d! jnz rotr ; back into position
	mov m,a ; back to alloc
	ret

copy$alv:
	; If Z flag set, copy 1st ALV to 2nd
	; Otherwise, copy 2nd ALV to 1st

if not BANKED
	lda bdos$flags! rlc! rlc! rc
endif

	push a
	call get$nalbs! mov b,h! mov c,l
	lhld alloca! mov d,h! mov e,l! dad b
	pop a! jz movef
	xchg! jmp movef

scandm$ab:
	; Set/Reset 1st and 2nd ALV
	push b! call scandm$a
	pop b! ;jmp scandm$b

scandm$b:
	; Set/Reset 2nd ALV

if not BANKED
	lda bdos$flags! ani 40h! rnz
endif

	push b! call get$nalbs
	xchg! lhld alloca
	pop b! push h! dad d! shld alloca
	call scandm$a
	pop h! shld alloca! ret

scandm$a:
	; Set/Reset 1st ALV
	; Scan the disk map addressed by dptr for non-zero
	; entries, the allocation vector entry corresponding
	; to a non-zero entry is set to the value of c (0,1)
	call getdptra ; hl = buffa + dptr
	; hl addresses the beginning of the directory entry
	lxi d,dskmap! dad d ; hl now addresses the disk map
	push b ; Save the 0/1 bit to set
	mvi c,fcblen-dskmap+1 ; size of single byte disk map + 1
	scandm0:
		; Loop once for each disk map entry
		pop d ; Recall bit parity
		dcr c! rz ; all done scanning?
		; no, get next entry for scan
		push d ; Replace bit parity
		lda single! ora a! jz scandm1
			; single byte scan operation
			push b ; Save counter
			push h ; Save map address
			mov c,m! mvi b,0 ; bc=block#
			jmp scandm2
		scandm1:
			; double byte scan operation
			dcr c ; count for double byte
			push b ; Save counter
			mov c,m! inx h! mov b,m ; bc=block#
			push h ; Save map address
		scandm2:
			; Arrive here with bc=block#, e=0/1
			mov a,c! ora b ; Skip if = 0000
			jz scandm3
			lhld maxall ; Check invalid index
			mov a,l! sub c! mov a,h! sbb b ; maxall - block#
			cnc set$alloc$bit
			; bit set to 0/1
		scandm3:
			pop h! inx h ; to next bit position
			pop b ; Recall counter
			jmp scandm0 ; for another item

get$nalbs: ; Get # of allocation vector bytes
	lhld maxall! mvi c,3
	; number of bytes in allocation vector is (maxall/8)+1
	call hlrotr! inx h! ret

if MPM

test$dir:
	call home
	call set$end$dir
test$dir1:
	mvi c,0feh! call read$dir
	lda flushed! ora a! rnz
	call end$of$dir! rz
	jmp test$dir1
endif

initialize:
	; Initialize the current disk
	; lret = false ; set to true if $ file exists
	; Compute the length of the allocation vector - 2

if MPM
	lhld tlog! call test$vector! jz initialize1
	lhld tlog! call remove$drive! shld tlog
	xra a! sta flushed
	call test$dir! rz
initialize1:
else
	call test$media$flag! mvi m,0 ; Reset media change flag
	call discard$data
	call discard$dir
endif

if BANKED
	; Is drive permanent with no chksum vector?
	call chksiz$eq$8000h! jnz initialize2 ; no
	; Is this an initial login operation?
	; register A = 0
	lhld lsn$add! cmp m! mvi m,2! jz initialize2 ; yes
	jmp copy$alv ; Copy 2nd ALV to 1st ALV
initialize2:

endif

	call get$nalbs ; Get # of allocation vector bytes
	mov b,h! mov c,l ; Count down bc til zero
	lhld alloca ; base of allocation vector
	; Fill the allocation vector with zeros
	initial0:
		mvi m,0! inx h ; alloc(i)=0
		dcx b ; Count length down
		mov a,b! ora c! jnz initial0

	lhld drvlbla! mov m,a ; Zero out drive desc byte

	; Set the reserved space for the directory

	lhld dirblk! xchg
	lhld alloca ; hl=.alloc()
	mov m,e! inx h! mov m,d ; sets reserved directory blks
	; allocation vector initialized, home disk
	call home
        ; cdrmax = 3 (scans at least one directory record)
	lhld cdrmaxa! mvi m,4! inx h! mvi m,0

	call set$end$dir ; dcnt = enddir
	lhld hashtbla! shld arecord1

	; Read directory entries and check for allocated storage

	initial2:
		mvi c,true! call read$dir
		call end$of$dir! jz copy$alv
		; not end of directory, valid entry?
		call getdptra ; hl = buffa + dptr
		xchg! lhld arecord1! mov a,h! ana l! inr a! xchg
		; is hashtbla ~= 0ffffh
		cnz init$hash ; yes - call init$hash
		mvi a,21h! cmp m
		jz initial2 ; Skip date & time records

		mvi a,empty! cmp m
		jz initial2 ; go get another item

		mvi a,20h! cmp m! jz drv$lbl
		mvi a,10h! ana m! jnz initial3

		; Now scan the disk map for allocated blocks

		mvi c,1 ; set to allocated
		call scandm$a
	initial3:
		call setcdr ; set cdrmax to dcnt
		jmp initial2 ; for another entry

drv$lbl:
		lxi d,extnum! dad d! mov a,m
		lhld drvlbla! mov m,a! jmp initial3

copy$dirloc:
	; Copy directory location to lret following
	; delete, rename, ... ops

	lda dirloc! jmp sta$ret
	; ret

compext:
	; Compare extent# in a with that in c, return nonzero
	; if they do not match
	push b ; Save c's original value
	push psw! lda extmsk! cma! mov b,a
	; b has negated form of extent mask
	mov a,c! ana b! mov c,a ; low bits removed from c
	pop psw! ana b ; low bits removed from a
	sub c! ani maxext ; Set flags
	pop b ; Restore original values
	ret

get$dir$ext:
	; Compute directory extent from fcb
	; Scan fcb disk map backwards
	call getfcba ; hl = .fcb(vrecord)
	mvi c,16! mov b,c! inr c! push b
	; b=dskmap pos (rel to 0)
get$de0:
	pop b
	dcr c
	xra a ; Compare to zero
get$de1:
	dcx h! dcr b; Decr dskmap position
	cmp m! jnz get$de2 ; fcb(dskmap(b)) ~= 0
	dcr c! jnz get$de1
	; c = 0 -> all blocks = 0 in fcb disk map
get$de2:
	mov a,c! sta dminx
	lda single! ora a! mov a,b
	jnz get$de3
	rar ; not single, divide blk idx by 2
get$de3:
	push b! push h ; Save dskmap position & count
	mov l,a! mvi h,0 ; hl = non-zero blk idx
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
	pop b ; Restore stack
	ret ; a = directory extent

searchi:
	; search initialization
	lhld info! shld searcha ; searcha = info
searchi1:
	mov a,c! sta searchl ; searchl = c
	call set$hash
	mvi a,0ffh! sta dirloc ; changed if actually found
	ret

search$namlen:
	mvi c,namlen! jmp search
search$extnum:
	mvi c,extnum
search:
	; Search for directory element of length c at info
	call searchi
search1: ; entry point used by rename
	call set$end$dir ; dcnt = enddir
	call tst$log$fxs! cz home
	; (drop through to searchn)

searchn:
	; Search for the next directory element, assuming
	; a previous call on search which sets searcha and
	; searchl

if MPM
	lxi h,user0$pass! xra a! cmp m! mov m,a! cnz swap
else
	xra a! sta user0$pass
endif

	call search$hash! jnz search$fin
	mvi c,false! call read$dir ; Read next dir element
	call end$of$dir! jz search$fin
		; not end of directory, scan for match
		lhld searcha! xchg ; de=beginning of user fcb
		ldax d ; first character
		cpi empty ; Keep scanning if empty
		jz searchnext
		; not empty, may be end of logical directory
		push d ; Save search address
		call compcdr ; past logical end?
		pop d ; Recall address
		jnc search$fin ; artificial stop
searchnext:
		call getdptra ; hl = buffa+dptr
		lda searchl! mov c,a ; length of search to c
		mvi b,0 ; b counts up, c counts down

		mov a,m! cpi empty! cz save$dcnt$pos1 

if BANKED
		xra a! sta save$xfcb
		mov a,m! ani 1110$1111b! cmp m! jz search$loop
		xchg! cmp m! xchg! jnz search$loop
		lda find$xfcb! ora a! jz search$n
		sta save$xfcb! jmp searchok
endif

		searchloop:
			mov a,c! ora a! jz endsearch
			ldax d! cpi '?'! jz searchok ; ? in user fcb
			; Scan next character if not ubytes
			mov a,b! cpi ubytes! jz searchok
			; not the ubytes field, extent field?
			cpi extnum ; may be extent field
			jz searchext ; Skip to search extent
			cpi modnum! ldax d! cz searchmod
			sub m! ani 7fh ; Mask-out flags/extent modulus
			jnz searchnm ; Skip if not matched
			jmp searchok ; matched character
		searchext:
			ldax d
			; Attempt an extent # match
			push b ; Save counters

if MPM
			push h
 			lhld sdcnt
 			inr h! jnz dont$save
 			lhld dcnt! shld sdcnt
 			lhld dblk! shld sdblk
 		dont$save:
			pop h
endif

			mov c,m ; directory character to c
			call compext ; Compare user/dir char

			mov b,a
			lda user0pass! inr a! jz save$dcnt$pos2
			; Disable search of user 0 if any fcb
			; is found under the current user #
			xra a! sta search$user0
			mov a,b

			pop b ; Recall counters
			ora a ; Set flag
			jnz searchn ; Skip if no match
		searchok:
			; current character matches
			inx d! inx h! inr b! dcr c
			jmp searchloop
		endsearch:
			; entire name matches, return dir position

if BANKED
			lda save$xfcb! inr a! jnz endsearch1
			lda xdcnt+1! cpi 0feh! cz save$dcnt$pos0
			jmp searchn
		endsearch1:
endif

			xra a! sta dirloc ; dirloc = 0
			sta lret ; lret = 0
			; successful search -
			; return with zero flag reset
			mov b,a! inr b! ret
		searchmod:
			ani 3fh! ret ; Mask off high 2 bits
		search$fin:
			; end of directory, or empty name

			call save$dcnt$pos1

			; Set dcnt = 0ffffh
			call set$end$dir ; may be artifical end
		lret$eq$ff:
			; unsuccessful search -
			; return with zero flag set
			; lret,low(aret) = 0ffh
			mvi a,255! mov b,a! inr b! jmp sta$ret

		searchnm: ; search no match routine
			mov a,b! ora a! jnz searchn ; fcb(0)?
			mov a,m! ora a! jnz searchn ; dir fcb(0)=0?
			lda search$user0! ora a! jz searchn
			sta user0$pass

if MPM
			call swap
endif

			jmp searchok

if MPM

swap: ; Swap dcnt,sdblk with sdcnt0,sdblk0
	push h! push d! push b
	lxi d,sdcnt! lxi h,sdcnt0
	mvi b,4
swap1:
	ldax d! mov c,a! mov a,m
	stax d! mov m,c
	inx h! inx d! dcr b! jnz swap1
	pop b! pop d! pop h! 
	ret
endif

save$dcnt$pos2:
	; Save directory position of matching fcb
	; under user 0 with matching extent # & modnum = 0
	; a = 0 on entry
	ora b! pop b! lxi b,searchn! push b! rnz
	inx h! inx h! mov a,m! ora a! rnz
	; Call if user0pass = 0ffh &
	;         dir fcb(extnum) = fcb(extnum)
	;         dir fcb(modnum) = 0
save$dcnt$pos0:
	call save$dcnt$pos ; Return to searchn
save$dcnt$pos1:
	; Save directory position of first empty fcb
	; or the end of the directory
	push h! 
	lhld xdcnt! inr h! jnz save$dcnt$pos$ret ; Return if h ~= 0ffh
save$dcnt$pos:
	lhld dcnt! shld xdcnt

if MPM
	lhld dblk! shld xdblk
endif

save$dcnt$pos$ret:
	pop h! ret

if BANKED

init$xfcb$search:
	mvi a,0ffh
init$xfcb$search1:
	sta find$xfcb! mvi a,0feh! sta xdcnt+1! ret

does$xfcb$exist:
	lda xdcnt+1! cpi 0feh! rz
	call set$dcnt$dblk
	xra a! call init$xfcb$search1
	lhld searcha! mov a,m! ori 10h! mov m,a
	mvi c,extnum! call searchi1! jmp searchn 

xdcnt$eq$dcnt:
	lhld dcnt! shld xdcnt! ret

restore$dir$fcb:
	call set$dcnt$dblk
	mvi c,namlen! call searchi! jmp searchn
endif

delete:
	; Delete the currently addressed file
	call get$atts

if BANKED
	sta attributes
	; Make search return matching fcbs and xfcbs
deletex:
	mvi a,0feh! call init$xfcb$search1
else
	; Return with aret = 0 for XFCB only delete
	; in non-banked systems
	ral! rc
endif

; Delete pass 1 - check r/o attributes and xfcb passwords

	call search$extnum! rz 

    delete00:
	    jz delete1

if BANKED
	    ; Is addressed dir fcb an xfcb?
	    call getdptra! mov a,m! ani 10h! jnz delete01 ; yes

if MPM
	    call tst$olist ; Verify fcb not open by someone else
endif

	    ; Check r/o attribute if this is not an
	    ; xfcb only delete operation.
	    lda attributes! ral! cnc check$rodir
else
	    call check$rodir
endif

if BANKED
	    ; Are xfcb passwords enabled?
	    call get$dir$mode! ral! jc delete02 ; no
endif

	    ; Is this a wild card delete operation?
	    lhld info! call chk$wild! jz delete02 ; yes
	    ; Not wild & passwords inactive
	    ; Skip to pass 2
	    jmp delete11

if BANKED

    delete01:
	    ; Check xfcb password if passwords enabled
	    call get$dir$mode! ral! jnc delete02
	    call chk$xfcb$password! jz delete02
	    call chk$pw$error! jmp deletex
endif

    delete02:
	    call searchn! jmp delete00

; Delete pass 2 - delete all matching fcbs and/or xfcbs.

delete1:
	call search$extnum

    delete10:
	    jz copy$dir$loc
    delete11:
	    call getdptra

if BANKED
	    ; Is addressed dir fcb an xfcb?
	    mov a,m! ani 10h! jnz delete12 ; yes
if MPM
	    push h
	    call chk$olist ; Delete olist item if present
	    pop h
endif
	    ; Is this delete operation xfcb only?
	    lda attributes! ani 80h! jnz delete13 ; yes
endif

    delete12:
	    ; Delete dir fcb or xfcb
	    ; if fcb free all alocated blocks.

	    mvi m,empty

if BANKED

    delete13:
	    push a ; Z flag set => free FCB blocks
	    ; Zero password mode byte in sfcb if sfcb exists
	    ; Does sfcb exist?
	    call get$dtba$8! ora a! jnz $+4 ; no
	    ; Zero mode byte
	    mov m,a
endif

	    call wrdir! mvi c,0

if BANKED
	    pop a! cz scandm$ab
else
	    call scandm$ab
endif

	    call fix$hash
	    call searchn! jmp delete10

get$block:
	; Given allocation vector position bc, find the zero bit
	; closest to this position by searching left and right.
	; if found, set the bit to one and return the bit position
	; in hl.  if not found (i.e., we pass 0 on the left, or
	; maxall on the right), return 0000 in hl
	mov d,b! mov e,c ; copy of starting position to de
	righttst:
		lhld maxall ; value of maximum allocation#
		mov a,e! sub l! mov a,d! sbb h ; right=maxall?
		jnc retblock0 ; return block 0000 if so
		inx d! push b! push d ; left, right pushed
		mov b,d! mov c,e ; ready right for call
		call getallocbit
		rar! jnc retblock ; Return block number if zero
		pop d! pop b ; Restore left and right pointers
	lefttst:
		mov a,c! ora b! jz righttst ; Skip if left=0000
		; left not at position zero, bit zero?
		dcx b! push d! push b ; left,right pushed
		call getallocbit
		rar! jnc retblock ; return block number if zero
		; bit is one, so try the right
		pop b! pop d ; left, right restored
		jmp righttst
	retblock:
		ral! inr a ; bit back into position and set to 1
		; d contains the number of shifts required to reposition
		call rotr ; move bit back to position and store
		pop h! pop d ; hl returned value, de discarded
		ret
	retblock0:
		; cannot find an available bit, return 0000
		mov a,c
		ora b! jnz lefttst ; also at beginning    
		lxi h,0000h! ret

copy$dir:
	; Copy fcb information starting at c for e bytes
	; into the currently addressed directory entry
	mvi d,80h
copy$dir0:
	call copy$dir2
	inr c
copy$dir1:
	dcr c! jz seek$copy
	mov a,m! ana b! push b
	mov b,a! ldax d! ani 7fh! ora b! mov m,a
	pop b! inx h! inx d! jmp copy$dir1
copy$dir2:
	push d ; Save length for later
	mvi b,0 ; double index to bc
	lhld info ; hl = source for data
	dad b
	inx h! mov a,m! sui '$'! cz set$submit$flag
	dcx h! xchg ; de=.fcb(c), source for copy
	call getdptra ; hl=.buff(dptr), destination
	pop b ; de=source, hl=dest, c=length
	ret

set$submit$flag:
	lxi d,ccp$flgs! ldax d! ori 1! stax d! ret

check$wild:
	; Check for ? in file name or type
	lhld info
check$wild0:	; entry point used by rename
	call chk$wild! rnz
	mvi a,9! jmp set$aret

chk$wild:
	mvi c,11
chk$wild1:
	inx h! mvi a,3fh! sub m! ani 7fh! rz
	dcr c! jnz chk$wild1! ora a! ret

copy$user$no:
	lhld info! mov a,m! lxi b,dskmap
	dad b! mov m,a! ret

rename:
	; Rename the file described by the first half of
	; the currently addressed file control block. The
	; new name is contained in the last half of the
	; currently addressed file control block.  The file
	; name and type are changed, but the reel number
	; is ignored.  The user number is identical.

	; Verify that the new file name does not exist.
	; Also verify that no wild chars exist in
	; either filename.

if MPM
	call getatts! sta attributes
endif

	; Verify that no wild chars exist in 1st filename.
	call check$wild

if BANKED
	; Check password of file to be renamed.
	call chk$password! cnz chk$pw$error
	; Setup search to scan for xfcbs.
	call init$xfcb$search
endif

	; Copy user number to 2nd filename
	call copy$user$no
	shld searcha

	; Verify no wild chars exist in 2nd filename
	call check$wild0

	; Verify new filename does not already exist
	mvi c,extnum! lhld searcha! call searchi1! call search1
	jnz file$exists ; New filename exists

if BANKED
	; If an xfcb exists for the new filename, delete it.
	call does$xfcb$exist! cnz delete11
endif

	call copy$user$no

if BANKED
	call init$xfcb$search
endif

	; Search up to the extent field
	call search$extnum
	rz
	call check$rodir ; may be r/o file

if MPM
	call chk$olist
endif

	; Copy position 0
	rename0:
		; not end of directory, rename next element
		mvi c,dskmap! mvi e,extnum! call copy$dir
		; element renamed, move to next

		call fix$hash
		call searchn
		jnz rename0
	rename1:

if BANKED
		call does$xfcb$exist! jz copy$dir$loc
		call copy$user$no! jmp rename0
else
		jmp copy$dir$loc
endif

indicators:
	; Set file indicators for current fcb
	call get$atts ; Clear f5' through f8'
	sta attributes

if BANKED
	call chk$password! cnz chk$pw$error
endif

	call search$extnum ; through file type
	rz

if MPM
	call chk$olist
endif

	indic0:
		; not end of directory, continue to change
		mvi c,0! mvi e,extnum ; Copy name
		call copy$dir2! call move
		lda attributes! ani 40h! jz indic1

		; If interface att f6' set, dir fcb(s1) = fcb(cr)

		push h! call getfcba! mov a,m
		pop h! inx h! mov m,a
	indic1:
		call seek$copy
		call searchn
		jz copy$dir$loc
		jmp indic0

open:
	; Search for the directory entry, copy to fcb
	call search$namlen
open1:
	rz ; Return with lret=255 if end
	; not end of directory, copy fcb information
open$copy:
	call setfwf! mov e,a! push h! dcx h! dcx h
	mov d,m! push d ; Save extent# & module# with fcb write flag set
	call getdptra! xchg ; hl = .buff(dptr)
	lhld info ; hl=.fcb(0)
	mvi c,nxtrec ; length of move operation
	call move ; from .buff(dptr) to .fcb(0)
	; Note that entire fcb is copied, including indicators
	call get$dir$ext! mov c,a
	; Restore module # and extent #
	pop d! pop h! mov m,e! dcx h! dcx h! mov m,d
	; hl = .user extent#, c = dir extent#
	; above move set fcb(reccnt) to dir(reccnt)
	; if fcb ext < dir ext then fcb(reccnt) = fcb(reccnt) | 128
	; if fcb ext = dir ext then fcb(reccnt) = fcb(reccnt)
	; if fcb ext > dir ext then fcb(reccnt) = 0

set$rc: ; hl=.fcb(ext), c=dirext
		mvi b,0
		xchg! lxi h,(reccnt-extnum)! dad d
		; Is fcb ext = dirext?
		ldax d! sub c! jz set$rc2 ; yes
		; Is fcb ext > dirext?
		mov a,b! jnc set$rc1 ; yes - fcb(rc) = 0
		; fcb ext  < dirext
		; fcb(rc) = 128 | fcb(rc)
		mvi a,128! ora m
	set$rc1:
		mov m,a! ret 
	set$rc2:
		; fcb ext = dirext
		mov a,m! ora a! rnz ; ret if fcb(rc) ~= 0
	set$rc3:
		mvi m,0 ; required by function 99
		lda dminx! ora a! rz ; ret if no blks in fcb
		mvi m,128! ret ; fcb(rc) = 128

mergezero:
	; hl = .fcb1(i), de = .fcb2(i),
	; if fcb1(i) = 0 then fcb1(i) := fcb2(i)
	mov a,m! inx h! ora m! dcx h! rnz ; return if = 0000
	ldax d! mov m,a! inx d! inx h ; low byte copied
	ldax d! mov m,a! dcx d! dcx h ; back to input form
	ret

restore$rc:
	; hl = .fcb(extnum)
	; if fcb(rc) > 80h then fcb(rc) = fcb(rc) & 7fh
	push h
	lxi d,(reccnt-extnum)! dad d
	mov a,m! cpi 81h! jc restore$rc1
	ani 7fh! mov m,a
restore$rc1:
	pop h! ret

close:
	; Locate the directory element and re-write it
	xra a! sta lret

if MPM
	sta dont$close
endif

	call nowrite! rnz ; Skip close if r/o disk
	; Check file write flag - 0 indicates written
	call getmodnum ; fcb(modnum) in a
	ani fwfmsk! rnz ; Return if bit remains set
close1:
	call chk$inv$fcb! jz mergerr

if MPM
	call set$fcb$cks$flag
endif

	call get$dir$ext! mov c,a
	mov b,m! push b
	; b = original extent, c = directory extent
	; Set fcb(ex) to directory extent
	mov m,c
	; Recompute fcb(rc)
	call restore$rc
	; Call set$rc if fcb ext > dir ext
	mov a,c! cmp b! cc set$rc
	call close$fcb
	; Restore original extent & reset fcb(rc)
	call get$exta! pop b
	mov c,m! mov m,b! jmp set$rc ; Reset fcb(rc)

close$fcb:
	; Locate file
	call search$namlen
	rz ; Return if not found
	; Merge the disk map at info with that at buff(dptr)
	lxi b,dskmap! call get$fcb$adds
	mvi c,(fcblen-dskmap) ; length of single byte dm
	merge0:
		lda single! ora a! jz merged ; Skip to double
		; This is a single byte map
		; if fcb(i) = 0 then fcb(i) = buff(i)
		; if buff(i) = 0 then buff(i) = fcb(i)
		; if fcb(i) <> buff(i) then error
		mov a,m! ora a! ldax d! jnz fcbnzero
			; fcb(i) = 0
			mov m,a ; fcb(i) = buff(i)
		fcbnzero:
		ora a! jnz buffnzero
			; buff(i) = 0
			mov a,m! stax d ; buff(i)=fcb(i)
		buffnzero:
		cmp m! jnz mergerr ; fcb(i) = buff(i)?
		jmp dmset ; if merge ok
	merged:
		; This is a double byte merge operation
		call mergezero ; buff = fcb if buff 0000
		xchg! call mergezero! xchg ; fcb = buff if fcb 0000
		; They should be identical at this point
		ldax d! cmp m! jnz mergerr ; low same?
		inx d! inx h ; to high byte
		ldax d! cmp m! jnz mergerr ; high same?
		; merge operation ok for this pair
		dcr c ; extra count for double byte
	dmset:
		inx d! inx h ; to next byte position
		dcr c! jnz merge0 ; for more
		; end of disk map merge, check record count
		; de = .buff(dptr)+32, hl = .fcb(32)

		xchg! lxi b,-(fcblen-extnum)! dad b! push h
		call get$dir$ext! pop d

		; hl = .fcb(extnum), de = .buff(dptr+extnum)

		call compare$extents

		; b=1 -> fcb(ext) ~= dir ext = buff(ext)
		; b=2 -> fcb(ext) = dir ext ~= buff(ext)
		; b=3 -> fcb(ext) = dir ext = buff(ext)

		; fcb(ext), buff(ext) = dir ext
		mov m,a! stax d! push b

		lxi b,(reccnt-extnum)! dad b! xchg! dad b
		pop b

		; hl = .buff(rc) , de = .fcb(rc)

		dcr b! jz mrg$rc1 ; fcb(rc) = buff(rc)

		dcr b! jz mrg$rc2 ; buff(rc) = fcb(rc)

		ldax d! cmp m! jc mrg$rc1 ; Take larger rc
		ora a! jnz mrg$rc2
		call set$rc3

       mrg$rc1: xchg

       mrg$rc2: ldax d! mov m,a

if MPM
		lda dont$close! ora a! rnz
endif

		; Set t3' off indicating file update
		call getdptra! lxi d,11! dad d
		mov a,m! ani 7fh! mov m,a
		call setfwf
		mvi c,1! call scandm$b ; Set 2nd ALV vector
		jmp seek$copy ; OK to "wrdir" here - 1.4 compat
		; ret
	mergerr:
		; elements did not merge correctly
		call make$fcb$inv
		jmp lret$eq$ff

compare$extents:
	mvi b,1! cmp m! rnz
	inr b! xchg! cmp m! xchg! rnz
	inr b! ret

set$xdcnt:
	lxi h,0ffffh! shld xdcnt! ret

set$dcnt$dblk:
	lhld xdcnt
set$dcnt$dblk1:
	mvi a,1111$1100b! ana l
	mov l,a! dcx h! shld dcnt

if MPM
	lhld xdblk! shld dblk
endif

	ret

if MPM

sdcnt$eq$xdcnt:
	lxi h,sdcnt! lxi d,xdcnt! mvi c,4
	jmp move
endif

make:
	; Create a new file by creating a directory entry
	; then opening the file

	lxi h,xdcnt! call test$ffff! cnz set$dcnt$dblk

	lhld info! push h ; Save fcb address, Look for E5
	lxi h,efcb! shld info ; info = .empty
	mvi c,1

	call searchi! call searchn

	; zero flag set if no space
	pop h ; Recall info address
	shld info ; in case we return here
	rz ; Return with error condition 255 if not found

if BANKED
	; Return early if making an xfcb
	lda make$xfcb! ora a! rnz
endif

	; Clear the remainder of the fcb
	; Clear s1 byte
	lxi d,13! dad d! mov m,d! inx h
	; Clear and save file write flag of modnum
	mov a,m! push a! push h! ani 3fh! mov m,a! inx h
	mvi a,1
	mvi c,fcblen-namlen ; number of bytes to fill
	make0:
		mov m,d! inx h! dcr c! jnz make0
		dcr a! mov c,d! cz get$dtba
		ora a! mvi c,10! jz make0
	call setcdr ; may have extended the directory
	; Now copy entry to the directory
	mvi c,0! lxi d,fcblen! call copy$dir0
	; and restore the file write flag
	pop h! pop a! mov m,a
	; and set the fcb write flag to "1"
	call fix$hash
	jmp setfwf

open$reel:
	; Close the current extent, and open the next one
	; if possible.  rmf is true if in read mode

if BANKED
	call reset$copy$cr$only
endif

	call getexta
	mov a,m! mov c,a
	inr c! call compext
	jz open$reel3
	push h! push b
	call close
	pop b! pop h
	lda lret! inr a! rz
	mvi a,maxext! ana c! mov m,a ; Incr extent field
	; Advance to module & save
	inx h! inx h! mov a,m! sta save$mod
	jnz open$reel0 ; Jump if in same module

	open$mod:
		; Extent number overflow, go to next module
		inr m ; fcb(modnum)=++1
		; Module number incremented, check for overflow

		mov a,m! ani 3fh ; Mask high order bits

		jz open$r$err ; cannot overflow to zero

		; otherwise, ok to continue with new module
	open$reel0:
		call set$xdcnt ; Reset xdcnt for make

if MPM
		call set$sdcnt
endif

		call search$namlen ; Next extent found?
		jnz open$reel1
			; end of file encountered
			lda rmf! inr a ; 0ffh becomes 00 if read
			jz open$r$err ; sets lret = 1
			; Try to extend the current file
			call make
			; cannot be end of directory
			jz open$r$err ; with lret = 1

if MPM
			call fix$olist$item
			call set$fcb$cks$flag
endif

			jmp open$reel2
		open$reel1:
			; not end of file, open
			call open$copy
	
if MPM
			call set$fcb$cks$flag
endif

		open$reel2:

if not MPM
			call set$lsn
endif

			call getfcb ; Set parameters
			xra a! sta vrecord! jmp sta$ret ; lret = 0
			; ret ; with lret = 0
	open$r$err:
		; Restore module and extent
		call getmodnum! lda save$mod! mov m,a
		dcx h! dcx h! mov a,m! dcr a! ani 1fh
		mov m,a! jmp setlret1 ; lret = 1

	open$reel3:
		inr m ; fcb(ex) = fcb(ex) + 1
		call get$dir$ext! mov c,a
		; Is new extent beyond dir$ext?
		cmp m! jnc open$reel4 ; no
		dcr m ; fcb(ex) = fcb(ex) - 1
		; Is this a read fx?
		lda rmf! inr a! jz set$lret1 ; yes - Don't advance ext
		inr m ; fcb(ex) = fcb(ex) + 1
	open$reel4:
		call restore$rc
		call set$rc! jmp open$reel2

seqdiskread:
diskread:	; (may enter from seqdiskread)
	call tst$inv$fcb ; Check for valid fcb
	mvi a,true! sta rmf ; read mode flag = true (open$reel)

if MPM
	sta dont$close
endif

	; Read the next record from the current fcb
	call getfcb ; sets parameters for the read
diskread0:
	lda vrecord! lxi h,rcount! cmp m ; vrecord-rcount
	; Skip if rcount > vrecord
	jc recordok

if MPM
		call test$disk$fcb! jnz diskread0
		lda vrecord
endif

		; not enough records in the extent
		; record count must be 128 to continue
		cpi 128 ; vrecord = 128?
		jnz setlret1 ; Skip if vrecord<>128
		call open$reel ; Go to next extent if so
		; Check for open ok
		lda lret! ora a! jnz setlret1 ; Stop at eof
	recordok:
		; Arrive with fcb addressing a record to read

if BANKED
		call set$copy$cr$only
endif

		call index ; Z flag set if arecord = 0

if MPM
		jnz recordok1
		call test$disk$fcb! jnz diskread0
endif

		jz setlret1 ; Reading unwritten data
	recordok1:
		; Record has been allocated, read it
		call atran ; arecord now a disk address
		call check$nprs
		jc setfcb
		jnz read$deblock

		call setdata
		call seek ; to proper track,sector

if BANKED
		mvi a,1! call setbnkf
endif

		call rdbuff ; to dma address
		jmp setfcb ; Replace parameter	

read$deblock:
	lxi h,0! shld last$block
	mvi a,1! call deblock$dta
	jmp setfcb

check$nprs:
	;
	; on exit,  c flg          -> no i/o operation
	;	    z flg & ~c flg -> direct(physical) i/o operation
	;	   ~z flg & ~c flg -> indirect(deblock) i/o operation
	;
	;          Dir$cnt contains the number of 128 byte records
	;	   to transfer directly.  This routine sets dir$cnt
	;	   when initiating a sequence of direct physical
	;	   i/o operations.  Dir$cnt is decremented each
	;	   time check$nprs is called during such a sequence.
	;
	; Is direct transfer operation in progress?
	lda blk$off! mov b,a
	lda phy$msk! mov c,a! ana b! push a
	lda dir$cnt! cpi 2! jc check$npr1 ; no
	; yes - Decrement direct record count
	dcr a! sta dir$cnt
	; Are we at a new physical record?
	pop a! stc! rnz ; no - ret with c flg set
	; Perform physical i/o operation
	xra a! ret ; Return with z flag set and c flag reset
check$npr1:
	; Are we in mid-physical record?
	pop a! jz check$npr11 ; no
check$npr1a:
	; Is phymsk = 0?
	mov a,c! ora a! rz ; yes - Don't deblock
check$npr1b: 
	; Deblocking required
	ori 1! ret ; ret with z flg reset and c flg reset
check$npr11:
	mov a,c! cma! mov d,a ; d = ~phy$msk
	lxi h,vrecord
	; Is mult$num < 2?
	lda mult$num! cpi 2! jc check$npr1a ; yes
	add m! cpi 80h! jc check$npr2
	mvi a,80h
check$npr2: ; a = min(vrecord + mult$num),80h) = x
	push b ; Save low(arecord) & blkmsk, phymsk
	mov b,m! mvi m,7fh ; vrecord = 7f
	push b ; Save vrecord
	push h ; Save .vrecord
	push a ; Save x
	lda blkmsk! mov e,a! inr e! cma! ana b! mov b,a
	; b = vrecord & ~blkmsk
	; e = blkmsk + 1
	pop h ; h = x
	; Is this a read function?
	lda rmf! ora a! jz check$npr21 ; no
	; Is rcount & ~phymsk < x?
	lda rcount! ana d! cmp h! jc check$npr23 ; yes
check$npr21:
	mov a,h ; a = x
check$npr23:
	sub b ; a = a - vrecord & ~blkmsk
	mov c,a ; c = max # of records from beginning of curr blk
	; Is c < blkmsk+1?
	cmp e! jc check$npr8 ; yes

if BANKED
	push b ; c = max # of records
	; Compute maximum disk map position
	call dm$position
	mov b,a ; b = index of last block in extent
	; Does the last block # = the current block #?
	lda dminx! cmp b! mov e,a! jz check$npr5 ; yes
	; Compute # of blocks in sequence
	mov c,a! push b! mvi b,0
	call get$dm ; hl = current block #
check$npr4:
	; Get next block #
	push h! inx b! call get$dm
	pop d! inx d
	; Does next block # = previous block # + 1?
	mov a,d! sub h! mov d,a
	mov a,e! sub l! ora d! jz check$npr4 ; yes
	; Is next block # = 0?
	mov a,h! ora l! jnz check$npr45 ; no
	; Is this a read function?
	lda rmf! ora a! jnz check$npr45 ; no
	; Is next block # > maxall?
	lhld maxall! mov a,l! sub e
	mov a,h! sbb d! jc check$npr45 ; yes
	; Is next block # allocated?
	push b! push d! mov b,d! mov c,e
	call getallocbit! pop h! pop b
	rar! jnc check$npr4 ; no - it will be later
check$npr45:
	dcr c! pop d
	; Is max dm position less than c?
	mov a,d! cmp c! jc check$npr5 ; yes
	mov a,c ; no
check$npr5: ; a = index of last block
	sub e! mov b,a! inr b ; b = # of consecutive blks
	lda blkmsk! inr a! mov c,a
check$npr6:
	dcr b! jz check$npr7
	add c! jmp check$npr6
check$npr7:
	pop b
	mov b,c ; b = max # of records
	mov c,a ; c = (# of consecutive blks)*(blkmsk+1)
	lda rmf! ora a! jz check$npr8
	mov a,b! cmp c! jc check$npr9
else
	mov c,e ; multis-sector max = 1 block in non-banked systems
endif

check$npr8:
	mov a,c
check$npr9:
	; Restore vrecord
	pop h! pop b! mov m,b
	pop b
	; a = max # of consecutive records including current blk
	; b = low(arecord) & blkmsk
	; c = phymsk
	; Is mult$num > a - b
	lxi h,mult$num! mov d,m
	sub b! cmp d! jnc check$npr10
	mov d,a ; yes - use smaller value to compute dir$cnt
check$npr10:
	; Does this operation involve at least 1 physical record?
	mov a,c! cma! ana d! sta dir$cnt! jz check$npr1b ; Deblocking required
	; Flush any pending buffers before doing multiple reads
	push a! lda rmf! ora a! jz check$npr10a
	call flushx! call setdata
check$npr10a:
	pop a! mov h,a ; Save # of 128 byte records
	; Does this operation involve more than 1 physical record?
	; Register h contains number of 128 byte records
	call shr$physhf! mov a,h
	cpi 1! mov c,a! cnz mult$iof ; yes - Make bios call
	xra a! ret ; Return with z flg set 

if MPM

test$unlocked:
	lda high$ext! ani 80h! ret

test$disk$fcb:
	call test$unlocked! rz
	lda dont$close! ora a! rz
	call close1
test$disk$fcb1:
	pop d
	lxi h,lret! inr m! mvi a,11! jz sta$ret
	mvi m,0
	push d
	call getrcnta! mov a,m! sta rcount ; Reset rcount
	xra a! sta dont$close
	inr a! ret
endif

reset$fwf:
	call getmodnum ; hl=.fcb(modnum), a=fcb(modnum)
	; Reset the file write flag to mark as written fcb
	ani (not fwfmsk) and 0ffh ; bit reset
	mov m,a ; fcb(modnum) = fcb(modnum) and 7fh
	ret

set$filewf:
	call getmodnum! ani 0100$0000b! push a
	mov a,m! ori 0100$0000b! mov m,a! pop a! ret

seqdiskwrite:
diskwrite:	; (may enter here from seqdiskwrite above)
	mvi a,false! sta rmf ; read mode flag
	; Write record to currently selected file

	call check$write ; in case write protected

if BANKED
	lda xfcb$read$only! ora a
	mvi a,3! jnz set$aret
endif

	lda high$ext

if MPM
	ani 0100$0000b
else
	ora a
endif

	; Z flag reset if r/o mode
	mvi a,3! jnz set$aret

	lhld info ; hl = .fcb(0)
	call check$rofile ; may be a read-only file

	call tst$inv$fcb ; Test for invalid fcb

	call update$stamp

	call getfcb ; to set local parameters
	lda vrecord! cpi lstrec+1 ; vrecord-128
	jc diskwrite0
	call open$reel ; vrecord = 128, try to open next extent
	lda lret! ora a! rnz ; no available fcb
disk$write0:

if MPM
	mvi a,0ffh! sta dont$close
disk$write1:

endif

	; Can write the next record, so continue
	call index ; Z flag set if arecord = 0
	jz diskwrite2
	; Was the last write operation for the same block & drive?
	lxi h,adrive! lxi d,last$drive! mvi c,3
	call compare! jz diskwrite15 ; yes
	; no - force preread in blocking/deblocking
	mvi a,0ffh! sta last$off
diskwrite15:

if MPM
	; If file is unlocked, verify record is not locked
	; Record has to be allocated to be locked
	call test$unlocked! jz not$unlocked
	call atran! mov c,a
	lda mult$cnt! mov b,a! push b
	call test$lock! pop b
	xra a! mov c,a! push b
	jmp diskwr10
not$unlocked:
	inr a
endif

	mvi c,0 ; Marked as normal write operation for wrbuff
	jmp diskwr1
diskwrite2:

if MPM
		call test$disk$fcb! jnz diskwrite1
endif

if BANKED
		call reset$copy$cr$only
endif

		; not allocated
		; The argument to getblock is the starting
		; position for the disk search, and should be
		; the last allocated block for this file, or
		; the value 0 if no space has been allocated
		call dm$position
		sta dminx ; Save for later
		lxi b,0000h ; May use block zero
		ora a! jz nopblock ; Skip if no previous block
			; Previous block exists at a
			mov c,a! dcx b ; Previous block # in bc
			call getdm ; Previous block # to hl
			mov b,h! mov c,l ; bc=prev block#
		nopblock:
			; bc = 0000, or previous block #
			call get$block ; block # to hl
		; Arrive here with block# or zero
		mov a,l! ora h! jnz blockok
			; Cannot find a block to allocate
			mvi a,2! jmp sta$ret ; lret=2	
		blockok:

if MPM
		call set$fcb$cks$flag
endif

		; allocated block number is in hl
		shld arecord! shld last$block! xra a! sta last$off
		lda adrive! sta lastdrive
		xchg ; block number to de
		lhld info! lxi b,dskmap! dad b ; hl=.fcb(dskmap)
		lda single! ora a ; Set flags for single byte dm
		lda dminx ; Recall dm index
		jz allocwd ; Skip if allocating word
			; Allocating a byte value
			call addh! mov m,e ; single byte alloc
			jmp diskwru ; to continue
		allocwd:
		; Allocate a word value
			mov c,a! mvi b,0 ; double(dminx)
			dad b! dad b ; hl=.fcb(dminx*2)
			mov m,e! inx h! mov m,d ; double wd
		diskwru:
		; disk write to previously unallocated block
		mvi c,2 ; marked as unallocated write
	diskwr1:
	; Continue the write operation of no allocation error
	; c = 0 if normal write, 2 if to prev unalloc block
	push b ; Save write flag
	call atran ; arecord set
diskwr10:
		lda fx! cpi 40! jnz diskwr11 ; fx ~= wrt rndm zero fill
		mov a,c! dcr a! dcr a! jnz diskwr11 ; old allocation  

		; write random zero fill + new block

		pop b! push a ; zero write flag
		lhld arecord! push h
		lxi h,phymsk! mov e,m! inr e! mov d,a! push d
		lhld dirbcba

if BANKED
		mov e,m! inx h! mov d,m! xchg
fill00:
		push h! call get$next$bcba! pop d! jnz fill00
		xchg
endif

		; Force prereads in blocking/deblocking
		; Discard BCB
		dcr a! sta last$off! mov m,a 
		call setdir1 ; Set dma to BCB buffer
		; Zero out BCB buffer
		pop d! push d! xra a
	fill0:
		mov m,a! inx h! inr d! jp fill0
		mov d,a! dcr e! jnz fill0
		; Write 1st physical record of block
		lhld arecord1! mvi c,2
	fill1:
		shld arecord! push b! call discard$data$bcb
		call seek

if BANKED
		xra a! call setbnkf
endif

		pop b! call wrbuff
		lhld arecord! pop d! push d
		; Continue writing until blkmsk & arecord = 0
		dad d! lda blkmsk! ana l! mvi c,0! jnz fill1
		; Restore arecord
		pop h! pop h! shld arecord

		call setdata ; Restore dma
	diskwr11:

	pop d! lda vrecord! mov d,a ; Load and save vrecord
	push d! call check$nprs

	jc dont$write
	jz write

	mvi a,2 ; deblock write code
	call deblock$dta
	jmp dont$write
write:
	call setdata
	call seek

if BANKED
	mvi a,1! call setbnkf
endif

	; Discard matching BCB if write is direct
	call discard$data$bcb

	; Set write flag to zero if arecord & blkmsk ~= 0

	pop b! push b! lda arecord
	lxi h,blkmsk! ana m! jz write0
	mvi c,0
write0:
	call wrbuff

dont$write:
	pop b ; c = 2 if a new block was allocated, 0 if not
	; Increment record count if rcount<=vrecord
	mov a,b! lxi h,rcount! cmp m ; vrecord-rcount
	jc diskwr2
		; rcount <= vrecord
		mov m,a! inr m ; rcount = vrecord+1

if MPM
		call test$unlocked! jz write1
	
		; for unlocked files 
		;   rcount = rcount & (~ blkmsk) + blkmsk + 1

		lda blkmsk! mov b,a! inr b! cma! mov c,a
		mov a,m! dcr a! ana c! add b! mov m,a
	write1:
endif

		mvi c,2 ; Mark as record count incremented
	diskwr2:
	; a has vrecord, c=2 if new block or new record#
	dcr c! dcr c! jnz noupdate
		call reset$fwf

if MPM
		call test$unlocked! jz noupdate
		lda rcount! call getrcnta! mov m,a
		call close
		call test$disk$fcb1
endif

noupdate:
	; Set file write flag if reset
	call set$filewf

if BANKED
	jnz disk$write3
	; Reset fcb file write flag to ensure t3' gets
	; reset by the close function
	call reset$fwf
	call reset$copy$cr$only
	jmp setfcb
disk$write3:
	call set$copy$cr$only
else
	cz reset$fwf
endif
	jmp setfcb ; Replace parameters
	; ret

rseek:   
	; Random access seek operation, c=0ffh if read mode
	; fcb is assumed to address an active file control block
	; (1st block of FCB = 0ffffh if previous bad seek)
	push b ; Save r/w flag
	lhld info! xchg ; de will hold base of fcb
		lxi h,ranrec! dad d ; hl=.fcb(ranrec)
		mov a,m! ani 7fh! push psw ; record number
		mov a,m! ral ; cy=lsb of extent#
		inx h! mov a,m! ral! ani 11111b ; a=ext#
		mov c,a ; c holds extent number, record stacked

		mov a,m! ani 1111$0000b! inx h! ora m
		rrc! rrc! rrc! rrc! mov b,a
		; b holds module #

		; Check high byte of ran rec <= 3
		mov a,m
		ani 1111$1100b! pop h! mvi l,6! mov a,h

		; Produce error 6, seek past physical eod
		jnz seekerr

		; otherwise, high byte = 0, a = sought record
		lxi h,nxtrec! dad d ; hl = .fcb(nxtrec)
		mov m,a ; sought rec# stored away

	; Arrive here with b=mod#, c=ext#, de=.fcb, rec stored
	; the r/w flag is still stacked.  compare fcb values

		lda fx! cpi 99! jz rseek3
		; Check module # first
		push d! call chk$inv$fcb! pop d! jz ranclose
		lxi h,modnum! dad d! mov a,b ; b=seek mod#
		sub m! ani 3fh! jnz ranclose ; same?
		; Module matches, check extent
		lxi h,extnum! dad d
		mov a,m! cmp c! jz seekok2 ; extents equal
		call compext! jnz ranclose
		; Extent is in same directory fcb
		push b! call get$dir$ext! pop b
		cmp c! jnc rseek2 ; jmp if dir$ext > ext
		pop d! push d! inr e! jnz rseek2 ; jmp if write fx
		inr e! pop d! jmp set$lret1 ; error - reading unwritten data
	rseek2:
		mov m,c ; fcb(ext) = c
		mov c,a ; c = dir$ext
		; hl=.fcb(ext),c=dir ext
		call restore$rc
		call set$rc
		jmp seekok1
	ranclose:
		push b! push d ; Save seek mod#,ext#, .fcb
		call close ; Current extent closed
		pop d! pop b ; Recall parameters and fill
		mvi l,3 ; Cannot close error #3
		lda lret! inr a! jz seekerr
        rseek3:
		call set$xdcnt ; Reset xdcnt for make

if MPM
		call set$sdcnt
endif

		lxi h,extnum! dad d! push h
		mov d,m! mov m,c ; fcb(extnum)=ext#
		inx h! inx h! mov a,m! mov e,a! push d
		ani 040h! ora b! mov m,a
		; fcb(modnum)=mod#
		call open ; Is the file present?
		lda lret! inr a! jnz seekok ; Open successful?
		; Cannot open the file, read mode?
		pop d! pop h! pop b ; r/w flag to c (=0ffh if read)
		push b! push h! push d ; Restore stack
		mvi l,4 ; Seek to unwritten extent #4
		inr c ; becomes 00 if read operation
		jz badseek ; Skip to error if read operation
		; Write operation, make new extent
		call make
		mvi l,5 ; cannot create new extent #5
		jz badseek ; no dir space

if MPM
		call fix$olist$item
endif

		; file make operation successful
	seekok:
		pop b! pop b ; Discard top 2 stacked items

if MPM
		call set$fcb$cks$flag
else
		call set$lsn
endif

	seekok1:

if BANKED
		call reset$copy$cr$only
endif

	seekok2:
		pop b ; Discard r/w flag or .fcb(ext)
		xra a! jmp sta$ret ; with zero set	
	badseek:
		; Restore fcb(ext) & fcb(mod)
		pop d! xthl ; Save error flag
		mov m,d! inx h! inx h! mov m,e
		pop h ; Restore error flag
	seekerr:

if BANKED
		call reset$copy$cr$only ; Z flag set
		inr a ; Reset Z flag
endif

		pop b ; Discard r/w flag
		mov a,l! jmp sta$ret ; lret=#, nonzero

randiskread:
	; Random disk read operation
	mvi c,true ; marked as read operation
	call rseek
	cz diskread ; if seek successful
	ret

randiskwrite:
	; Random disk write operation
	mvi c,false ; marked as write operation
	call rseek
	cz diskwrite ; if seek successful
	ret

compute$rr:
	; Compute random record position for getfilesize/setrandom
	xchg! dad d
	; de=.buf(dptr) or .fcb(0), hl = .f(nxtrec/reccnt)
	mov c,m! mvi b,0 ; bc = 0000 0000 ?rrr rrrr
	lxi h,extnum! dad d! mov a,m! rrc! ani 80h ; a=e000 0000
	add c! mov c,a! mvi a,0! adc b! mov b,a
	; bc = 0000 000? errrr rrrr
	mov a,m! rrc! ani 0fh! add b! mov b,a
	; bc = 000? eeee errrr rrrr
	lxi h,modnum! dad d! mov a,m ; a=xxmm mmmm
	add a! add a! add a! add a ; cy=m a=mmmm 0000

	ora a! add b! mov b,a! push psw ; Save carry
	mov a,m! rar! rar! rar! rar! ani 0000$0011b ; a=0000 00mm
	mov l,a! pop psw! mvi a,0! adc l ; Add carry
	ret

compare$rr:
	mov e,a ; Save cy
	mov a,c! sub m! mov d,a! inx h ; lst byte
	mov a,b! sbb m! inx h ; middle byte
	push a! ora d! mov d,a! pop a
	mov a,e! sbb m ; carry if .fcb(ranrec) > directory
	ret

set$rr:
	mov m,e! dcx h! mov m,b! dcx h! mov m,c! ret

getfilesize:
	; Compute logical file size for current fcb
	; Zero the receiving ranrec field
	call get$rra! push h ; Save position
	mov m,d! inx h! mov m,d! inx h! mov m,d ; =00 00 00
	call search$extnum
	getsize:
		jz setsize
		; current fcb addressed by dptr
		call getdptra! lxi d,reccnt ; ready for compute size
		call compute$rr
		; a=0000 00mm bc = mmmm eeee errr rrrr
		; Compare with memory, larger?
		pop h! push h ; Recall, replace .fcb(ranrec)
		call compare$rr! cnc set$rr
		call searchn
		mvi a,0! sta aret
		jmp getsize
	setsize:

	pop h ; Discard .fcb(ranrec)
	ret

setrandom:
	; Set random record from the current file control block
	xchg! lxi d,nxtrec ; Ready params for computesize
	call compute$rr ; de=info, a=0000 00mm, bc=mmmm eeee errr rrrr
	lxi h,ranrec! dad d ; hl = .fcb(ranrec)
	mov m,c! inx h! mov m,b! inx h! mov m,a ; to ranrec
	ret

disk$select:
	; Select disk info for subsequent input or output ops
	sta adrive
disk$select1: ; called by deblock
	mov m,a ; curdsk = seldsk or adrive
	mov d,a ; Save seldsk in register D for selectdisk call
	lhld dlog! call test$vector ; test$vector does not modify DE
	mov e,a! push d ; Send to seldsk, save for test below
	call selectdisk! pop h ; Recall dlog vector
	jnc sel$error ; returns with C flag set if select ok
	; Is the disk logged in?
	dcr l ; reg l = 1 if so
	ret

tmpselect:
	lxi h,seldsk! mov m,e

curselect:
	lda seldsk! lxi h,curdsk! cmp m! jnz select
	cpi 0ffh! rnz ; return if seldsk ~= ffh

select:
	call disk$select

if MPM
	jnz select1 ; no
	; yes - drive previously logged in
	lhld rlog! call test$vector
	sta rem$drv! ret ; Set rem$drv & return
select1:

else
	rz ; yes - drive previously logged in
endif

	call initialize ; Log in the directory

	; Increment login sequence # if odd
	lhld lsn$add! mov a,m! ani 1! push a! add m! mov m,a
	pop a! cnz set$rlog

	call set$dlog

if MPM
	lxi h,chksiz+1! mov a,m! ral! mvi a,0! jc select2
	lxi d,rlog! call set$cdisk ; rlog=set$cdisk(rlog)
	mvi a,1
select2:
	sta rem$drv
endif

	ret

reselectx:
	xra a! sta high$ext

if BANKED
	sta xfcb$read$only
endif

	jmp reselect1

reselect:
	; Check current fcb to see if reselection necessary
	lxi b,807fh
	lhld info! lxi d,7! xchg! dad d

if BANKED
	; xfcb$read$only = 80h & fcb(7)
	mov a,m! ana b! sta xfcb$read$only
	; fcb(7) = fcb(7) & 7fh
	mov a,m! ana c! mov m,a
endif

if MPM
	; if fcb(8) & 80h
	;    then fcb(8) = fcb(8) & 7fh, high$ext = 60h
	;    else high$ext = fcb(ext) & 0e0h
	inx h! lxi d,4
	mov a,m! ana c! cmp m! mov m,a! mvi a,60h! jnz reselect0
	dad d! mvi a,0e0h! ana m
reselect0:
	sta high$ext
else
	; high$ext = 80h & fcb(8)
	inx h! mov a,m! ana b! sta high$ext
	; fcb(8) = fcb(8) & 7fh
	mov a,m! ana c! mov m,a
endif

	; fcb(ext) = fcb(ext) & 1fh
	call clr$ext
reselect1:

	lxi h,0

if BANKED
	shld make$xfcb ; make$xfcb,find$xfcb = 0
endif
	shld xdcnt ; required by directory hashing

	xra a! sta search$user0
	dcr a! sta resel ; Mark possible reselect
	lhld info! mov a,m ; drive select code
	sta fcbdsk ; save drive code
	ani 1$1111b ; non zero is auto drive select
	dcr a ; Drive code normalized to 0..30, or 255
	sta linfo ; Save drive code
	cpi 0ffh! jz noselect
		; auto select function, seldsk saved above
		sta seldsk
	noselect:
		call curselect
		; Set user code
		lda usrcode ; 0...15
		lhld info! mov m,a
	noselect0:
		; Discard directory BCB's if drive is removable
		; and fx = 15,17,19,22,23,30 etc.
		call tst$log$fxs! cz discard$dir
		; Check for media change on currently slected disk
		call check$media
		; Check for media change on any other disks
		jmp check$all$media

check$media:
	; Check media if DPH media flag set.
	; Is DPH media flag set?
	call test$media$flag! rz ; no
	; Test for media change by reading directory
	; to current high water mark or until media change
	; is detected.
	; First reset DPH media flag & discard directory BCB's
	mvi m,0
	call discard$dir
	lhld dcnt! push h
	call home! call set$end$dir
check$media1:
	mvi c,false! call r$dir
	lxi h,relog! mov a,m! ora a! jz check$media2
	mvi m,0! pop h! lda fx! cpi 48! rz
	call drv$relog! jmp chk$exit$fxs
check$media2:
	call comp$cdr! jc check$media1
	pop h! shld dcnt! ret

check$all$media:
	; This routine checks all logged-in drives for
	; a set DPH media flag and pending buffers.  It reads 
	; the directory for these drives to verify that media 
	; has not changed.  If media has changed, the drives 
	; get reset (but not relogged-in).
	; Is SCB media flag set?
	lxi h,media$flag! mov a,m! ora a! rz ; no
	; Reset SCB media flag
	mvi m,0
	; Test logged-in drives only
	lhld dlog! mvi a,16
chk$am1:
	dcr a! dad h! jnc chk$am2
	; A = drive #
	; Select drive
	push a! push h! lxi h,curdsk! call disk$select
	; Does drive have pending data buffers?
	call test$pending! cnz check$media ; yes
	pop h! pop a
chk$am2:
	ora a! jnz chk$am1
	jmp curselect

test$pending:
	; On return, Z flag reset if buffer pending

	; Does dta$bcba = 0ffffh
	lhld dta$bcba! mov a,l! ana h! inr a! rz ; yes

if BANKED

test$p1:
	; Does bcb addr = 0?
	mov e,m! inx h! mov d,m
	mov a,e! ora d! rz ; yes - no pending buffers
	lxi h,4
else
	lxi d,4
endif

	; Is buffer pending?
	dad d! mov a,m! ora a ; A ~= 0 if so

if BANKED
	rnz ; yes
	; no - advance to next bcb
	lxi h,13! dad d! jmp test$p1
else
	ret
endif

get$dir$mode:
	lhld drvlbla! mov a,m

if not BANKED
	ani 7fh ; Mask off password bit
endif

	ret

if BANKED

chk$password:
	call get$dir$mode! ani 80h! rz

chk$pw:		; Check password
	call get$xfcb! rz ; a = xfcb options
	jmp cmp$pw

chk$pw$error:
	; Disable special searches
	xra a! sta xdcnt+1
	; pw$fcb = dir$xfcb
	call getdptra! xchg
	mvi c,12! lxi h,pw$fcb! push h
	call move! ldax d! inx h! mov m,a! pop d
	lhld info! mov a,m! stax d
	; push original info and xfcb password mode
	; info = .pw$fcb
	push h! xchg! shld info
	; Does fcb(ext = 0, mod = 0) exist?
	call search$namlen! jz chk$pwe2 ; no
	; Does sfcb exist for fcb ?
	call get$dtba$8! ora a! jnz chk$pwe1 ; no 
	xchg! lxi h,pw$mode
	; Is sfcb password mode nonzero?
	mov b,m! ldax d! mov m,a! ora a! jz chk$pwe2 ; no
	; Do password modes match?
	xra b! ani 0e0h! jz chk$pwe1 ; yes
	; no - update xfcb to match sfcb
	call get$xfcb! jz chk$pwe1 ; no xfcb (error)
	lda pw$mode! mov m,a! call nowrite! cz seek$copy
chk$pwe1:
	pop h! shld info
	lda fx! cpi 15! rz! cpi 22! rz

pw$error:	; password error
	mvi a,7! jmp set$aret

chk$pwe2:
	xra a! sta pw$mode
	call nowrite! jnz chk$pwe3
	; Delete xfcb
	call get$xfcb! push a
	lhld info! mov a,m! ori 10h! mov m,a
	pop a! cnz delete$10
chk$pwe3:
	; Restore info
	pop h! shld info! ret

cmp$pw:		; Compare passwords
	inx h! mov b,m
	mov a,b! ora a! jnz cmp$pw2
	mov d,h! mov e,l! inx h! inx h
	mvi c,9
cmp$pw1:
	inx h! mov a,m! dcr c! rz
	ora a! jz cmp$pw1
	cpi 20h! jz cmp$pw1
	xchg
cmp$pw2:
	lxi d,(23-ubytes)! dad d! xchg
	lhld xdmaad! mvi c,8
cmp$pw3:
	ldax d! xra b! cmp m! jnz cmp$pw4
	dcx d! inx h! dcr c! jnz cmp$pw3
	ret
cmp$pw4:
	dcx d! dcr c! jnz cmp$pw4
	inx d

if MPM
	call get$df$pwa! inr a! jnz cmp$pw5
	inr a! ret
cmp$pw5:

else
	lxi h,df$password
endif

	mvi c,8! jmp compare

if MPM

get$df$pwa:	; a = ff => no df pwa
	call rlr! lxi b,console! dad b
	mov a,m! cpi 16! mvi a,0ffh! rnc
	mov a,m! add a! add a! add a
	mvi h,0! mov l,a! lxi b,dfpassword! dad b
	ret
endif

set$pw:		; Set password in xfcb
	push h ; Save .xfcb(ex) 
	lxi b,8 ; b = 0, c = 8
	lxi d,(23-extnum)! dad d
	xchg! lhld xdmaad
set$pw0:
	xra a! push a
set$pw1:
	mov a,m! stax d! ora a! jz set$pw2
	cpi 20h! jz set$pw2
	inx sp! inx sp! push a
set$pw2:
	add b! mov b,a
	dcx d! inx h! dcr c! jnz set$pw1
	pop a! ora b! pop h! jnz set$pw3
	; is fx = 100 (directory label)?
	lda fx! cpi 100! jz set$pw3 ; yes
	mvi m,0 ; zero xfcb(ex) - no password
set$pw3:
	inx d! mvi c,8
set$pw4:
	ldax d! xra b! stax d! inx d! dcr c! jnz set$pw4
	inx h! ret

get$xfcb:
	lhld info! mov a,m! push a
	ori 010h! mov m,a
	call search$extnum! mvi a,0! sta lret
	lhld info! pop b! mov m,b! rz
get$xfcb1:
	call getdptra! xchg
	lxi h,extnum! dad d! mov a,m! ani 0e0h! ori 1
	ret

adjust$dmaad:
	push h! lhld xdmaad! dad d
	shld xdmaad! pop h! ret

init$xfcb:
	call setcdr ; may have extended the directory
	lxi b,1014h ; b=10h, c=20
init$xfcb0:
	; b = fcb(0) logical or mask
	; c = zero count
	push b
	call getdptra! xchg! lhld info! xchg
	; Zero extnum and modnum
	ldax d! ora b! mov m,a! inx d! inx h
	mvi c,11! call move! pop b! inr c
init$xfcb1:
	dcr c! rz
	mvi m,0! inx h! jmp init$xfcb1

chk$xfcb$password:
	call get$xfcb1
chk$xfcb$password1:
	push h! call cmp$pw! pop h! ret

endif

stamp1:
	mvi c,0! jmp stamp3
stamp2:
	mvi c,4
stamp3:
	call get$dtba! ora a! rnz
	lxi d,seek$copy! push d
stamp4:

if MPM
	push h
	call get$stamp$add! xchg
	pop h
else
	lxi d,stamp
endif

	push h! push d
	mvi c,0! call timef ; does not modify hl,de
	mvi c,4! call compare
	mvi c,4! pop d! pop h! jnz move
	pop h! ret

stamp5:
	call getdptra! dad b! lxi d,func$ret! push d
	jmp stamp4

if BANKED

get$dtba$8:
	mvi c,8
endif

get$dtba:
	; c = offset of sfcb subfield (0,4,8)
	; Return with a = 0 if sfcb exists

	; Does fcb occupy 4th item of sector?
	lda dcnt! ani 3! cpi 3! rz ; yes
	mov b,a
	lhld buffa! lxi d,96! dad d
	; Does sfcb reside in 4th directory item?
	mov a,m! sui 21h! rnz ; no
	; hl = hl + 10*lret + 1 + c
	mov a,b! add a! mov e,a! add a! add a! add e
	inr a! add c! mov e,a! dad d! xra a
	ret

qstamp:
	; Is fcb 1st logical fcb for file?
	call qdirfcb1! rnz ; no
qstamp1:
	; Does directory label specify requested stamp?
	lhld drvlbla! mov a,c! ana m! jnz nowrite ; yes - verify drive r/w
	inr a! ret ; no - return with Z flag reset

qdirfcb1:
	; Routine to determine if fcb is 1st directory fcb
	; for file
	; Is fcb(ext) & ~extmsk & 00011111b = 0?
	lda extmsk! ori 1110$0000b! cma! mov b,a
	call getexta! mov a,m! ana b! rnz ; no
	; is fcb(mod) & 0011$1111B = 0?
	inx h! inx h! mov a,m! ani 3fh! ret ; Z flag set if zero

update$stamp:
	; Is update stamping requested on drive?
	mvi c,0010$0000b! call qstamp1! rnz ; no
	; Has file been written to since it was opened?
	call getmodnum! ani 40h! rnz ; yes - update stamp performed
	; Search for 1st dir fcb
	call getexta! mov b,m! mvi m,0! push h
	inx h! inx h! mov c,m! mvi m,0! push b
	; Search from beginning of directory
	call search$namlen
	; Perform update stamp if dir fcb 1 found
	cnz stamp2
	xra a! sta lret
	; Restore fcb extent and module fields
	pop b! pop h! mov m,b! inx h! inx h! mov m,c! ret

if MPM

pack$sdcnt:

;packed$dcnt = dblk(low 15 bits) || dcnt(low 9 bits)

;	if sdblk = 0 then dblk = shr(sdcnt,blkshf+2)
;		     else dblk = sdblk
;	dcnt = sdcnt & (blkmsk || '11'b)
;
;	packed$dcnt format (24 bits)
;
;	12345678 12345678 12345678
;	23456789 .......1 ........ sdcnt (low 9 bits)
;	........ 9abcdef. 12345678 sdblk (low 15 bits)
;
	lhld sdblk! mov a,h! ora l! jnz pack$sdcnt1
	lda blkshf! adi 2! mov c,a! lhld sdcnt
	call hlrotr
pack$sdcnt1:
	dad h! xchg! lxi h,sdcnt! mvi b,1
	lda blkmsk! ral! ora b! ral! ora b
	ana m! sta packed$dcnt
	lda blkshf! cpi 7! jnz pack$sdcnt2
	inx h! mov a,m! ana b! jz pack$sdcnt2
	mov a,e! ora b! mov e,a
pack$sdcnt2:
	xchg! shld packed$dcnt+1
	ret

; olist element = link(2) || atts(1) || dcnt(3) || 
;		 pdaddr(2) || opncnt(2)
;
;	link = 0 -> end of list
;
;	atts - 80 - open in locked mode
;	       40 - open in unlocked mode
;	       20 - open in read/only mode
;	       10 - deleted item
;	       0n - drive code (0-f)
;
;	dcnt = packed sdcnt+sdblk
;	pdaddr = process descriptor addr
;	opncnt = # of open calls - # of close calls
;		 olist item freed by close when opncnt = 0
;
; llist element = link(2) || drive(1) || arecord(3) || 
;	         pdaddr(2) || .olist$item(2)
;
;	link = 0 -> end of list
;
;	drive - 0n - drive code (0-f)
;
;	arecord = record number of locked record
;	pdaddr = process descriptor addr
;	.olist$item = address of file's olist item

search$olist:
	lxi h,open$root! jmp srch$list0
search$llist:
	lxi h,lock$root! jmp srch$list0
searchn$list:
	lhld cur$pos
srch$list0:
	shld prv$pos

; search$olist, search$llist, searchn$list conventions
;
;	b = 0 -> return next item
;	b = 1 -> search for matching drive
; 	b = 3 -> search for matching dcnt
;	b = 5 -> search for matching dcnt + pdaddr
;	if found then z flag is set
;	              prv$pos -> previous list element
;		      cur$pos -> found list element
;		      hl -> found list element
;	else prv$pos -> list element to insert after
;
;	olist and llist are maintained in drive order

srch$list1:
	mov e,m! inx h! mov d,m! xchg
	mov a,l! ora h! jz srch$list3
	xra a! cmp b! jz srch$list6
	inx h! inx h!
	lxi d,curdsk! mov a,m! ani 0fh! mov c,a
	ldax d! sub c! jnz srch$list4
	mov a,b! dcr a! jz srch$list5
	mov c,b! push h
	inx d! inx h! call compare
	pop h! jz srch$list5
srch$list2:
	dcx h! dcx h
	shld prv$pos! jmp srch$list1
srch$list3:
	inr a! ret
srch$list4:
	jnc srch$list2
srch$list5:
	dcx h! dcx h
srch$list6:
	shld cur$pos! ret

delete$item: ; hl -> item to be deleted
	di
	push d! push h
	mov e,m! inx h! mov d,m
	lhld prv$pos! shld cur$pos
	; prv$pos.link = delete$item.link
	mov m,e! inx h! mov m,d

	lhld free$root! xchg
	; free$root = .delete$item
	pop h! shld free$root
	; delete$item.link = previous free$root
	mov m,e! inx h! mov m,d
	pop d! ei! ret

create$item: ; hl -> new item if successful
	     ; z flag set if no free items
	lhld free$root! mov a,l! ora h! rz
	push d! push h! shld cur$pos
	mov e,m! inx h! mov d,m
	; free$root = free$root.link
	xchg! shld free$root

	lhld prv$pos
	mov e,m! inx h! mov d,m
	pop h
	; create$item.link = prv$pos.link
	mov m,e! inx h! mov m,d! dcx h
	xchg! lhld prv$pos
	; prv$pos.link = .create$item
	mov m,e! inx h! mov m,d! xchg
	pop d! ret

set$olist$item:
	; a = attributes
	; hl = olist entry address
	inx h! inx h
	mov b,a! lxi d,curdsk! ldax d! ora b
	mov m,a! inx h! inx d
	mvi c,5! call move
	xra a! mov m,a! inx h! mov m,a! ret

set$sdcnt:
 	mvi a,0ffh! sta sdcnt+1! ret

tst$olist:
	mvi a,0c9h! sta chk$olist05! jmp chk$olist0
chk$olist:
	xra a! sta chk$olist05
chk$olist0:
	lxi d,dcnt! lxi h,sdcnt! mvi c,4! call move
	call pack$sdcnt! mvi b,3! call search$olist! rnz
	pop d ; pop return address
	inx h! inx h
	mov a,m! ani 80h! jz openx06 
	dcx h! dcx h
	push d! push h
	call compare$pds! pop h! pop d! jnz openx06
	push d ; Restore return address
chk$olist05:
	nop ; tst$olist changes this instr to ret
	call delete$item! lda pdcnt
chk$olist1:
	adi 16! jz chk$olist1
	sta pdcnt

	push a! call rlr
	lxi b,pdcnt$off! dad b! pop a
	mov m,a! ret

remove$files:	; bc = pdaddr
	lhld cur$pos! push h
	lhld prv$pos! push h
	mov d,b! mov e,c! lxi h,open$root! shld cur$pos
remove$file1:
	mvi b,0! push d! call searchn$list! pop d! jnz remove$file2
	lxi b,6! call tst$tbl$lmt! jnz remove$file1
	inx h! inx h! mov a,m! ori 10h! mov m,a
	sta deleted$files
	jmp remove$file1
remove$file2:
	pop h! shld prv$pos
	pop h! shld cur$pos
	ret

delete$files:
	lxi h,open$root! shld cur$pos
delete$file1:
	mvi b,0! call search$nlist! rnz
	inx h! inx h! mov a,m! ani 10h! jz delete$file1
	dcx h! dcx h! call remove$locks! call delete$item
	jmp delete$file1

flush$files:
	lxi h,flushed! mov a,m! ora a! rnz
	inr m
flush$file0:
	lxi h,open$root! shld cur$pos
flush$file1:
	mvi b,1! call searchn$list! rnz
	push h! call remove$locks! call delete$item! pop h
	lxi d,6! dad d! mov e,m! inx h! mov d,m
	lxi h,pdcnt$off! dad d! mov a,m! ani 1! jnz flush$file1
	mov a,m! ori 1! mov m,a
	lhld pdaddr! mvi c,2! call compare! jnz flush$file1
	lda pdcnt! adi 10h! sta pdcnt! jmp flush$file1

free$files:
	; free$mode = 1 - remove curdsk files for process
	;	      0 - remove all files for process
	lhld pdaddr! xchg! lxi h,open$root! shld curpos
free$files1:
	lda free$mode! mov b,a
	push d! call searchn$list! pop d! rnz
	lxi b,6! call tst$tbl$lmt! jnz free$files1
	push h! inx h! inx h! inx h
	call test$ffff! jnz free$files2
	call test$ffff! jz free$files3
free$files2:
	mvi a,0ffh! sta incr$pdcnt
free$files3:
	pop h! call remove$locks! call delete$item
	jmp free$files1

remove$locks:
	shld file$id
	inx h! inx h! mov a,m! ani 40h! jz remove$lock3
	push d! lhld prv$pos! push h
	lhld file$id! xchg! lxi h,lock$root! shld cur$pos
remove$lock1:
	mvi b,0! push d! call searchn$list! pop d
	jnz remove$lock2
	lxi b,8! call tst$tbl$lmt! jnz remove$lock1
	call delete$item
	jmp remove$lock1
remove$lock2:
	pop h! shld prv$pos! pop d
remove$lock3:
	lhld file$id! ret

tst$tbl$lmt:
	push h! dad b
	mov a,m! inx h! mov h,m
	sub e! jnz tst$tbl$lmt1
	mov a,h! sub d
tst$tbl$lmt1:
	pop h! ret

create$olist$item:
	mvi b,1! call search$olist
	di
	call create$item! lda attributes! call set$olist$item
	ei
	ret

count$opens:
	xra a! sta open$cnt
	lhld pdaddr! xchg! lxi h,open$root! shld curpos
count$open1:
	mvi b,0! push d! call searchn$list! pop d! jnz count$open2
	lxi b,6! call tst$tbl$lmt! jnz count$open1
	lda open$cnt! inr a! sta open$cnt
	jmp count$open1
count$open2:
	lxi h,open$max! lda open$cnt! ret

count$locks:
	xra a! sta lock$cnt
	xchg! lxi h,lock$root! shld cur$pos
count$lock1:
	mvi b,0! push d! call searchn$list! pop d! rnz
	lxi b,8! call tst$tbl$lmt! jnz count$lock1
	lda lock$cnt! inr a! sta lock$cnt
	jmp count$lock1

check$free:
	lda mult$cnt! mov e,a
	mvi d,0! lxi h,free$root! shld cur$pos
check$free1:
	mvi b,0! push d! call searchn$list! pop d! jnz check$free2
	inr d! mov a,d! sub e! jc check$free1
	ret
check$free2:
	pop h! mvi a,14! jmp sta$ret

lock:				; record lock and unlock 
	call reselect! call check$fcb
	call test$unlocked
	rz ; file not opened in unlocked mode
	lhld xdmaad! mov e,m! inx h! mov d,m
	xchg! inx h! inx h
	mov a,m! mov b,a! lda curdsk! sub b
	ani 0fh! jnz lock8 ; invalid file id
	mov a,b! ani 40h! jz lock8 ; invalid file id
	dcx h! dcx h! shld file$id
	lda lock$unlock! inr a! jnz lock1 ; jmp if unlock
	call count$locks
	lda lock$cnt! mov b,a
	lda mult$cnt! add b! mov b,a
	lda lock$max! cmp b
	mvi a,12! jc sta$ret ; too many locks by this process
	call check$free
lock1:
	call save$rr! lxi h,lock9! push h! lda mult$cnt
lock2:
	push a! call get$lock$add
	lda lock$unlock! inr a! jnz lock3
	call test$lock
lock3:
	pop a! dcr a! jz lock4
	call incr$rr! jmp lock2
lock4:
	call reset$rr! lda mult$cnt
lock5:
	push a! call get$lock$add
	lda lock$unlock! inr a! jnz lock6
	call set$lock! jmp lock7
lock6:
	call free$lock
lock7:
	pop a! dcr a! rz
	call incr$rr! jmp lock5
lock8:
	mvi a,13! jmp sta$ret ; invalid file id
lock9:
	call reset$rr! ret

get$lock$add:
	lxi h,0! dad sp! shld lock$sp
	mvi a,0ffh! sta lock$shell
	call rseek
	xra a! sta lock$shell
	call getfcb
	lhld aret! mov a,l! ora a! jnz lock$err
	call index! lxi h,1! jz lock$err
	call atran! ret

lock$perr:
	xra a! sta lock$shell
	xchg! lhld lock$sp! sphl! xchg
lock$err:
	pop d ; Discard return address
	pop b ; b = mult$cnt-# recs processed
	lda mult$cnt! sub b
	add a! add a! add a! add a
	ora h! mov h,a! mov b,a
	shld aret! ret

test$lock:
	call move$arecord
	mvi b,3! call search$llist! rnz
	call compare$pds! rz
	lxi h,8! jmp lock$err

set$lock:
	call move$arecord
	mvi b,1! call search$llist
	di
	call create$item
	xra a! call set$olist$item
	xchg! lhld file$id! xchg
	mov m,d! dcx h! mov m,e
	ei! ret

free$lock:
	call move$arecord
	mvi b,5! call search$llist! rnz
free$lock0:
	call delete$item
	mvi b,5! call searchn$list! rnz
	jmp free$lock0

compare$pds:
	lxi d,6! dad d! xchg
	lxi h,pdaddr! mvi c,2! jmp compare


move$arecord:
	lxi d,arecord! lxi h,packed$dcnt


fix$olist$item:
	lxi d,xdcnt! lxi h,sdcnt
	; Is xdblk,xdcnt < sdblk,sdcnt
	mvi c,4! ora a!
fix$ol1:
	ldax d! sbb m! inx h! inx d! dcr c! jnz fix$ol1
	rnc
	; yes - update olist entry
	call swap! call sdcnt$eq$xdcnt
	lxi h,open$root! shld cur$pos
	; Find file's olist entry
fix$ol2:
	call swap! call pack$sdcnt! call swap
	mvi b,3! call searchn$list! rnz
	; Update olist entry with new dcnt value
	push h! call pack$sdcnt! pop h
	inx h! inx h! inx h! lxi d,packed$dcnt
	mvi c,3! call move! jmp fix$ol2

hl$eq$hl$and$de:
	mov a,l! ana e! mov l,a
	mov a,h! ana d! mov h,a
	ret

remove$drive:
	xchg! lda curdsk! mov c,a! lxi h,1
	call hlrotl
	mov a,l! cma! ana e! mov e,a
	mov a,h! cma! ana d! mov d,a
	xchg! ret

diskreset:
	lxi h,0! shld ntlog
	xra a! sta set$ro$flag
	lhld info
intrnldiskreset:
	xchg! lhld open$root! mov a,h! ora l! rz
	xchg! lda curdsk! push a! mvi b,0
dskrst1:
	mov a,l! rar! jc dskrst3
dskrst2:
	mvi c,1! call hlrotr! inr b
	mov a,h! ora l! jnz dskrst1
	pop a! sta curdsk
	lhld ntlog! xchg! lhld tlog
	mov a,l! ora e! mov l,a
	mov a,h! ora d! mov h,a! shld tlog
	inr a! ret
dskrst3:
	push b! push h! mov a,b! sta curdsk
	lhld rlog! call test$vector1! push a
	lhld rodsk! lda curdsk! call test$vector1! mov b,a
	pop h! lda set$ro$flag! ora b! ora h! sta check$disk
	lxi h,open$root! shld cur$pos
dskrst4:
	mvi b,1! call searchn$list! jnz dskrst6
	lda check$disk! ora a! jz dskrst5
	push h! call compare$pds! jz dskrst45
	pop h! xra a! xchg! jmp dskrst6
dskrst45:
	lxi d,ntlog! call set$cdisk
	pop h! jmp dskrst4
dskrst5:
	lhld info! call remove$drive! shld info
	ori 1
dskrst6:
	pop h! pop b! jnz dskrst2

	; error - olist item exists for another process
	; for removable drive to be reset
	pop a! sta curdsk! mov a,b! adi 41h ; a = ascii drive
	lxi h,6! dad d! mov c,m! inx h! mov b,m ; bc = pdaddr
	push psw! call test$error$mode! pop d! jnz dskrst7
	mov a,d

	push b! push psw
	call rlr! lxi d,console! dad d! mov d,m ; d = console #
	lxi b,deniedmsg! call xprint
	pop psw! mov c,a! call conoutx
	mvi c,':'! call conoutx
	lxi b,cnsmsg! call xprint
	pop h! push h! lxi b,console! dad b
	mov a,m! adi '0'! mov c,a! call conoutx
	lxi b,progmsg! call xprint
	pop h! call dsplynm

dskrst7:
	pop h ; Remove return addr from diskreset
	lxi h,0ffffh! shld aret ; Flag the error
	ret

deniedmsg:
	db cr,lf,'disk reset denied, drive ',0
cnsmsg:
	db ' console ',0
progmsg:
	db ' program ',0
endif

;
;	individual function handlers
;

func12:
	; Return version number

if MPM
	lxi h,0100h+dvers! jmp sthl$ret
else
	lda version! jmp sta$ret ; lret = dvers (high = 00)
endif

func13:

if MPM
	lhld dlog! shld info
	call diskreset! jz reset$all
	call reset$37
	jmp func13$cont
reset$all:

	; Reset disk system - initialize to disk 0
	lxi h,0! shld rodsk! shld dlog

	shld rlog! shld tlog
func13$cont:
	mvi a,0ffh! sta curdsk
else
	lxi h,0ffffh! call reset$37x
endif
	xra a! sta olddsk ; Note that usrcode remains unchanged

if MPM
	xra a! call getmemseg ; a = mem seg tbl index
	ora a! rz
	inr a! rz
	call rlradr! lxi b,msegtbl-rlros! dad b
	add a! add a! mov e,a! mvi d,0! dad d
	mov h,m! mvi l,80h
	jmp intrnlsetdma
else
	lxi h,tbuff! shld dmaad ; dmaad = tbuff
        jmp setdata ; to data dma address
endif

func14:	

if MPM
	call tmpselect ; seldsk = reg e
	call rlr! lxi b,diskselect! dad b
	mov a,m! ani 0fh! rrc! rrc! rrc! rrc
	mov b,a! lda seldsk! ora b! rrc! rrc! rrc! rrc
	mov m,a! ret
else
	call tmpselect ; seldsk = reg e
	lda seldsk! sta olddsk! ret
endif

func15:
	; Open file
	call clrmodnum ; Clear the module number

if MPM
	call reselect
	xra a! sta make$flag
	call set$sdcnt
	lxi h,open$file! push h
	mvi a,0c9h! sta check$fcb4
	call check$fcb1
	pop h! lda high$ext! cpi 060h! jnz open$file
	call home! call set$end$dir
	jmp open$user$zero
open$file:
	call set$sdcnt
	call reset$chksum$fcb ; Set invalid check sum
else
	call reselectx
endif

	call check$wild ; Check for wild chars in fcb

if MPM

	call get$atts! ani 1100$0000b ; a = attributes
 	cpi 1100$0000b! jnz att$ok
	ani 0100$0000b ; Mask off unlock mode 
att$ok:
	sta high$ext
	mov b,a! ora a! rar! jnz att$set
	mvi a,80h
att$set:
	sta attributes! mov a,b
	ani 80h! jnz call$open
endif

	lda usrcode! ora a! jz call$open 
	mvi a,0feh! sta xdcnt+1! inr a! sta search$user0

if MPM
	sta sdcnt0+1
endif

call$open:
	call open! call openx ; returns if unsuccessful, a = 0
	lxi h,search$user0! cmp m! rz
	mov m,a! lda xdcnt+1! cpi 0feh! rz 
;
;	file exists under user 0
;

if MPM
	call swap
endif

	call set$dcnt$dblk

if MPM
	mvi a,0110$0000b
else
	mvi a,80h
endif

	sta high$ext
open$user$zero:
	; Set fcb user # to zero
	lhld info! mvi m,0
	mvi c,namlen! call searchi! call searchn
	call open1 ; Attempt reopen under user zero
	call openx ; openx returns only if unsuccessful
	ret
openx:
	call end$of$dir! rz
	call getfcba! mov a,m! inr a! jnz openxa
	dcx d! dcx d! ldax d! mov m,a
openxa:
	; open successful
	pop h ; Discard return address
	; Was file opened under user 0 after unsuccessful
	; attempt to open under user n?

if MPM
	lda high$ext! cpi 060h! jz openx00 ; yes
	; Was file opened in locked mode?
	ora a! jnz openx0 ; no
	; does user = zero?
	lhld info! ora m! jnz openx0 ; no
	; Does file have read/only attribute set?
	call rotest! jnc openx0 ; no
	; Does file have system attribute set?
	inx h! mov a,m! ral! jnc openx0 ; no

	; Force open mode to read/only mode and set user 0 flag
	; if file opened in locked mode, user = 0, and
	; file has read/only and system attributes set

openx00:

else
	lda high$ext! ral! jnc openx0
endif

	; Is file under user 0 a system file ?

if MPM
	mvi a,20h! sta attributes
endif

	lhld info! lxi d,10! dad d
	mov a,m! ani 80h! jnz openx0 ; yes - open successful
	; open fails
	sta high$ext! jmp lret$eq$ff
openx0:

if MPM
	call reset$chksum$fcb
else
	call set$lsn
endif

if BANKED

	; Are passwords enabled on drive?
	call get$dir$mode! ani 80h! jz openx1a ; no
	; Is this 1st dir fcb?
	call qdirfcb1! jnz openx0a ; no
	; Does sfcb exist?
	call get$dtba$8! ora a! jnz openx0a ; no
	; Is sfcb password mode read or write?
	mov a,m! ani 0c0h! jz openx1a ; no
	; Does xfcb exist?
	call xdcnt$eq$dcnt
	call get$xfcb! jnz openx0b ; yes
	; no - set sfcb password mode to zero
	call restore$dir$fcb! rz ; (error)
	; Does sfcb still exist?
	call get$dtba$8! ora a! jnz openx1a ; no (error)
	; sfcb password mode = 0
	mov m,a
	; update sfcb
	call nowrite! cz seek$copy
	jmp openx1a
openx0a:
	call xdcnt$eq$dcnt
	; Does xfcb exist?
	call get$xfcb! jz openx1 ; no
openx0b:
	; yes - check password
	call cmp$pw! jz openx1
	call chk$pw$error
	lda pw$mode! ani 0c0h! jz openx1
	ani 80h! jnz pw$error
	mvi a,080h! sta xfcb$read$only
openx1:
	call restore$dir$fcb! rz ; (error)
openx1a:
	call set$lsn

if MPM
	call pack$sdcnt
	; Is this file currently open?
	mvi b,3! call search$olist! jz openx04
openx01:
	; no - is olist full?
	lhld free$root! mov a,l! ora h! jnz openx03
	; yes - error
openx02:
	mvi a,11! jmp set$aret
openx03:
	; Has process exceeded open file maximum?
	call count$opens! sub m! jc openx035
	; yes - error
openx034:
	mvi a,10! jmp set$aret
openx035:
	; Create new olist element
	call create$olist$item
	jmp openx08
openx04:
	; Do file attributes match?
	inx h! inx h
	lda attributes! ora m! cmp m! jnz openx06
	; yes - is open mode locked?
	ani 80h! jnz openx07
	; no - has this file been opened by this process?
	lhld prv$pos! shld cur$pos
	mvi b,5! call searchn$list! jnz openx01
openx05:
	; yes - increment open file count
	lxi d,8! dad d! inr m! jnz openx08
	; count overflow
	inx h! inr m! jmp openx08
openx06:
	; error - file opened by another process in imcompatible mode
	mvi a,5! jmp set$aret
openx07:
	; Does this olist item belong to this process?
	dcx h! dcx h! push h
	call compare$pds
	pop h! jnz openx06 ; no - error
	jmp openx05 ; yes
openx08:; Wopen ok
	; Was file opened in unlocked mode?
	lda attributes! ani 40h! jz openx09 ; no
	; yes - return .olist$item in ranrec field of fcb
	call get$rra
	lxi d,cur$pos! mvi c,2! call move
openx09:
	call set$fcb$cks$flag
	lda make$flag! ora a! rnz
endif
endif

	mvi c,0100$0000b
openx2:
	call qstamp! cz stamp1
	lxi d,olog! jmp set$cdisk

func16:
	; Close file
	call reselect

if MPM
	call get$atts! sta attributes
	lxi h,close00! push h
	mvi a,0c9h! sta check$fcb4
	call check$fcb1! pop h
	call set$sdcnt
	call getmodnum! ani 80h! jnz close01
	call close! jmp close02
close00:
	mvi a,6! jmp set$aret
close01:
	mvi a,0ffh! sta dont$close! call close1
close02:
else
	call set$lsn
	call chek$fcb! call close
endif

	lda lret! inr a! rz

	jmp flush ; Flush buffers

if MPM
	lda attributes! ral! rc
	call pack$sdcnt
	; Find olist item for this process & file
	mvi b,5! call search$olist! jnz close03
	; Decrement open count
	push h! lxi d,8! dad d
	mov a,m! sui 1! mov m,a! inx h
	mov a,m! sbi 0! mov m,a! dcx h
	; Is open count = 0ffffh
	call test$ffff! pop h! jnz close03
	; yes - remove file's olist entry
	shld file$id! call delete$item
	call reset$chksum$fcb
	; if unlocked file, remove file's locktbl entries
	call test$unlocked! jz close03
	lhld file$id! call remove$locks
close03:
	ret

endif

func17:
	; Search for first occurrence of a file
	xchg! xra a
csearch:
	push a
	mov a,m! cpi '?'! jnz csearch1 ; no reselect if ?
	call curselect! call noselect0! mvi c,0! jmp csearch3
csearch1:
	call getexta! mov a,m! cpi '?'! jz csearch2
	call clr$ext! call clrmodnum
csearch2:
	call reselectx
	mvi c,namlen
csearch3:
	pop a! push a! jz csearch4
	; dcnt = dcnt & 0fch
	lhld dcnt! push h! mvi a,0fch
	ana l! mov l,a! shld dcnt
	call rd$dir
	pop h! shld dcnt
csearch4:
	pop a
	lxi h,dir$to$user
	push h
	jz search
	lda searchl! mov c,a! call searchi! jmp searchn

func18:
	; Search for next occurrence of a file name

if BANKED
	xchg! shld searcha
else
	lhld searcha! shld info
endif

	ori 1! jmp csearch

func19:
	; Delete a file
	call reselectx
	jmp delete

func20:
	; Read a file
	call reselect
	call check$fcb
	jmp seqdiskread

func21:
	; Write a file
	call reselect
	call check$fcb
	jmp seqdiskwrite

func22:
	; Make a file

if BANKED
	call get$atts! sta attributes
endif

	call clr$ext
	call clrmodnum ; fcb mod = 0
	call reselectx

if MPM
	call reset$chksum$fcb
endif

	call check$wild
	call set$xdcnt ; Reset xdcnt for make

if MPM
	call set$sdcnt
endif

	call open ; Verify file does not already exist

if MPM
	call reset$chksum$fcb
endif

	; Does dir fcb for fcb exist?
	; ora a required to reset carry
	call end$of$dir! ora a! jz makea0 ; no
	; Is dir$ext < fcb(ext)?
	call get$dir$ext! cmp m! jnc file$exists ; no
makea0:
	push a ; carry set if dir fcb already exists

if MPM
	lda attributes! ani 80h! rrc! jnz makex00
	mvi a,80h
makex00:
	sta make$flag
	lda sdcnt+1! inr a! jz makex01
	call pack$sdcnt
	mvi b,3! call search$olist! jz make$x02
makex01:
	lhld free$root! mov a,l! ora h! jz openx02
	jmp makex03
makex02:
	inx h! inx h
	lda makeflag! ana m! jz openx06
	dcx h! dcx h! call compare$pds! jz makex03
	lda makeflag! ral! jc openx06
makex03:

endif

if BANKED
	; Is fcb 1st fcb for file?
	call qdirfcb1! jz makex04 ; yes
	; no - does dir lbl require passwords?
	call get$dir$mode! ani 80h! jz makex04
	; no - does xfcb exist with mode 1 or 2 password?
	call get$xfcb! jz makex04
	; yes - check password
	call chk$xfcb$password1! jz makex04
	; Verify password error
	call chk$pw$error
	lda pw$mode! ani 0c0h! jnz pw$error
makex04:

endif

	; carry on stack indicates a make not required because
	; of extent folding
	pop a! cnc make

if MPM
	call reset$chksum$fcb
endif

	; end$of$dir call either applies to above make or open call
	call end$of$dir! rz ; Return if make unsuccessful

if not MPM
	call set$lsn
endif

if BANKED

	; Are passwords activated by dir lbl?
	call get$dir$mode! ani 80h! jz make3a
	; Did user set password attribute?
	lda attributes! ani 40h! jz make3a
	; Is fcb file's 1st logical fcb?
	call qdirfcb1! jnz make3a
	; yes - does xfcb already exist for file
	call xdcnt$eq$dcnt
	call get$xfcb! jnz make00 ; yes
	; Attempt to make xfcb
	mvi a,0ffh! sta make$xfcb! call make! jnz make00
	; xfcb make failed - delete fcb that was created above
	call search$namlen
	call delete10! jmp lret$eq$ff ; Return with a = 0ffh

make00:
	call init$xfcb ; Initialize xfcb
	; Get password mode from dma + 8
	xchg! lhld xdmaad! lxi b,8! dad b! xchg
	ldax d! ani 0e0h! jnz make2
	mvi a,080h ; default password mode is read protect
make2:
	sta pw$mode
	; Set xfcb password mode field
	push a! call getxfcb1! pop a! mov m,a
	; Set xfcb password and password checksum
	; Fix hash table and write xfcb
	call set$pw! mov m,b! call sdl3
	; Return to fcb
	call restore$dir$fcb! rz
	; Does sfcb exist?
	mvi c,8! call getdtba! ora a! jnz make3a ; no
	; Place password mode in sfcb if sfcb exists
	lda pw$mode! mov m,a! call seek$copy
	call set$lsn
endif

make3a:
	mvi c,0101$0000b

if MPM
	call openx2
	lda make$flag! sta attributes
	ani 40h! ral! sta high$ext
	lda sdcnt+1! inr a! jnz makexx02
	call sdcnt$eq$xdcnt! call pack$sdcnt
	jmp openx03
makexx02:
	call fix$olist$item! jmp openx1
	jmp set$fcb$cks$flag
else
	call openx2
	mvi c,0010$0000b! call qstamp! rnz
	call stamp2! jmp set$filewf
endif

file$exists:
	mvi a,8
set$aret:
	mov c,a! sta aret+1! call lret$eq$ff

if MPM
	call test$error$mode! jnz goback
else
	jmp goerr1
endif

if MPM
	mov a,c! sui 3
	mov l,a! mvi h,0! dad h
	lxi d,xerr$list! dad d
	mov e,m! inx h! mov d,m
	xchg! jmp report$err
endif

func23:
	; Rename a file
	call reselectx
	jmp rename

func24:
	; Return the login vector
	lhld dlog! jmp sthl$ret	

func25:
	; Return selected disk number
	lda seldsk! jmp sta$ret

func26:

if MPM
	; Save dma address in process descriptor
	lhld info
intrnlsetdma:
	xchg
	call rlr! lxi b,disksetdma! dad b
	mov m,e! inx h! mov m,d
endif

	; Set the subsequent dma address to info
	xchg! shld dmaad ; dmaad = info
        jmp setdata ; to data dma address

func27:
	; Return the login vector address
	call curselect
	lhld alloca! jmp sthl$ret

if MPM

func28:
	; Write protect current disk
	; first check for open files on disk
	mvi a,0ffh! sta set$ro$flag
	lda seldsk! mov c,a! lxi h,0001h
	call hlrotl! call intrnldiskreset
	jmp set$ro
else

func28:	equ	set$ro ; Write protect current disk

endif

func29:
	; Return r/o bit vector
	lhld rodsk! jmp sthl$ret

func30:
	; Set file indicators
	call check$wild
	call reselectx
	call indicators
	jmp copy$dirloc ; lret=dirloc

func31:
	; Return address of disk parameter block
	call curselect
	lhld dpbaddr
sthl$ret:
 	shld aret! ret

func32:
	; Set user code
        lda linfo! cpi 0ffh! jnz setusrcode
		; Interrogate user code instead
		lda usrcode! jmp sta$ret ; lret=usrcode	
	setusrcode:
		ani 0fh! sta usrcode

if MPM
		push a
		call rlr! lxi b,diskselect! dad b
		pop b
		mov a,m! ani 0f0h! ora b! mov m,a
endif

		ret

func33:
	; Random disk read operation
	call reselect
	call check$fcb
	jmp randiskread ; to perform the disk read

func34:
	; Random disk write operation
	call reselect
	call check$fcb
	jmp randiskwrite ; to perform the disk write

func35:
	; Return file size (0-262,144)
	call reselect
	jmp getfilesize

func36	equ setrandom ; Set random record

func37:	
	; Drive reset

if MPM
	call diskreset
reset$37:
	lhld info
else
	xchg
endif

reset$37x:
	mov a,l! cma! mov e,a! mov a,h! cma
	lhld dlog! ana h! mov d,a! mov a,l! ana e
	mov e,a! lhld rodsk! xchg! shld dlog

if MPM
	push h! call hl$eq$hl$and$de
else
	mov a,l! ana e! mov l,a
	mov a,h! ana d! mov h,a
endif

	shld rodsk

if MPM
	pop h! xchg! lhld rlog! call hl$eq$hl$and$de! shld rlog
endif

	; Force select call in next curselect
	mvi a,0ffh! sta curdsk! ret

if MPM

func38:				
	; Access drive

	lxi h,packed$dcnt! mvi a,0ffh
	mov m,a! inx h! mov m,a! inx h! mov m,a
	xra a! xchg! lxi b,16
acc$drv0:
	dad h! adc b! dcr c! jnz acc$drv0
	ora a! rz
	sta mult$cnt! dcr a! push a
	call acc$drv02
	pop a! jmp openx02 ; insufficient free lock list items
acc$drv02:
	call check$free! pop h ; Discard return addr, free space exists
	call count$opens! pop b! add b! jc openx034
	sub m! jnc openx034 ; openmax exceeded
	lhld info! lda curdsk! push a! mvi a,16
acc$drv1:
	dcr a! dad h! jc acc$drv2
acc$drv15:
	ora a! jnz acc$drv1
	pop a! sta curdsk! ret
acc$drv2:
	push a! push h! sta curdsk
	call create$olist$item
	pop h! pop a! jmp acc$drv15

func39:				
	; Free drive
	lhld open$root! mov a,h! ora l! rz
	xra a! sta incr$pdcnt! inr a! sta free$mode
	lhld info! mov a,h! cmp l! jnz free$drv1
	inr a! jnz free$drv1
	sta free$mode! call free$files! jmp free$drv3
free$drv1:
	lda curdsk! push a! mvi a,16
free$drv2:
	dcr a! dad h! jc free$drv4
free$drv25:
	ora a! jnz free$drv2
	pop a! sta curdsk
free$drv3:
	lda incr$pdcnt! ora a! rz
	lda pdcnt! jmp chk$olist1
free$drv4:
	push a! push h! sta curdsk
	call free$files
	pop h! pop a! jmp free$drv25
else

func38 	equ	func$ret
func39	equ	func$ret

endif

func40  equ	func34		; Write random with zero fill

if MPM

func41	equ	func$ret	; Test & write
func42:				; Record lock
	mvi a,0ffh! sta lock$unlock! jmp lock
func43:				; Record unlock
	xra a! sta lock$unlock! jmp lock

else

func42	equ	func$ret	; Record lock
func43	equ	func$ret	; Record unlock

endif

func44:				; Set multi-sector count
	mov a,e! ora a! jz lret$eq$ff
	cpi 129! jnc lret$eq$ff
	sta mult$cnt

if MPM
	mov d,a
	call rlr! lxi b,mult$cnt$off! dad b
	mov m,d
endif

	ret

func45:				; Set bdos error mode

if MPM
	call rlr! lxi b,pname+4! dad b
	call set$pflag
	mov m,a! inx h
	call set$pflag
	mov m,a! ret

set$pflag:
	mov a,m! ani 7fh! inr e! rnz
	ori 80h! ret
else
	mov a,e! sta error$mode
endif

	ret

func46:				
	; Get free space
	; Perform temporary select of specified drive
	call tmpselect
	lhld alloca! xchg ; de = alloc vector addr
	call get$nalbs ; Get # alloc blocks
	; hl = # of allocation vector bytes
	; Count # of true bits in allocation vector
	lxi b,0 ; bc = true bit accumulator
gsp1:	ldax d
gsp2:	ora a! jz gsp4
gsp3:	rar! jnc gsp3
	inx b! jmp gsp2
gsp4:	inx d! dcx h
	mov a,l! ora h! jnz gsp1
	; hl = 0 when allocation vector processed
	; Compute maxall + 1 - bc
	lhld maxall! inx h
	mov a,l! sub c! mov l,a
	mov a,h! sbb b! mov h,a
	; hl = # of available blocks on drive
	lda blkshf! mov c,a! xra a
	call shl3bv
	; ahl = # of available sectors on drive
	; Store ahl in beginning of current dma
	xchg! lhld xdmaad! mov m,e! inx h
	mov m,d! inx h! mov m,a! ret

if MPM

func47	equ	func$ret

else

func47: 			; Chain to program
	lxi h,ccp$flgs! mov a,m! ori 80h! mov m,a
	inr e! jnz rebootx1
	mov a,m! ori 40h! mov m,a
	jmp rebootx1
endif

func48:				; Flush buffers
	call check$all$media
	call flushf
	call diocomp
flush0:				; Function 98 entry point
	lhld dlog! mvi a,16
flush1:
	dcr a! dad h! jnc flush5
	push a! push h! mov e,a! call tmpselect ; seldsk = e
	lda fx! cpi 48! jz flush3
	; Function 98 - reset allocation
	; Copy 2nd ALV over 1st ALV
	call copy$alv! jmp flush35
flush3:
	call flushx
	; if e = 0ffh then discard buffers after possible flush
	lda linfo! inr a! jnz flush4
flush35:
	call discard$data
flush4:
	pop h! pop a
flush5:
	ora a! jnz flush1
	ret

flush:
	call flushf
	call diocomp
flushx:
	lda phymsk! ora a! rz
	mvi a,4! jmp deblock$dta

if MPM

func49	equ	func$ret

else

func49:	; Get/Set system control block

	xchg! mov a,m! cpi 99! rnc
	xchg! lxi h,scb! add l! mov l,a
	xchg! inx h! mov a,m! cpi 0feh! jnc func49$set
	xchg! mov e,m! inx h! mov d,m! xchg
	jmp sthl$ret
func49$set:
	mov b,a! inx h! mov a,m! stax d! inr b! rz
	inx h! inx d! mov a,m! stax d! ret
endif

if MPM

func50	equ	func$ret

else

func50:				; Direct bios call
	; de -> function (1 byte)
	;       a  value (1 byte)
	;       bc value (2 bytes)
	;       de value (2 bytes)
	;       hl value (2 bytes)

	lxi h,func50$ret! push h
	xchg

if BANKED
	mov a,m! cpi 27! rz
	cpi 12! jnz dir$bios1
	lxi d,dir$bios3! push d
dir$bios1:
	cpi 9! jnz dir$bios2
	lxi d,dirbios4! push d
dir$bios2:

endif

	push h! inx h! inx h
	mov c,m! inx h! mov b,m! inx h
	mov e,m! inx h! mov d,m! inx h
	mov a,m! inx h! mov h,m! mov l,a
	xthl! mov a,m! push h! mov l,a! add a! add l

	lxi h,bios

	add l! mov l,a! xthl
	inx h! mov a,m! pop h! xthl! ret

if BANKED

dir$bios3:
	mvi a,1! jmp setbnkf

dir$bios4:
	mov a,l! ora h! rz
	xchg! lxi h,10! dad d! mvi m,0 ; Zero login sequence #
	lhld common$base! call subdh! xchg! rnc
	; Copy DPH to common memory
	xchg! lhld info! inx h! push h! lxi b,25
	call movef! pop h! ret
endif

func50$ret:

if BANKED
	shld aret! mov b,a
	lhld info! mov a,m
	cpi 9! rz
	cpi 16! rz
	cpi 20! rz
	cpi 22! rz
	mov a,b! jmp sta$ret
else
	xchg! lhld entsp! sphl! xchg! ret
endif
endif

func98 	equ	flush0			; Reset Allocation

func99:					; Truncate file
	call reselectx
	call check$wild

if BANKED
	call chk$password! cnz chk$pw$error
endif

	mvi c,true! call rseek! jnz lret$eq$ff
	; compute dir$fcb size
	call getdptra! lxi d,reccnt
	call compute$rr ; cba = fcb size
	; Is random rec # >= dir$fcb size
	call get$rra! call compare$rr
	jc lret$eq$ff ; yes ( > )
	ora d! jz lret$eq$ff  ; yes ( = )
	; Perform truncate
	call check$rodir ; may be r/o file
	call wrdir ; verify BIOS can write to disk
	call update$stamp ; Set update stamp
	call search$extnum
trunc1:
	jz copy$dirloc
	; is dirfcb < fcb?
	call compare$mod$ext! jc trunc2 ; yes
	; remove dirfcb blocks from allocation vector
	push a! mvi c,0! call scandm$ab! pop a
	; is dirfcb = fcb?
	jz trunc3 ; yes
	; delete dirfcb
	call getdptra! mvi m,empty! call fix$hash
trunc15:
	call wrdir
trunc2:
	call searchn
	jmp trunc1
trunc3:
	call getfcb! call dm$position
	call zero$dm
	; fcb(extnum) = dir$ext after blocks removed
	call get$dir$ext! cmp m! mov m,a! push a
	; fcb(rc) = fcb(cr) + 1
	call getfcba! mov a,m! inr a! stax d
	; rc = 0 or 128 if dir$ext < fcb(extnum)
	pop a! xchg! cnz set$rc3
	; rc = 0 if no blocks remain in fcb
	lda dminx! ora a! cz set$rc3
	lxi b,11! call get$fcb$adds! xchg
	; reset archive (t3') attribute bit
	mov a,m! ani 7fh! mov m,a! inx h! inx d
	; dirfcb(extnum) = fcb(extnum)
	ldax d! mov m,a
	; advance to .fcb(reccnt) & .dirfcb(reccnt)
	inx h! mvi m,0! inx h! inx h
	inx d! inx d! inx d
	; dirfcb_rc+dskmap = fcb_rc+dskmap
	mvi c,17! call move
	; restore non-erased blkidxs in allocation vector
	mvi c,1! call scandm$ab
	jmp trunc15

get$fcb$adds:
	call getdptra! dad b! xchg
	lhld info! dad b! ret

compare$mod$ext:
	lxi b,modnum! call get$fcb$adds
	mov a,m! ani 3fh! mov b,a
	; compare dirfcb(modnum) to fcb(modnum)
	ldax d! cmp b! rnz ; dirfcb(modnum) ~= fcb(modnum)
	dcx h! dcx h! dcx d! dcx d
	; compare dirfcb(extnum) to fcb(extnum)
	ldax d! mov c,m! call compext! rz ; dirfcb(extnum) = fcb(extnum)
	ldax d! cmp m! ret

zero$dm:
	inr a! lxi h,single! inr m! jz zero$dm1
	add a
zero$dm1:
	dcr m
	call getdma! mov c,a! mvi b,0! dad b
	mvi a,16
zero$dm2:
	cmp c! rz
	mov m,b! inx h! inr c! jmp zero$dm2

if BANKED

func100:			; Set directory label
	; de -> .fcb
	;       drive location
	;       name & type fields user's discretion
	;       extent field definition
	;       bit 1 (80h): enable passwords on drive
	;       bit 2 (40h): enable file access 	
	;       bit 3 (20h): enable file update stamping
	;       bit 4 (10h): enable file create stamping
	;       bit 8 (01h): assign new password to dir lbl
	call reselectx
	lhld info! mvi m,21h! mvi c,1
	call search! jnz sdl0
	call getexta! mov a,m! ani 0111$0000b! jnz lret$eq$ff
sdl0:
	; Does dir lbl exist on drive?
	lhld info! mvi m,20h! mvi c,1
	call set$xdcnt! call search! jnz sdl1
	; no - make one
	mvi a,0ffh! sta make$xfcb
	call make! rz ; no dir space
	call init$xfcb
	lxi b,24! call stamp5! call stamp1 
sdl1:
	; Update date & time stamp
	lxi b,28! call stamp5! call stamp2
	; Verify password - new dir lbl falls through
	call chk$xfcb$password! jnz pw$error
	lxi b,0! call init$xfcb0
	; Set dir lbl dta in extent field
	ldax d! ori 1h! mov m,a
	; Low bit of dir lbl data set to indicate dir lbl exists
	; Update drive's dir lbl vector element
	push h! lhld drvlbla! mov m,a! pop h
sdl2:
	; Assign new password to dir lbl or xfcb?
	ldax d! ani 1! jz sdl3
	; yes - new password field is in 2nd 8 bytes of dma
	lxi d,8! call adjust$dmaad
	call set$pw! mov m,b
	lxi d,-8! call adjust$dmaad
sdl3:
	call fix$hash
	jmp seek$copy
else

func100	equ	lret$eq$ff
func103 equ	lret$eq$ff

endif

func101:			
	; Return directory label data
	; Perform temporary select of specified drive
	call tmpselect
	call get$dir$mode! jmp sta$ret

func102:			
	; Read file xfcb
	call reselectx
	call check$wild
	call zero$ext$mod
	call search$namlen! rz
	call getdma! lxi b,8! call zero
	push h! mvi c,0! call get$dtba! ora a! jnz rxfcb2
	pop d! xchg! mvi c,8

if BANKED
	call move! ldax d! jmp rxfcb3
else
	jmp move
endif

rxfcb2:
	pop h! lxi b,8

if BANKED
	call zero! call get$xfcb! rz
	mov a,m
rxfcb3:
	call getexta! mov m,a! ret
else
	jmp zero
endif

if BANKED

func103:			
	; Write or update file xfcb
	call reselectx
	; Are passwords enabled in directory label?
	call get$dir$mode! ral! jnc lret$eq$ff ; no
	call check$wild
	; Save .fcb(ext) & ext
	call getexta! mov b,m! push h! push b
	; Set extent & mod to zero
	call zero$ext$mod
	; Does file's 1st fcb exist in directory?
	call search$namlen
	; Restore extent
	pop b! pop h! mov m,b! rz ; no
	call set$xdcnt
	; Does sfcb exist?
	call get$dtba$8! ora a! jz wxfcb5 ; yes
	; No - Does xfcb exist?
	call get$xfcb! jnz wxfcb1 ; yes
wxfcb0:
	; no - does file exist in directory?
	mvi a,0ffh! sta make$xfcb
	call search$extnum! rz
	; yes - attempt to make xfcb for file
	call make! rz ; no dir space
	; Initialize xfcb
	call init$xfcb
wxfcb1:
	; Verify password - new xfcb falls through
	call chk$xfcb$password! jnz pw$error
	; Set xfcb options data
	push h! call getexta! pop d! xchg
	mov a,m! ora a! jnz wxfcb2
	ldax d! ani 1! jnz wxfcb2
	call sdl3! jmp wxfcb4
wxfcb2:
	ldax d! ani 0e0h! jnz wxfcb3
	mvi a,80h
wxfcb3:
	mov m,a! call sdl2
wxfcb4:
	call get$xfcb1! dcr a! sta pw$mode
	call zero$ext$mod
	call search$namlen! rz
	call get$dtba$8! ora a! rnz
	lda pw$mode! mov m,a! jmp seek$copy
wxfcb5:
	; Take sfcb's password mode over xfcb's mode
	mov a,m! push a
	call get$xfcb
	; does xfcb exist?
	pop b! jz wxfcb0 ; no
	; Set xfcb's password mode to sfcb's mode
	mov m,b! jmp wxfcb1

endif

func104:			; Set current date and time

if MPM
	call get$stamp$add
else
	lxi h,stamp
endif
	call copy$stamp
	mvi m,0! mvi c,0ffh! jmp timef

func105:			; Get current date and time



if MPM
	call get$stamp$add
else
	mvi c,0! call timef
	lxi h,stamp
endif

	xchg
	call copy$stamp
	ldax d! jmp sta$ret

copy$stamp:
	mvi c,4! jmp move ; ret

if MPM

get$stamp$add:
	call rlradr! lxi b,-5! dad b
	ret
endif

if BANKED

func106:			; Set default password

if MPM
	call get$df$pwa! inr a! rz
	lxi b,7! dad b
else
	lxi h,df$password+7
endif
	xchg! lxi b,8! push h
	jmp set$pw0
else

func106	equ	func$ret

endif

func107:			; Return serial number

if MPM
	lhld sysdat! mvi l,181
else
	lxi h,serial
endif

	xchg! mvi c,6! jmp move

func108:			; Get/Set program return code

	; Is de = 0ffffh?
	mov a,d! ana e! inr a
	lhld clp$errcde! jz sthl$ret ; yes - return return code
	xchg! shld clp$errcde! ret ; no - set return code

goback0:
	lxi h,0ffffh! shld aret
goback:
	; Arrive here at end of processing to return to user
	lda resel! ora a! jz retmon

if MPM
		lda comp$fcb$cks! ora a! cnz set$chksum$fcb
endif

		lhld info! lda fcbdsk! mov m,a ; fcb(0)=fcbdsk
if BANKED

		; fcb(7) = fcb(7) | xfcb$read$only
		lxi d,7! dad d! lda xfcb$read$only! ora m! mov m,a

endif
if MPM
		; if high$ext = 60h then fcb(8) = fcb(8) | 80h
		;                   else fcb(ext) = fcb(ext) | high$ext

		call getexta! lda high$ext! cpi 60h! jnz goback2
		lxi d,-4! dad d! mvi a,80h
	goback2:
		ora m! mov m,a
else
		; fcb(8) = fcb(8) | high$ext
if BANKED
		inx h
else
		lxi d,8! dad d
endif
		lda high$ext! ora m! mov m,a
endif

;	return from the disk monitor

retmon:
	lhld entsp! sphl
	lhld aret! mov a,l! mov b,h! ret
;
;	data areas
;
efcb:	db	empty	; 0e5=available dir entry
rodsk:	dw	0	; read only disk vector
dlog:	dw	0	; logged-in disks

if MPM

rlog:	dw	0	; removeable logged-in disks
tlog:	dw	0	; removeable disk test login vector
ntlog:	dw	0	; new tlog vector
rem$drv: ds	byte	; curdsk removable drive switch
			; 0 = permanent drive, 1 = removable drive
endif

if not BANKED

xdmaad	equ	$
curdma	ds	word	; current dma address

endif

if not MPM

buffa:	ds	word	; pointer to directory dma address

endif

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
	ds	word	; directory hash table address
	ds	byte	; directory hash table bank

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
;	       bank(1) || link(2)
;	       12         13
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
drec      	ds	word	; directory record number
blk$off:	ds	byte	; record offset within block
last$off:	ds	byte	; last offset within new block
last$drive:	ds	byte	; drive of last new block
last$block:	ds	word	; last new block

; The following two variables are initialized as a pair on entry

dir$cnt:	ds	byte	; direct i/o count
mult$num:	ds	byte	; multi-sector number

tranv:	ds	word	; address of translate vector
lock$unlock:
make$flag:
rmf:	ds	byte	; read mode flag for open$reel
incr$pdcnt:
dirloc:	ds	byte	; directory flag in rename, etc.
free$mode:
linfo:	ds	byte	; low(info)
dminx:	ds	byte	; local for diskwrite

if MPM

searchl:ds	byte	; search length

endif
if BANKED

searcha:ds	word	; search address

endif

if BANKED

save$xfcb:
	ds	byte	; search xfcb save flag

endif

single:	ds	byte	; set true if single byte allocation map

if MPM

seldsk: ds	byte	; currently selected disk 

endif

seldsk:	ds	byte	; disk on entry to bdos
rcount:	ds	byte	; record count in current fcb
extval:	ds	byte	; extent number and extmsk
save$mod:
	ds	byte	; open$reel module save field

vrecord:ds	byte	; current virtual record

if not MPM

curdsk: db	0ffh	; current disk

endif

adrive: db	0ffh	; current blocking/deblocking disk
arecord:ds	word	; current actual record
	ds	byte

save$ranr:	ds	3	; random record save area
arecord1:	ds	word	; current actual block# * blkmsk
attributes:	ds	byte	; make attribute hold area
readf$sw:	ds	byte	; BIOS read/write switch

;******** following variable order critical *****************

if MPM

mult$cnt:	ds	byte	; multi-sector count
pdcnt:		ds	byte	; process descriptor count

endif

high$ext:	ds	byte	; fcb high ext bits

if BANKED

xfcb$read$only:	ds	byte	; xfcb read only flag

endif
if MPM

curdsk:		db	0ffh			;current disk
packed$dcnt:	ds	3    			;
pdaddr:		ds	word			;
;************************************************************
cur$pos:	ds	word			;
prv$pos:	ds	word			;
sdcnt:		ds	word   	 		;
sdblk:		ds	word   			;
sdcnt0:		ds	word			;
sdblk0:		ds	word			;
dont$close:	ds	byte			;
open$cnt:			; mp/m temp variable for open
lock$cnt:	ds	word	; mp/m temp variable for lock
file$id:	ds	word	; mp/m temp variable for lock
deleted$files:	ds	byte
lock$shell:	ds	byte
lock$sp:	ds	word
set$ro$flag:	ds	byte
check$disk:	ds	byte
flushed:	ds	byte
fcb$cks$valid:  ds	byte
;				mp/m variables  *

endif

;	local variables for directory access
dptr:	ds	byte	; directory pointer 0,1,2,3

save$hash:	ds	4	; hash code save area

if BANKED

copy$cr$init:	ds	byte	; copy$cr$only initialization value

else

hashmx:	ds	word	; cdrmax or dirmax
xdcnt:	ds	word	; empty directory dcnt

endif

if MPM

xdcnt:	ds	word	; empty directory dcnt
xdblk:  ds	word	; empty directory block
dcnt:	ds	word	; directory counter 0,1,...,dirmax
dblk:	ds	word	; directory block index

endif

search$user0:	ds	byte	; search user 0 for file (open)

user0$pass:	ds	byte	; search user 0 pass flag

fcbdsk:		ds	byte	; disk named in fcb

if MPM

make$xfcb:	ds	1
find$xfcb:	ds	1

endif

log$fxs:db	15,16,17,19,22,23,30,35,99,100,102,103,0
rw$fxs: db	20,21,33,34,40,41,0
sc$fxs: db	16,18,0

if MPM

comp$fcb$cks:	ds	byte	; compute fcb checksum flag

endif
if BANKED

pw$fcb:		ds	12	;1 |
		db	0	;2 |
pw$mode:	db	0	;3 |- Order critical
		db	0	;4 |
		db	0	;5 |

df$password:	ds	8

if MPM
		ds	120
endif
endif

phy$off:	ds	byte
curbcba:	ds	word

if BANKED

lastbcba:	ds	word
rootbcba:	ds	word
emptybcba:	ds	word
seqbcba:	ds	word
buffer$bank:	ds	byte

endif

track:		ds	word
sector:		ds	word

; 	**************************
; 	Blocking/Deblocking Module
;	**************************

deblock$dta:
	lhld dtabcba

if BANKED
	cpi 4! jnz deblock
deblock$flush:
	; de = addr of 1st bcb
	mov e,m! inx h! mov d,m
	; Search for dirty bcb with lowest track #
	lxi h,0ffffh! shld track! xchg
deblock$flush1:
	; Does current drive own bcb?
	lda adrive! cmp m! jnz deblock$flush2 ;no
	; Is bcb's buffer pending?
	xchg! lxi h,4! dad d! mov a,m
	xchg! inr a! jnz deblock$flush2 ; no
	; Is bcb(6) < track?
	push h! inx d! inx d! xchg
	mov e,m! inx h! mov d,m
	; Subdh computes hl = de - hl
	lhld track! call subdh! pop h! jnc deblock$flush2 ; no
	; yes - track = bcb(6) , sector = addr(bcb)
	xchg! shld track! xchg! shld sector
deblock$flush2:
	; Is this the last bcb?
	call get$next$bcba! jnz deblock$flush1 ; no - hl = addr of next bcb
	; Does track = ffff?
	lxi h,track! call test$ffff! rz ; yes - no bcb to flush
	; Flush bcb located by sector
	lhld sector! xra a! mvi a,4! call deblock
	lhld dtabcba! jmp deblock$flush ; Repeat until no bcb's to flush
endif

deblock:

	; BDOS Blocking/Deblocking routine
	; a = 1 -> read command
	; a = 2 -> write command
	; a = 3 -> locate command
	; a = 4 -> flush command
	; a = 5 -> directory update

	push a ; Save z flag and deblock fx

	; phy$off = low(arecord) & phymsk
	; low(arecord) = low(arecord) & ~phymsk
	call deblock8
	lda arecord! mov e,a! ana b! sta phy$off
	mov a,e! ana c! sta arecord

if BANKED
	pop a! push a! cnz get$bcba
endif

	shld curbcba! call getbuffa! shld curdma
	; hl = curbcba, de = .adrive, c = 4
	call deblock9
	; Is BCB discarded?
	mov a,m! inr a! jz deblock2 ; yes
	; Is command flush?
	pop a! push a! cpi 4! jnc deblock1 ; yes
	; Is referenced physical record already in buffer?
	call compare! jz deblock45 ; yes
	xra a
deblock1:
	; Does buffer contain an updated record?
	call deblock10
	cpi 5! jz deblock15
	mov a,m! ora a! jz deblock2 ; no
deblock15:
	; Reset record pending flag
	mvi m,0
	; Save arecord
	lhld arecord! push h! lda arecord+2! push a
	; Flush physical record buffer
	call deblock9
	xchg! call move
	; Select drive to be flushed
	lxi h,curdsk! lda adrive! cmp m! cnz disk$select1
	; Write record if drive logged-in
	mvi a,1! cz deblock$io
	; Restore arecord
	pop b! pop d! call set$arecord
	; Restore selected drive
	call curselect
deblock2:
	; Is deblock command flush | dir write?
	pop a! cpi 4! rnc ; yes - return
	; Is deblock command write?
	push a! cpi 2! jnz deblock25 ; no
	; Is blk$off < last$off
	lxi h,last$off! lda blk$off! cmp m! jnc deblock3 ; no
deblock25:
	; Discard BCB on read operations in case 
	; I/O error occurs
	lhld curbcba! mvi m,0ffh
	; Read physical record buffer
	mvi a,2! jmp deblock35
deblock3:
	; last$off = blk$off + 1
	inr a! mov m,a
	; Place track & sector in bcb
	xra a
deblock35:
	call deblock$io
deblock4:
	call deblock9 ; phypfx = adrive || arecord
	call move! mvi m,0 ; zero pending flag

if BANKED
	; Zero logical record sequence
	inx h! call set$bcb$seq
endif

deblock45:
	; recadd = phybuffa + phy$off*80h
	lda phy$off! inr a! lxi d,80h! lxi h,0ff80h
deblock5:
	dad d! dcr a! jnz deblock5
	xchg! lhld curdma! dad d
	; If deblock command = locate then buffa = recadd; return
	pop a! cpi 3! jnz deblock6
	shld buffa! ret
deblock6:
	xchg! lhld dmaad! lxi b,80h
	; If deblock command = read
	cpi 1

if BANKED
	jnz deblock7
	; then move to tpa
	lda common$base+1! dcr a! cmp d! jc move$tpa
	lda buffer$bank! mov c,a! mvi b,1! call deblock12
	lxi b,80h! jmp move$tpa
deblock7:

else
	jz move$tpa ; then move to dma
endif

	; else move from dma
	xchg

if BANKED
	lda common$base+1! dcr a! cmp h! jc deblock75
	lda buffer$bank! mov b,a! mvi c,1! call deblock12
	lxi b,80h
deblock75:

endif

	call move$tpa
	; Set physical record pending flag for write command
	call deblock10! mvi m,0ffh
	ret

deblock8:
	lda phymsk! mov b,a! cma! mov c,a! ret

deblock9:
	lhld curbcba! lxi d,adrive! mvi c,4! ret

deblock10:
	lxi d,4
deblock11:
	lhld curbcba! dad d! ret

if BANKED

deblock12:
	push h! push d! call xmovef
	pop d! pop h! ret
endif

deblock$io:
	; a = 0 -> seek only
	; a = 1 -> write
	; a = 2 -> read
	push a! call seek

if BANKED
	lda buffer$bank! call setbnkf
endif

	mvi c,1
	pop a! dcr a
	jz wrbuff
	cp rdbuff
	; Move track & sector to bcb
	call deblock10! inx h! inx h
	lxi d,track! mvi c,4! jmp move

if BANKED

get$bcba:
	shld rootbcba
	lxi d,-13! dad d! shld lastbcba
	call get$next$bcba! push h
	; Is there only 1 bcb in list?
	call get$next$bcba! pop h! rz ; yes - return
	xchg! lxi h,0! shld emptybcba! shld seqbcba
	xchg
get$bcb1:
	; Does bcb contain requested record?
	shld curbcba! call deblock9! call compare! jz get$bcb4 ; yes
	; Is bcb discarded?
	lhld curbcba! mov a,m! inr a! jnz get$bcb11 ; no
	xchg! lhld lastbcba! shld emptybcba! jmp get$bcb14
get$bcb11:
	; Does bcb contain record from current disk?
	lda adrive! cmp m! jnz get$bcb15 ; no
	xchg! lxi h,5! dad d! lda phy$msk
	; Is phy$msk = 0?
	ora a! jz get$bcb14 ; yes
	; Does bcb(5) [bcb sequence] = phymsk?
	cmp m! jnz get$bcb14 ; no
	lhld seqbcba! mov a,l! ora h! jnz get$bcb14
	lhld lastbcba! shld seqbcba
get$bcb14:
	xchg
get$bcb15:
	; Advance to next bcb - list exhausted?
	push h! call get$next$bcba! pop d! jz get$bcb2 ; yes
	xchg! shld lastbcba! xchg! jmp get$bcb1
get$bcb2:
	; Matching bcb not found
	; Was a sequentially accessed bcb encountered?
	lhld seqbcba! mov a,l! ora h! jnz get$bcb25 ; yes
	; Was a discarded bcb encountered?
	lhld emptybcba! mov a,l! ora h! jz get$bcb3 ; no
get$bcb25:
	shld lastbcba
get$bcb3:
	; Insert selected bcb at head of list
	lhld lastbcba! call get$next$bcba
	shld curbcba! call get$next$bcba
	xchg! call last$bcb$links$de
	lhld rootbcba! mov e,m! inx h! mov d,m
	lhld curbcba! lxi b,13! dad b
	mov m,e! inx h! mov m,d
	lhld curbcba! xchg! lhld rootbcba
	mov m,e! inx h! mov m,d! xchg! ret
get$bcb4:
	; BCB matched arecord
	lhld curbcba! lxi d,5! dad d
	; Does bcb(5) = phy$off?
	lda phy$off! cmp m! jz get$bcb5 ; yes
	; Does bcb(5) + 1 = phy$off?
	inr m! cmp m! jz get$bcb5 ; yes
	call set$bcb$seq
get$bcb5:
	; Is bcb at head of list?
	lhld curbcba! xchg! lhld rootbcba
	mov a,m! inx h! mov l,m! mov h,a
	call subdh! ora l! xchg! rz ; yes
	jmp get$bcb3 ; no - insert bcb at head of list

last$bcb$links$de:
	lhld lastbcba! lxi b,13! dad b
	mov m,e! inx h! mov m,d! ret

get$next$bcba:
	lxi b,13! dad b! mov e,m! inx h! mov d,m
	xchg! mov a,h! ora l! ret

set$bcb$seq:
	lda phy$off! mov m,a! ora a! rz
	lda phy$msk! inr a! mov m,a! ret

endif

if not MPM
if not BANKED

		ds	112
last:
		org	base + (((last-base)+255) and 0ff00h) - 112

olog:		dw	0
rlog:		dw	0

patch$flgs:	dw	0,0	
          	dw	base+6
		xra a! ret

; System Control Block

SCB:

; Expansion Area - 6 bytes

hashl:		db	0
hash:		dw	0,0
version:	db	31h

; Utilities Section - 8 bytes

util$flgs:	dw	0,0
dspl$flgs:	dw	0
		dw	0

; CLP Section - 4 bytes

clp$flgs:	dw	0
clp$errcde:	dw	0

; CCP Section - 8 bytes

ccp$comlen:	db	0
ccp$curdrv:	db	0
ccp$curusr:	db	0
ccp$conbuff:	dw	0
ccp$flgs:	dw	0
		db	0

; Device I/O Section - 32 bytes

conwidth:	db	0
column:		db	0
conpage:	db	0
conline:	db	0
conbuffadd:	dw	0
conbufflen:	dw	0
conin$rflg:	dw	0
conout$rflg:	dw	0
auxin$rflg:	dw	0
auxout$rflg:	dw	0
lstout$rflg:	dw	0
page$mode:	db	0
pm$default:	db	0
ctlh$act:	db	0
rubout$act:	db	0
type$ahead:	db	0
contran:	dw	0
conmode:	dw	0
      		db	0
		db	0
outdelim:	db	'$'
listcp		db	0
qflag:		db	0

; BDOS Section - 42 bytes

scbadd:		dw	scb
dmaad:		dw	0080h
olddsk:		db	0
info:		dw	0
resel:		db	0
relog:		db	0
fx:		db	0
usrcode:	db	0
dcnt:		dw	0
searcha:	dw	0
searchl:	db	0
multcnt:	db	1
errormode:	db	0
searchchain:	db	0,0ffh,0ffh,0ffh
temp$drive:	db	0
errdrv:		db	0
		dw	0
media$flag:	db	0
      		dw	0
bdos$flags:	db	0
stamp:		db	0ffh,0ffh,0ffh,0ffh,0ffh
commonbase:	dw	0
error: 		jmp	error$sub
bdosadd:	dw	base+6

endif
endif

;	************************
;	Directory Hashing Module
;	************************

; Hash format
; xxsuuuuu xxxxxxxx xxxxxxxx ssssssss
; x = hash code of fcb name field
; u = low 5 bits of fcb user field
;     1st bit is on for XFCB's
; s = shiftr(mod || ext,extshf)

if not BANKED

hashorg:
	org	base+(((hashorg-base)+255) and 0ff00h) 
endif

init$hash:
	; de = .hash table entry
	; hl = .dir fcb
	push h! push d! call get$hash
	; Move computed hash to hash table entry
	pop h! lxi d,hash! lxi b,4

if BANKED
	lda hash$tbla+2! call move$out
else
	call movef
endif

	; Save next hash table entry address
	shld arecord1
	; Restore dir fcb address
	pop h! ret

set$hash:
	; Return if searchl = 0
	ora a! rz
	; Is searchl < 12 ?
	cpi 12! jc set$hash2 ; yes - hashl = 0
	; Is searchl = 12 ?
	mvi a,2! jz set$hash1 ; yes - hashl = 2
	mvi a,3 ; hashl = 3
set$hash1:
	sta hashl
	xchg
	; Is dir hashing invoked for drive?
	call test$hash! rz ; no
	xchg
	lda fx
	cpi 16! jz get$hash ; bdos fx = 16
	cpi 35! jz set$hash15
	cpi 20! jnc get$hash ; bdos fx = 20 or above
set$hash15:
	mvi a,2! sta hashl ; bdos fx = 15,17,18,19, or 35
	; if fcb wild then hashl = 0, hash = fcb(0)
	;             else hashl = 2, hash = get$hash
	push h! call chk$wild! pop h! jnz get$hash
set$hash2:
	xra a! sta hashl
	; jmp get$hash

get$hash:
	; hash(0) = fcb(0)
	mov a,m! sta hash! inx h! xchg
	; Don't compute hash for dir lbl & sfcb's
	lxi h,0! ani 20h! jnz get$hash6
	; b = 11, c = 8, ahl = 0 
	; Compute fcb name hash (000000xx xxxxxxxxx xxxxxxxx) (ahl)
	lxi b,0b08h
get$hash1:
	; Don't shift if fcb(8)
	dcr c! push b! jz get$hash3
	; Don't shift if fcb(6)
	dcr c! dcr c! jz get$hash3
	; ahl = ahl * 2
	dad h! adc a! push a! mov a,b
	; is b odd?
	rar! jc get$hash4 ; yes
	; ahl = ahl * 2 for even fcb(i)
	pop a! dad h! adc a
get$hash3:
	push a
get$hash4:
	; a = fcb(i) & 7fh - 20h divided by 2 if even
	ldax d! ani 7fh! sui 20h! rar! jnc get$hash5
	ral
get$hash5:
	; ahl = ahl + a
	mov c,a! mvi b,0
	pop a! dad b! aci 0! pop b
	; advance to next fcb char
	inx d! dcr b! jnz get$hash1
get$hash6:
	; ahl = 000000xx xxxxxxxx xxxxxxxx
	; Store low 2 bytes of hash
	shld hash+1! lxi h,hash
	; hash(0) = hash(0) (000uuuuu) | xx000000
	ani 3! rrc! rrc! ora m! mov m,a
	; Does fcb(0) = e5h, 20h, or 21h?
	ani 20h! jnz get$hash9 ; yes
	; bc = 00000mmm mmmeeeee, m = module #, e = extent
	ldax d! ani 1fh! mov c,a! inx d! inx d
	ldax d! ani 3fh! rrc! rrc! rrc! mov d,a
	ani 7! mov b,a! mov a,d! ani 0e0h! ora c! mov c,a
	; shift bc right by # of bits in extmsk
	lda extmsk
get$hash7:
	rar! jnc get$hash8
	push a
	mov a,b! rar! mov b,a
	mov a,c! rar! mov c,a
	pop a! jmp get$hash7
get$hash8:
	; hash(0) = hash(0) (xx0uuuuu) | 00s00000
	mov a,b! ani 1! rrc! rrc
get$hash9:
	rrc! ora m! mov m,a
	; hash(3) = ssssssss
	lxi d,3! dad d! mov m,c! ret

test$hash:
	lhld hash$tbla! mov a,l! ora h! inr a! ret

search$hash:
	; Does hash table exist for drive?
	call test$hash! rz ; no
	; Has dir hash search been disabled?
	lda hashl! inr a! rz ; yes
	; Is searchl = 0?
	lda searchl! ora a! rz ; yes
	; hashmx = cdrmaxa if searchl ~= 1
	;          dir$max if searchl = 1
	lhld cdrmaxa! mov e,m! inx h! mov d,m
	xchg! dcr a! jnz search$h0
	lhld dir$max
search$h0:
	shld hashmx

if BANKED
	; call search$hash in resbdos, a = bank, hl = hash tbl addr
	lda hash$tbla+2! lhld hash$tbla! call srch$hash
	; Was search successful?
	jnz search$h1 ; no
	; Is directory read required?
	lda rd$dir$flag! ora a! mvi c,0
	cnz r$dir2 ; yes if Z flag reset
	; Is function = 18?
	lda fx! sui 18! rz ; Never reset dcnt for fx 18
	; Was media change detected by above read?
	lda hashl! inr a! cz setenddir ; yes
	xra a! ret ; search$hash successful
search$h1:
	; Was search initiated from beginning of directory?
	call end$of$dir! rnz ; no
	; Is bdos fx = 15,17,19,22,23,30?
	call tst$log$fxs! rnz ; no
	; Disable hash & return successful
	mvi a,0ffh! sta hashl
	lhld cdrmaxa! mov e,m! inx h! mov d,m! xchg
	dcx h! call set$dcnt$dblk1! xra a! ret
else
	lhld hash$tbla! mov b,h! mov c,l
	lhld hashmx! xchg
	; Return with Z flag set if dcnt = hashmx
	lhld dcnt! push h! call subdh! pop d! ora l! rz
	; Push hashmx - dcnt (# of hashtbl entries to search)
	; Push dcnt + 1
	push h! inx d! xchg! push h
	; Compute .hash$tbl(dcnt)
	dcx h! dad h! dad h! dad b
search$h1:
	; Advance hl to address of next hash$tbl entry
	lxi d,4! dad d! lxi d,hash
	; Do hash u fields match?
	ldax d! xra m! ani 1fh! jnz search$h3 ; no
	; Do hash's match?
	call search$h6! jz search$h4 ; yes
search$h2:
	xchg! pop h
search$h25:
	; de = .hash$tbl(dcnt), hl = dcnt
	; dcnt = dcnt + 1
	inx h! xthl
	; hl = # of hash$tbl entries to search
	; decrement & test for zero
	; Restore stack & hl to .hashtbl(dcnt)
	dcx h! mov a,l! ora h! xthl! push h
	; Are we done?
	xchg! jnz search$h1 ; no - keep searching
	; Search unsuccessful
	pop h! pop h
	; Was search initiated from beginning of directory?
	call end$of$dir! rnz ; no
	; Is fx = 15,17,19,22,23,30 & drive removeable?
	call tst$log$fxs! rnz ; no
	; Disable hash & return successful
	mvi a,0ffh! sta hashl
	lhld cdrmaxa! mov e,m! inx h! mov d,m! xchg
	dcx h! call set$dcnt$dblk1! xra a! ret

search$h3:
	; Does xdcnt+1 = 0ffh?
	lda xdcnt+1! inr a! jz search$h5 ; yes
	; Does xdcnt+1 = 0feh?
	inr a! jnz search$h2 ; no - continue searching
	; Do hash's match?
	call search$h6! jnz search$h2 ; no
	; xdcnt+1 = 0feh
	; Open user 0 search
	; Does hash u field = 0?
	mov a,m! ani 1fh! jnz search$h2 ; no
	; Search successful
search$h4:
	; Successful search
	; Set dcnt to search$hash dcnt-1
	; dcnt gets incremented by read$dir
	; Also discard search$hash loop count
	lhld dcnt! xchg
	pop h! dcx h! shld dcnt! pop b
	; Does dcnt&3 = 3?
	mov a,l! ani 03h! cpi 03h! rz ; yes
	; Does old dcnt & new dcnt reside in same sector?
	mov a,e! ani 0fch! mov e,a
	mov a,l! ani 0fch! mov l,a
	call subdh! ora l! rz ; yes
	; Read directory record
	call read$dir2
	; Has media change been detected?
	lda hashl! inr a! cz setenddir ; dcnt = -1 if hashl = 0ffh
	xra a! ret
search$h5:
	; xdcnt+1 = 0ffh
	; Make search to save dcnt of empty fcb
	; Is hash$tbl entry empty?
	mov a,m! cpi 0f5h! jnz search$h2 ; no
search$h55:
	; xdcnt = dcnt
	xchg! pop h! shld xdcnt! jmp search$h25
search$h6:
	; hash compare routine
	; Is hashl = 0?
	lda hashl! ora a! rz ; yes - hash compare successful
	; b = 0f0h if hashl = 3
	;     0d0h if hashl = 2
	mov c,a! rrc! rrc! rrc! ori 1001$0000b! mov b,a
	; hash s field must be screened out of hash(0)
	; if hashl = 2
	; Do hash(0) fields match?
	ldax d! xra m! ana b! rnz ; no
	; Compare remainder of hash fields for hashl bytes
	push h! inx h! inx d! call compare
	pop h! ret
endif

fix$hash:
	call test$hash! rz
	lxi h,save$hash! lxi d,hash! lxi b,4
	push h! push d! push b! call movef
	lhld hash$tbla! push h
	call get$dptra! call get$hash
	lhld dcnt! dad h! dad h
	pop d! dad d
	pop b! pop d! push d! push b

if BANKED
	lda hash$tbla+2! call move$out
else
	call movef
endif

	pop b! pop h! pop d! jmp movef

if not MPM
if BANKED

	ds	1
last:
	org	(((last-base)+255) and 0ff00h) - 1
	db	0
endif

else
	ds	192
last:
	org	(((last-base)+255) and 0ff00h) - 192

	;	bnkbdos patch area

	dw	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	dw	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	dw	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	dw	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	dw	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	dw	0,0,0,0,0,0,0,0,0,0,0,0

free$root:	dw	$-$
open$root:	dw	0
lock$root:	dw	0
lock$max:	db	0
open$max:	db	0

;	BIOS access table

bios	equ	$		; base of the bios jump table
bootf	equ	bios		; cold boot function
wbootf	equ	bootf+3		; warm boot function
constf	equ	wbootf+3	; console status function
coninf	equ	constf+3	; console input function
conoutf	equ	coninf+3	; console output function
listf	equ	conoutf+3	; list output function
punchf	equ	listf+3		; punch output function
readerf	equ	punchf+3	; reader input function
homef	equ	readerf+3	; disk home function
seldskf	equ	homef+3		; select disk function
settrkf	equ	seldskf+3	; set track function
setsecf	equ	settrkf+3	; set sector function
setdmaf	equ	setsecf+3	; set dma function
readf	equ	setdmaf+3	; read disk function
writef	equ	readf+3		; write disk function
liststf	equ	writef+3	; list status function
sectran	equ	liststf+3	; sector translate

endif

		end
