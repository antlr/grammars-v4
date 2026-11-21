	title	'CP/M 3 Banked BDOS Resident Module, Dec 1982'
;***************************************************************
;***************************************************************
;**                                                           **
;**   B a s i c    D i s k   O p e r a t i n g   S y s t e m  **
;**                                                           **
;**   R e s i d e n t   M o d u l e  -  B a n k e d  B D O S  **
;**                                                           **
;***************************************************************
;***************************************************************

;/*
;  Copyright (C) 1978,1979,1980,1981,1982
;  Digital Research
;  P.O. Box 579
;  Pacific Grove, CA 93950
;
;  December, 1982
;
;*/
;
ssize		equ	30
diskfx		equ	12
conoutfxx	equ	2
printfx		equ	9
constatfx	equ	11
setdmafx	equ	26
chainfx		equ	47
ioloc		equ	3

		org	0000h
base		equ	$

bnkbdos$pg	equ	base+0fc00h
resbdos$pg	equ	base+0fd00h
scb$pg		equ	base+0fe00h
bios$pg		equ	base+0ff00h

bnkbdos		equ	bnkbdos$pg+6
error$jmp 	equ	bnkbdos$pg+7ch

bios		equ	bios$pg
bootf		equ	bios$pg 	; 00. cold boot function
wbootf		equ	bios$pg+3	; 01. warm boot function
constf		equ	bios$pg+6	; 02. console status function
coninf		equ	bios$pg+9	; 03. console input function
conoutf		equ	bios$pg+12	; 04. console output function
listf		equ	bios$pg+15	; 05. list output function
punchf		equ	bios$pg+18	; 06. punch output function
readerf		equ	bios$pg+21	; 07. reader input function
homef		equ	bios$pg+24	; 08. disk home function
seldskf		equ	bios$pg+27	; 09. select disk function
settrkf		equ	bios$pg+30	; 10. set track function
setsecf		equ	bios$pg+33	; 11. set sector function
setdmaf		equ	bios$pg+36	; 12. set dma function
readf		equ	bios$pg+39	; 13. read disk function
writef		equ	bios$pg+42	; 14. write disk function
liststf		equ	bios$pg+45	; 15. list status function
sectran		equ	bios$pg+48	; 16. sector translate
conoutstf	equ	bios$pg+51	; 17. console output status function
auxinstf	equ	bios$pg+54	; 18. aux input status function
auxoutstf	equ	bios$pg+57	; 19. aux output status function
devtblf		equ	bios$pg+60	; 20. return device table address fx
devinitf	equ	bios$pg+63	; 21. initialize device function
drvtblf		equ	bios$pg+66	; 22. return drive table address
multiof		equ	bios$pg+69	; 23. multiple i/o function
flushf		equ	bios$pg+72	; 24. flush function
movef		equ	bios$pg+75	; 25. memory move function
timef		equ	bios$pg+78	; 26. get/set system time function
selmemf		equ	bios$pg+81	; 27. select memory function
setbnkf		equ	bios$pg+84	; 28. set dma bank function
xmovef		equ	bios$pg+78	; 29. extended move function

sconoutf	equ	conoutf		; 31. escape sequence decoded conout
screenf		equ	0ffffh		; 32. screen function

serial:	db	'654321'

	jmp	bdos
	jmp	move$out	;A = bank #
				;HL = dest, DE = srce
	jmp	move$tpa	;A = bank #
				;HL = dest, DE = srce
	jmp 	search$hash	;A = bank #
				;HL = hash table address

	; on return, Z flag set for eligible DCNTs
	;	     Z flag reset implies unsuccessful search

	; Additional variables referenced directly by bnkbdos

hashmx:		dw	0	;max hash search dcnt
rd$dir:		db	0	;read directory flag
make$xfcb:	db	0	;Make XFCB flag
find$xfcb:	db	0	;Search XFCB flag
xdcnt:		dw	0	;current xdcnt

xdmaadd:	dw	common$dma
curdma:		dw	0
copy$cr$only:	db	0
user$info:	dw	0
kbchar:		db	0
		jmp	qconinx

bdos:	;arrive here from user programs
	mov a,c ; c = BDOS function #
	
	;switch to local stack

	lxi h,0! shld aret
	dad sp! shld entsp ; save stack pointer
	lxi sp,lstack! lxi h,goback! push h

	cpi diskfx! jnc disk$func
	
	lxi h,functab! mvi b,0
	dad b! dad b! mov a,m
	inx h! mov h,m! mov l,a! pchl

	db	'COPYRIGHT (C) 1982,'
	db	' DIGITAL RESEARCH '
	db	'151282'
	dw	0,0,0,0,0,0,0,0,0,0

functab:
	dw	wbootf, bank$bdos, bank$bdos, func3
	dw	func4, func5, func6, func7
	dw	func8, func9, func10, bank$bdos

func3:
	call readerf! jmp sta$ret

func4:
	mov c,e! jmp punchf

func5:
	mov c,e! jmp listf

func6:
	mov a,e! inr a! jz dirinp	;0ffh -> cond. input
	inr a! jz dirstat		;0feh -> status
	inr a! jz dirinp1		;0fdh -> input
	mov c,e! jmp conoutf		;	 output
dirstat:
	call constx! jmp sta$ret
dirinp:
	call constx! ora a! rz
dirinp1:
	call conin! jmp sta$ret

constx:
	lda kbchar! ora a! mvi a,0ffh! rnz
	jmp constf

conin:
	lxi h,kbchar! mov a,m! mvi m,0! ora a! rnz
	jmp coninf

func7:
	call auxinstf! jmp sta$ret

func8:
	call auxoutstf! jmp sta$ret

func9:
	mov b,d! mov c,e
print:
	lxi h,outdelim
	ldax b! cmp m! rz
	inx b! push b! mov c,a
	call blk$out0
	pop b! jmp print

func10:
	xchg
	mov a,l! ora h! jnz func10a
	lxi h,buffer+2! shld conbuffadd
	lhld dmaad
func10a:
	push h! lxi d,buffer! push d
	mvi b,0! mov c,m! inx b! inx b! inx b
	xchg! call movef! mvi m,0
	pop d! push d! mvi c,10
	call bank$bdos
	lda buffer+1! mov c,a! mvi b,0
	inx b! inx b
	pop d! pop h! jmp movef

func111:
func112:
	sta res$fx
	xchg! mov e,m! inx h! mov d,m! inx h
	mov c,m! inx h! mov b,m! xchg
	; hl = addr of string
	; bc = length of string
blk$out:
	mov a,b! ora c! rz
	push b! push h! mov c,m
	lxi d,blk$out2! push d
	lda res$fx! cpi 112! jz listf

blk$out0:
	lda conmode! mov b,a! ani 2! jz blk$out1
	mov a,b! ani 14h! jz blk$out1
	ani 10h! jnz sconoutf
	jmp conoutf

blk$out1:
	mov e,c! mvi c,conoutfxx! jmp bank$bdos

blk$out2:
	pop h! inx h! pop b! dcx b
	jmp blk$out

qconinx:
	; switch to bank 1
	mvi a,1! call selmemf
	; get character
	mov b,m
	; return to bank zero
	xra a! call selmemf
	; return with character in A
	mov a,b! ret

switch1:
	lxi d,switch0! push d
	mvi a,1! call selmemf! pchl
switch0:
	mov b,a! xra a! call selmemf
	mov a,b! ret

disk$func:
	cpi ndf! jc OKdf ;func < ndf
	cpi 98! jc badfunc ;ndf < func < 98
	cpi nxdf! jnc badfunc ;func >= nxdf
	cpi 111! jz func111
	cpi 112! jz func112
	jmp disk$function

    OKdf:
	cpi 17! jz search
	cpi 18! jz searchn
	cpi setdmafx! jnz disk$function

	; Set dma addr
	xchg! shld dmaad! shld curdma! ret

    search:
	xchg! shld searcha
	
    searchn:
	lhld searcha! xchg

disk$function:

;
;	Perform the required buffer tranfers from
;	the user bank to common memory
;

	lxi h,dfctbl-12
	mov a,c! cpi 98! jc normalCPM
	lxi h,xdfctbl-98
    normalCPM:
	mvi b,0! dad b! mov a,m

; ****  SAVE DFTBL ITEM, INFO, & FUNCTION *****
 
	mov b,a! push b! push d

	rar! jc cpycdmain			;cdmain test
	rar! jc cpyfcbin			;fcbin test
	jmp nocpyin

    cpycdmain:
	lhld dmaad! xchg
	lxi h,common$dma! lxi b,16
	call movef
	pop d! push d

    cpyfcbin:
	xra a! sta copy$cr$only
	lxi h,commonfcb! lxi b,36
	call movef
	lxi d,commonfcb
	pop h! pop b! push b! push h
	shld user$info

    nocpyin:

	call bank$bdos

	pop d ;restore FCB address
	pop b! mov a,b ;restore fcbtbl byte & function #
	ani 0f8h! rz
	lxi h,commonfcb! xchg! lxi b,33
	ral! jc copy$fcb$back			;fcbout test
	mvi c,36! ral! jc copy$fcb$back		;pfcbout test
	ral! jc cdmacpyout128			;cdmaout128 test
	mvi c,4! ral! jc movef			;timeout test
	ral! jc cdmacpyout003			;cdmaout003 test
	mvi c,6! jmp movef			;seriout 

    copy$fcb$back:
	lda copy$cr$only! ora a! jz movef
	lxi b,14! dad b! xchg! dad b
	mov a,m! stax d
	inx h! inx d
	mov a,m! stax d
	inx b! inx b! inx b! dad b! xchg! dad b
	ldax d! mov m,a! ret

    cdmacpyout003:
	lhld dmaad! lxi b,3! lxi d,common$dma
	jmp movef

    cdmacpyout128:
	lhld dmaad! lxi b,128! lxi d,common$dma
	jmp movef

parse:
	xchg! mov e,m! inx h! mov d,m
	inx h! mov c,m! inx h! mov b,m
	lxi h,buffer+133! push h! push b! push d
	shld buffer+2! lxi h,buffer+4! shld buffer
	lxi b,128! call movef! mvi m,0
	mvi c,152! lxi d,buffer! call bank$bdos
	pop b! mov a,l! ora h! jz parse1
	mov a,l! ana h! inr a! jz parse1
	lxi d,buffer+4
	mov a,l! sub e! mov l,a
	mov a,h! sbb d! mov h,a
	dad b! shld aret
parse1:
	pop h! pop d! lxi b,36! jmp movef

bad$func:
	cpi 152! jz parse

	; A = 0 if fx >= 128, 0ffh otherwise
	ral! mvi a,0! jc sta$ret

	dcr a

sta$ret:
	sta aret

goback:
	lhld entsp! sphl ;user stack restored
	lhld aret! mov a,l! mov b,h ;BA = HL = aret
	ret

BANK$BDOS:

	xra a! call selmemf

	call bnkbdos

	shld aret
	mvi a,1! jmp selmemf ;ret


move$out:
	ora a! jz move$f
	call selmemf
move$ret:
	call movef
	xra a! jmp selmemf

move$tpa:
	mvi a,1! call selmemf
	jmp move$ret

search$hash: ; A = bank # , HL = hash table addr

	; Hash format
	; xxsuuuuu xxxxxxxx xxxxxxxx ssssssss
	; x = hash code of fcb name field
	; u = low 5 bits of fcb user field
	;     1st bit is on for XFCB's
	; s = shiftr(mod || ext,extshf)

	shld hash$tbla! call selmemf
	; Push return address
	lxi h,search$h7! push h
	; Reset read directory record flag
	xra a! sta rd$dir

	lhld hash$tbla! mov b,h! mov c,l
	lhld hashmx! xchg
	; Return with Z flag set if dcnt = hash$mx
	lhld dcnt! push h! call subdh! pop d! ora l! rz
	; Push hash$mx-dcnt (# of hash$tbl entries to search)
	; Push dcnt+1
	push h! inx d! xchg! push h
	; Compute .hash$tbl(dcnt-1)
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
	; Restore stack & hl to .hash$tbl(dcnt)
	dcx h! mov a,l! ora h! xthl! push h
	; Are we done?
	xchg! jnz search$h1 ; no - keep searching
	; Search unsuccessful - return with Z flag reset
	inr a! pop h! pop h! ret
search$h3:
	; Does xdcnt+1 = 0ffh?
	lda xdcnt+1! inr a! jz search$h5 ; yes
	; Does xdcnt+1 = 0feh?
	inr a! jnz search$h2 ; no - continue searching
	; Do hash's match?
	push d! call search$h6! pop d! jnz search$h2 ; no
	; Does find$xfcb = 0ffh?
	lda find$xfcb! inr a! jz search$h45 ; yes
	; Does find$xfcb = 0feh?
	inr a! jz search$h35 ; yes
	; xdcnt+1 = 0feh & find$xfcb < 0feh
	; Open user 0 search
	; Does hash u field = 0?
	mov a,m! ani 1fh! jnz search$h2 ; no
	; Search successful
	jmp search$h4
search$h35:
	; xdcnt+1 = 0feh & find$xfcb = 0feh
	; Delete search to return matching fcb's & xfcbs
	; Do hash user fields match?
	ldax d! xra m! ani 0fh! jnz search$h2 ; no
	; Exclude empty fcbs, sfcbs, and dir lbls
	mov a,m! ani 30h! cpi 30h! jz search$h2
search$h4:
	; successful search
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
	; Set directory read flag
	mvi a,0ffh! sta rd$dir
	xra a! ret
search$h45:
	; xdcnt+1 = 0feh, find$xfcb = 0ffh
	; Rename search to save dcnt of xfcb in xdcnt
	; Is hash entry an xfcb?
	mov a,m! ani 10h! jz search$h2 ; no
	; Do hash user fields agree?
	ldax d! xra m! ani 0fh! jnz search$h2 ; no
	; set xdcnt
	jmp search$h55
search$h5:
	; xdcnt+1 = 0ffh
	; Make search to save dcnt of empty fcb
	; is hash$tbl entry empty?
	mov a,m! cpi 0f5h! jnz search$h2 ; no
search$h55:
	; xdcnt = dcnt
	xchg! pop h! shld xdcnt! jmp search$h25
search$h6:
	; hash compare routine
	; Is hashl = 0?
	lda hashl! ora a! rz ; yes - hash compare successful
	; hash$mask = 0e0h if hashl = 3
	;           = 0c0h if hashl = 2
	mov c,a! rrc! rrc! rar! mov b,a
	; hash s field does not pertain if hashl ~= 3
	; Does hash(0) fields match?
	ldax d! xra m! ana b! rnz ; no
	; Compare remainder of hash fields for hashl bytes
	push h! inx h! inx d! call compare
	pop h! ret
search$h7:
	; Return to bnkbdos
	push a! xra a! call selmemf! pop a! ret

subdh: 
	;compute HL = DE - HL
	mov a,e! sub l! mov l,a
	mov a,d! sbb h! mov h,a
	ret

compare:
	ldax d! cmp m! rnz
	inx h! inx d! dcr c! rz
	jmp compare

;	Disk Function Copy Table

cdmain	equ	00000001B	;copy 1ST 16 bytes of DMA to
				;common$dma on entry
fcbin	equ	00000010b	;fcb copy on entry
fcbout	equ	10000000b	;fcb copy on exit
pfcbout	equ	01000000b	;random fcb copy on exit
cdma128	equ	00100000b	;copy 1st 128 bytes of common$dma 
				;to DMA on exit
timeout equ	00010000b	;copy date & time on exit
cdma003 equ	00001000B	;copy 1ST 3 bytes of common$dma
				;to DMA on exit
serout  equ	00000100b	;copy serial # on exit

dfctbl:
	db 0			; 12=return version #
	db 0			; 13=reset disk system
	db 0			; 14=select disk
	db fcbin+fcbout+cdmain  ; 15=open file
	db fcbin+fcbout		; 16=close file
	db fcbin+cdma128        ; 17=search first
	db fcbin+cdma128      	; 18=search next
	db fcbin+cdmain		; 19=delete file
	db fcbin+fcbout		; 20=read sequential
	db fcbin+fcbout		; 21=write sequential
	db fcbin+fcbout+cdmain	; 22=make file
	db fcbin+cdmain		; 23=rename file
	db 0			; 24=return login vector
	db 0			; 25=return current disk
	db 0			; 26=set DMA address
	db 0			; 27=get alloc address
	db 0			; 28=write protect disk
	db 0			; 29=get R/O vector
	db fcbin+fcbout+cdmain	; 30=set file attributes
	db 0			; 31=get disk param addr
	db 0			; 32=get/set user code
	db fcbin+fcbout		; 33=read random
	db fcbin+fcbout		; 34=write random
	db fcbin+pfcbout	; 35=compute file size
	db fcbin+pfcbout	; 36=set random record
	db 0			; 37=drive reset
	db 0			; 38=access drive
 	db 0			; 39=free drive
	db fcbin+fcbout		; 40=write random w/ zero fill

	db fcbin+fcbout		; 41=test & write record
	db 0			; 42=record lock
	db 0			; 43=record unlock
	db 0			; 44=set multi-sector count
	db 0			; 45=set BDOS error mode
	db cdma003		; 46=get disk free space
	db 0     		; 47=chain to program
	db 0			; 48=flush buffers
	db fcbin		; 49=Get/Set system control block
	db fcbin		; 50=direct BIOS call (CP/M)
ndf equ ($-dfctbl)+12

xdfctbl:
	db 0			; 98=reset allocation vectors
	db fcbin+cdmain		; 99=truncate file
	db fcbin+cdmain		; 100=set directory label
	db 0     		; 101=return directory label data
	db fcbin+fcbout+cdmain	; 102=read file xfcb
	db fcbin+cdmain		; 103=write or update file xfcb
	db fcbin		; 104=set current date and time
	db fcbin+timeout	; 105=get current date and time
	db fcbin		; 106=set default password
	db fcbin+serout	 	; 107=return serial number
	db 0			; 108=get/set program return code
	db 0			; 109=get/set console mode
	db 0			; 110=get/set output delimiter
	db 0			; 111=print block
	db 0			; 112=list block

nxdf equ ($-xdfctbl)+98

res$fx:	ds	1
hash$tbla:
	ds	2
bank:	ds	1
aret:	ds	2	;address value to return

buffer:			;function 10 256 byte buffer

commonfcb:
	ds	36	;fcb copy in common memory

common$dma:
        ds	220	;function 10 buffer cont.

	ds	ssize*2
lstack:
entsp:	ds	2

; BIOS intercept vector

wbootfx:	jmp	wbootf
		jmp	switch1
constfx:	jmp	constf
		jmp	switch1
coninfx:	jmp	coninf
		jmp	switch1
conoutfx:	jmp	conoutf
		jmp	switch1
listfx:		jmp	listf
		jmp	switch1
		
		dw	0,0,0
		dw	0,0

olog:		dw	0
rlog:		dw	0

patch$flgs:	dw	0,0

; Base of RESBDOS

	dw	base+6

; Reserved for use by non-banked BDOS

	ds	2

; System Control Block

SCB:

; Expansion Area - 6 bytes

hashl:		db	0	;hash length (0,2,3)
hash:		dw	0,0	;hash entry
version:	db	31h	;version 3.1

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
		dw	buffer+64
outdelim:	db	'$'
listcp:		db	0
qflag:		db	0

; BDOS Section - 42 bytes

scbadd:		dw	scb
dmaad:		dw	0080h
seldsk:		db	0
info:		dw	0
resel:		db	0
relog: 		db	0
fx:		db	0
usrcode:	db	0
dcnt:		dw	0
searcha:	dw	0
searchl:	db	0
multcnt:	db	1
errormode:	db	0
searchchain:	db	0,0ffh,0ffh,0ffh
temp$drive:	db	0
errdrv:      	db	0
		dw	0
media$flag:	db	0
      		dw	0
bdos$flags:	db	80h
stamp:		db	0ffh,0ffh,0ffh,0ffh,0ffh
commonbase:	dw	0
error: 		jmp	error$jmp
bdosadd:	dw	base+6
		end

