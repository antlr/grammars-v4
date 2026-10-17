	title	'CP/M BDOS Interface, BDOS, Version 3.0 Dec, 1982'
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
;       December 1982
;
on	equ	0ffffh
off	equ	00000h
MPM	equ	off
BANKED	equ	off

;
;	equates for non graphic characters
;

ctla	equ	01h	; control a
ctlb	equ	02h	; control b
ctlc	equ	03h	; control c
ctle	equ	05h	; physical eol
ctlf	equ	06h	; control f
ctlg	equ	07h	; control g
ctlh	equ	08h	; backspace
ctlk	equ	0bh	; control k
ctlp	equ	10h	; prnt toggle
ctlq	equ	11h	; start screen
ctlr	equ	12h	; repeat line
ctls	equ	13h	; stop screen
ctlu	equ	15h	; line delete
ctlw	equ	17h	; control w
ctlx	equ	18h	; =ctl-u
ctlz	equ	1ah	; end of file
rubout	equ	7fh	; char delete
tab	equ	09h	; tab char
cr	equ	0dh	; carriage return
lf	equ	0ah	; line feed
ctl	equ	5eh	; up arrow

		org	0000h
base		equ	$

; Base page definitions

bnkbdos$pg	equ	base+0fc00h
resbdos$pg	equ	base+0fd00h
scb$pg		equ	base+0fb00h
bios$pg		equ	base+0ff00h

; Bios equates

bios		equ	bios$pg
bootf		equ	bios$pg 	; 00. cold boot function

if BANKED

wbootf		equ	scb$pg+68h	; 01. warm boot function
constf		equ	scb$pg+6eh  	; 02. console status function
coninf		equ	scb$pg+74h	; 03. console input function
conoutf		equ	scb$pg+7ah	; 04. console output function
listf		equ	scb$pg+80h	; 05. list output function

else

wbootf		equ	bios$pg+3	; 01. warm boot function
constf		equ	bios$pg+6	; 02. console status function
coninf		equ	bios$pg+9	; 03. console input function
conoutf		equ	bios$pg+12	; 04. console output function
listf		equ	bios$pg+15	; 05. list output function

endif

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
devtblf		equ	bios$pg+60	; 20. retunr device table address fx
devinitf	equ	bios$pg+63	; 21. initialize device function
drvtblf		equ	bios$pg+66	; 22. return drive table address
multiof		equ	bios$pg+69	; 23. multiple i/o function
flushf		equ	bios$pg+72	; 24. flush function
movef		equ	bios$pg+75	; 25. memory move function
timef		equ	bios$pg+78	; 26. system get/set time function
selmemf		equ	bios$pg+81	; 27. select memory function
setbnkf		equ	bios$pg+84	; 28. set dma bank function
xmovef		equ	bios$pg+87	; 29. extended move function

if BANKED

; System Control Block equates

olog		equ	scb$pg+090h
rlog		equ	scb$pg+092h

SCB		equ	scb$pg+09ch

; Expansion Area - 6 bytes

hashl		equ	scb$pg+09ch
hash		equ	scb$pg+09dh
version		equ	scb$pg+0a1h

; Utilities Section - 8 bytes

util$flgs	equ	scb$pg+0a2h
dspl$flgs	equ	scb$pg+0a6h

; CLP Section - 4 bytes

clp$flgs	equ	scb$pg+0aah
clp$errcde	equ	scb$pg+0ach

; CCP Section - 8 bytes

ccp$comlen	equ	scb$pg+0aeh
ccp$curdrv	equ	scb$pg+0afh
ccp$curusr	equ	scb$pg+0b0h
ccp$conbuff	equ	scb$pg+0b1h
ccp$flgs	equ	scb$pg+0b3h

; Device I/O Section - 32 bytes

conwidth	equ	scb$pg+0b6h
column		equ	scb$pg+0b7h
conpage		equ	scb$pg+0b8h
conline		equ	scb$pg+0b9h
conbuffadd	equ	scb$pg+0bah
conbufflen	equ	scb$pg+0bch
conin$rflg	equ	scb$pg+0beh
conout$rflg	equ	scb$pg+0c0h
auxin$rflg	equ	scb$pg+0c2h
auxout$rflg	equ	scb$pg+0c4h
lstout$rflg	equ	scb$pg+0c6h
page$mode	equ	scb$pg+0c8h
pm$default	equ	scb$pg+0c9h
ctlh$act	equ	scb$pg+0cah
rubout$act	equ	scb$pg+0cbh
type$ahead	equ	scb$pg+0cch
contran		equ	scb$pg+0cdh
conmode		equ	scb$pg+0cfh
outdelim	equ	scb$pg+0d3h
listcp		equ	scb$pg+0d4h
qflag		equ	scb$pg+0d5h

; BDOS Section - 42 bytes

scbadd		equ	scb$pg+0d6h
dmaad		equ	scb$pg+0d8h
olddsk		equ	scb$pg+0dah
info		equ	scb$pg+0dbh
resel		equ	scb$pg+0ddh
relog 		equ	scb$pg+0deh
fx		equ	scb$pg+0dfh
usrcode		equ	scb$pg+0e0h
dcnt		equ	scb$pg+0e1h
;searcha	equ	scb$pg+0e3h
searchl		equ	scb$pg+0e5h
multcnt		equ	scb$pg+0e6h
errormode	equ	scb$pg+0e7h
searchchain	equ	scb$pg+0e8h
temp$drive	equ	scb$pg+0ech
errdrv      	equ	scb$pg+0edh
media$flag	equ	scb$pg+0f0h
bdos$flags	equ	scb$pg+0f3h
stamp		equ	scb$pg+0f4h
commonbase	equ	scb$pg+0f9h
error		equ	scb$pg+0fbh	;jmp error$sub
bdosadd		equ	scb$pg+0feh

; Resbdos equates

resbdos		equ	resbdos$pg
move$out	equ	resbdos$pg+9	; a=bank #, hl=dest, de=srce
move$tpa	equ	resbdos$pg+0ch	; a=bank #, hl=dest, de=srce
srch$hash	equ	resbdos$pg+0fh	; a=bank #, hl=hash table addr
hashmx		equ	resbdos$pg+12h	; max hash search dcnt
rd$dir$flag	equ	resbdos$pg+14h	; directory read flag
make$xfcb	equ	resbdos$pg+15h	; make function flag
find$xfcb	equ	resbdos$pg+16h	; search function flag
xdcnt		equ	resbdos$pg+17h	; dcnt save for empty fcb, 
					; user 0 fcb, or xfcb
xdmaad		equ	resbdos$pg+19h	; resbdos dma copy area addr
curdma		equ	resbdos$pg+1bh	; current dma
copy$cr$only	equ	resbdos$pg+1dh	; dont restore fcb flag
user$info	equ	resbdos$pg+1eh	; user fcb address
kbchar		equ	resbdos$pg+20h  ; conbdos look ahead char
qconinx		equ	resbdos$pg+21h	; qconin mov a,m routine

ELSE

move$out	equ	movef
move$tpa	equ	movef

ENDIF

;
serial: db	'654321'
;
;	Enter here from the user's program with function number in c,
;	and information address in d,e
;

bdose:	; Arrive here from user programs
	xchg! shld info! xchg ; info=de, de=info

	mov a,c! sta fx! cpi 14! jc bdose2
	lxi h,0! shld dircnt ; dircnt,multnum = 0
	lda olddsk! sta seldsk ; Set seldsk

if BANKED
	dcr a! sta copy$cr$init
ENDIF

	; If mult$cnt ~= 1 then read or write commands
	; are handled by the shell
	lda mult$cnt! dcr a! jz bdose2
	lxi h,mult$fxs
bdose1:
	mov a,m! ora a! jz bdose2
	cmp c! jz shell
	inx h! jmp bdose1
bdose2:
	mov a,e! sta linfo ; linfo = low(info) - don't equ
	lxi h,0! shld aret ; Return value defaults to 0000
	shld resel ; resel,relog = 0
	; Save user's stack pointer, set to local stack
	dad sp! shld entsp ; entsp = stackptr

if not BANKED
	lxi sp,lstack ; local stack setup
ENDIF

	lxi h,goback ; Return here after all functions
	push h ; jmp goback equivalent to ret
	mov a,c! cpi nfuncs! jnc high$fxs ; Skip if invalid #
	mov c,e ; possible output character to c
	lxi h,functab! jmp bdos$jmp
	; look for functions 98 ->
high$fxs:
	cpi 128! jnc test$152
	sui 98! jc lret$eq$ff ; Skip if function < 98
	cpi nfuncs2! jnc lret$eq$ff
	lxi h,functab2
bdos$jmp:
	mov e,a! mvi d,0 ; de=func, hl=.ciotab
	dad d! dad d! mov e,m! inx h! mov d,m ; de=functab(func)
	lhld info ; info in de for later xchg	
	xchg! pchl ; dispatched

;	   CAUTION: In banked systems only,
;          error$sub is referenced indirectly by the SCB ERROR
; 	   field in RESBDOS as (0fc7ch).  This value is converted
; 	   to the actual address of error$sub by GENSYS.  If the offset
; 	   of error$sub is changed, the SCB ERROR value must also
; 	   be changed.

;
;	error subroutine
;

error$sub:
	mvi b,0! push b! dcr c
	lxi h,errtbl! dad b! dad b
	mov e,m! inx h! mov d,m! xchg
	call errflg
	pop b! lda error$mode! ora a! rnz
	jmp reboote

mult$fxs:	db	20,21,33,34,40,0

if BANKED
	db	'COPYRIGHT (C) 1982,'
	db	' DIGITAL RESEARCH '
	db	'151282'
else
	db	'COPR. ''82 DRI 151282'

	;	31 level stack

	dw	0c7c7h,0c7c7h,0c7c7h,0c7c7h,0c7c7h,0c7c7h,0c7c7h
	dw	0c7c7h,0c7c7h,0c7c7h,0c7c7h,0c7c7h,0c7c7h,0c7c7h,0c7c7h
	dw	0c7c7h,0c7c7h,0c7c7h,0c7c7h,0c7c7h,0c7c7h,0c7c7h,0c7c7h
	dw	0c7c7h,0c7c7h,0c7c7h,0c7c7h,0c7c7h,0c7c7h,0c7c7h,0c7c7h
lstack:

endif

;	dispatch table for functions

functab:
	dw	rebootx1, func1, func2, func3
	dw	punchf, listf, func6, func7
	dw	func8, func9, func10, func11
diskf	equ	($-functab)/2	; disk funcs
	dw	func12,func13,func14,func15
	dw	func16,func17,func18,func19
	dw	func20,func21,func22,func23
	dw	func24,func25,func26,func27
	dw	func28,func29,func30,func31
	dw	func32,func33,func34,func35
	dw	func36,func37,func38,func39
	dw	func40,lret$eq$ff,func42,func43
	dw	func44,func45,func46,func47
	dw	func48,func49,func50
nfuncs	equ	($-functab)/2

functab2:
	dw	func98,func99
	dw	func100,func101,func102,func103
	dw	func104,func105,func106,func107
	dw	func108,func109,func110,func111
	dw	func112

nfuncs2	equ	($-functab2)/2

errtbl:
	dw	permsg
	dw	rodmsg
	dw	rofmsg
	dw	selmsg
	dw	0
	dw	0
	dw	passmsg
	dw	fxstsmsg
	dw	wildmsg

test$152:
	cpi 152! rnz

;
;	PARSE version 3.0b  Oct 08 1982 - Doug Huskey
;
;
      	; DE->.(.filename,.fcb)
	;
	; filename = [d:]file[.type][;password]
	;             
	; fcb assignments
	;
	;   0     => drive, 0 = default, 1 = A, 2 = B, ...
	;   1-8   => file, converted to upper case,
	;            padded with blanks (left justified)
	;   9-11  => type, converted to upper case,
	;	     padded with blanks (left justified)
	;   12-15 => set to zero
	;   16-23 => password, converted to upper case,
	;	     padded with blanks
	;   24-25 => 0000h
	;   26    => length of password (0 - 8)
	;
	; Upon return, HL is set to FFFFH if DE locates
	;            an invalid file name;
	; otherwise, HL is set to 0000H if the delimiter
	;            following the file name is a 00H (NULL)
	; 	     or a 0DH (CR);
	; otherwise, HL is set to the address of the delimiter
	;            following the file name.
	;
	lxi 	h,sthl$ret
	push	h
	lhld 	info
	mov	e,m		;get first parameter
	inx	h
	mov	d,m
	push	d		;save .filename
	inx	h
	mov	e,m		;get second parameter
	inx	h
	mov	d,m
	pop	h		;DE=.fcb  HL=.filename
	xchg
parse0:
	push	h		;save .fcb
	xra	a
	mov	m,a		;clear drive byte
	inx	h
	lxi	b,20h*256+11
	call	pad		;pad name and type w/ blanks
	lxi	b,4
	call	pad		;EXT, S1, S2, RC = 0
	lxi	b,20h*256+8
	call	pad		;pad password field w/ blanks
	lxi	b,12
	call	pad		;zero 2nd 1/2 of map, cr, r0 - r2
;
;	skip spaces
;
	call	skps
;
;	check for drive
;
	ldax	d
	cpi	':'		;is this a drive?
	dcx	d
	pop	h
	push	h		;HL = .fcb
	jnz	parse$name
;
;	Parse the drive-spec
;
parsedrv:
	call 	delim
	jz	parse$ok
	sui	'A'
	jc	perror1
	cpi	16
	jnc	perror1
	inx	d
	inx	d		;past the ':'
	inr	a		;set drive relative to 1
	mov	m,a		;store the drive in FCB(0)
;
;	Parse the file-name
;
parse$name:
	inx	h		;HL = .fcb(1)
	call	delim
	jz	parse$ok
	lxi	b,7*256

parse6:	ldax	d		;get a character
	cpi	'.'		;file-type next?
	jz	parse$type	;branch to file-type processing
	cpi	';'
	jz	parse$pw
	call	gfc		;process one character
	jnz	parse6		;loop if not end of name
	jmp	parse$ok
;
;	Parse the file-type
;
parse$type:	
	inx	d		;advance past dot
	pop	h
	push	h		;HL =.fcb
	lxi	b,9
	dad	b		;HL =.fcb(9)
	lxi	b,2*256

parse8:	ldax	d
	cpi	';'
	jz	parsepw
	call	gfc		;process one character
	jnz	parse8		;loop if not end of type
;
parse$ok:
	pop	b
	push	d
	call	skps		;skip trailing blanks and tabs
	dcx	d
	call	delim		;is next nonblank char a delim?
	pop	h
	rnz			;no
	lxi	h,0
	ora	a
	rz			;return zero if delim = 0
	cpi	cr
	rz			;return zero if delim = cr
	xchg
	ret
;
;	handle parser error
;
perror:
	pop	b			;throw away return addr
perror1:
	pop	b
	lxi	h,0ffffh
	ret
;
;	Parse the password
;
parsepw:
	inx	d
	pop	h
	push	h
	lxi	b,16
	dad	b
	lxi	b,7*256+1
parsepw1:
	call	gfc
	jnz	parsepw1
	mvi	a,7
	sub	b
	pop	h
	push	h
	lxi	b,26
	dad	b
	mov	m,a
	ldax	d			;delimiter in A
	jmp	parse$ok
;
;	get next character of name, type or password
;
gfc:	call	delim		;check for end of filename
	rz			;return if so
	cpi	' '		;check for control characters
	inx	d
	jc	perror		;error if control characters encountered
	inr	b		;error if too big for field
	dcr	b
	jm	perror
	inr	c
	dcr	c
	jnz	gfc1
	cpi	'*'		;trap "match rest of field" character
	jz	setmatch
gfc1:	mov	m,a		;put character in fcb
	inx	h
	dcr	b		;decrement field size counter
	ora	a		;clear zero flag
	ret
;;
setmatch:
	mvi	m,'?'		;set match one character
	inx	h
	dcr	b
	jp	setmatch
	ret
;	
;	check for delimiter
;
;	entry:	A = character
;	exit:	z = set if char is a delimiter
;
delimiters:	db	cr,tab,' .,:;[]=<>|',0

delim:	ldax	d		;get character
	push	h
	lxi	h,delimiters
delim1:	cmp	m		;is char in table
	jz	delim2
	inr	m
	dcr	m		;end of table? (0)
	inx	h
	jnz	delim1
	ora	a		;reset zero flag
delim2:	pop	h
	rz
	;
	;	not a delimiter, convert to upper case
	;
	cpi	'a'
	rc
	cpi	'z'+1
	jnc	delim3
	ani 	05fh
delim3:	ani	07fh	
	ret			;return with zero set if so
;
;	pad with blanks or zeros
;
pad:	mov	m,b
	inx	h
	dcr	c
	jnz	pad
	ret
;
;	skip blanks and tabs
;
skps:	ldax	d
	inx	d
	cpi	' '		;skip spaces & tabs
	jz 	skps
	cpi	tab
	jz	skps
	ret
;
;	end of PARSE
;

errflg:
	; report error to console, message address in hl
	push h! call crlf ; stack mssg address, new line
	lda adrive! adi 'A'! sta dskerr ; current disk name
	lxi b,dskmsg

if BANKED
	call zprint ; the error message
else
	call print
endif

	pop b

if BANKED
	lda bdos$flags! ral! jnc zprint
	call zprint ; error message tail
	lda fx! mvi b,30h
	lxi h,pr$fx1
	cpi 100! jc errflg1
	mvi m,31h! inx h! sui 100
errflg1:
	sui 10! jc errflg2
	inr b! jmp errflg1
errflg2:
	mov m,b! inx h! adi 3ah! mov m,a
	inx h! mvi m,20h
	lxi h,pr$fcb! mvi m,0
	lda resel! ora a! jz errflg3
	mvi m,20h! push d
	lhld info! inx h! xchg! lxi h,pr$fcb1
	mvi c,8! call move! mvi m,'.'! inx h
	mvi c,3! call move! pop d
errflg3:
	call crlf
	lxi b,pr$fx! jmp zprint

zprint:
	ldax b! ora a! rz
	push b! mov c,a
	call tabout
	pop b! inx b! jmp zprint

pr$fx:	db	'BDOS Function = '
pr$fx1:	db	'   '
pr$fcb:	db	' File = '
pr$fcb1:ds	12
	db	0

else
	jmp	print
endif

reboote: 
	lxi h,0fffdh! jmp rebootx0 ; BDOS error
rebootx:
	lxi h,0fffeh ; CTL-C error
rebootx0:
	shld clp$errcde
rebootx1:
	jmp wbootf

entsp:	ds	2	; entry stack pointer

shell:
	lxi h,0! dad sp! shld shell$sp

if not BANKED
	lxi sp,shell$stk
endif

	lxi h,shell$rtn! push h
	call save$rr! call save$dma
	lda mult$cnt
mult$io:
	push a! sta mult$num! call cbdos
	ora a! jnz shell$err
	lda fx! cpi 33! cnc incr$rr
	call adv$dma
	pop a! dcr a! jnz mult$io
	mov h,a! mov l,a! ret

shell$sp:	dw	0

		dw	0c7c7h,0c7c7h,0c7c7h,0c7c7h,0c7c7h

shell$stk:		; shell has 5 level stack
hold$dma:	dw	0

cbdos:
	lda fx! mov c,a
cbdos1:
	lhld info! xchg! jmp bdose2

adv$dma:
	lhld dmaad! lxi d,80h! dad d! jmp reset$dma1

save$dma:
	lhld dmaad! shld hold$dma! ret

reset$dma:
	lhld hold$dma
reset$dma1:
	shld dmaad! jmp setdma

shell$err:
	pop b! inr a! rz
	lda mult$cnt! sub b! mov h,a! ret

shell$rtn:
	push h! lda fx! cpi 33! cnc reset$rr
	call reset$dma
	pop d! lhld shell$sp! sphl! xchg
	mov a,l! mov b,h! ret

	page


