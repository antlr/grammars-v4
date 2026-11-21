	title 'CP/M 3 DUMP Utility'
	;***************************
	;***************************
	;**                       **
	;**        D U M P        **
	;**                       **
	;**  FILE  DUMP  ROUTINE  **
	;**                       **
	;**    JULY  16  1982     **
	;**                       **
 	;***************************
	;***************************
	;
	;
	;
	org	100h		;base of TPA
	;
	;******************
	;* BDOS Functions *
	;******************
return  equ	0		;System reset
conin	equ	01		;Read console
conout	equ	02		;Type character
bdos	equ	05		;DOS entry point
input	equ	06		;Raw console I/O
pstring	equ	09		;Type string
rstring	equ	10		;Read connsole buffer
chkio	equ	11		;Console status
reset	equ	13		;Reset Disk System
openf	equ	15		;Open file
readf	equ	20		;Read buffer
dmaf	equ	26		;Set DMA address
fsize	equ	35		;Compute file size
errmode equ	45		;Set ERROR mode 
getscb	equ	49		;Get/Set SCB
conmode equ	109		;Set console mode			
	;**************************
	;* Non Graphic Characters *
	;**************************
ctrlc	equ	03h		;control - C (^C)
ctrlx	equ	018h		;control - X (^X)
cr	equ	0dh		;carriage return
lf      equ	0ah		;line feed
	;
	;*******************
	;* FCB definitions *
	;*******************
fcb	equ	5ch		;File Control Block
buf	equ	80h		;Password Buffer Location
	;
	;*****************
	;* Begin Program *
	;*****************
	jmp	begin
	;
	;*********************************************
	;* Patch Area, Date, Version & Serial Number *
	;*********************************************
dw	0,0,0,0,0,0
db	0
db	'DUMP VERSION 3.0'
db	'   DUMP.COM     '
dw	0,0,0,0,0,0,0,0
dw      0,0,0,0,0,0,0,0
db	'COPYRIGHT 1982, '
db	'DIGITAL RESEARCH'
db	'151282'		;version date  [day-month-year]
db	0,0,0,0			;patch bit map
db	'654321'		;Serial Number
	;
pgraph:				;print graphic char. in ACC. or period
	cpi	7fh
	jnc	pperiod
	cpi	' '
	jnc	pchar
	;
pperiod:			;print period
	mvi	a,'.'
	jmp	pchar
	;
pchar:				;print char. in ACC. to console
	push	h		
	push	d
	push	b
	mov	e,a		;value in ACC. is put in register E 
	mvi	c,conout	;value in register E is sent to console  
	call	bdos		;print character
	pop	b
	pop	d
	pop	h
	ret
	;
pnib:				;print nibble in low Acc. 
	cpi	10
	jnc	pnibh		;jump if 'A-F'
	adi	'0'
	jmp	pchar
	;
pnibh:
	adi	'A'-10
	jmp	pchar
	;
pbyte:				;print byte in hex
	push	psw		;save copy for low nibble
	rar			;rotate high nibble to low
	rar
	rar
	rar
	ani	0fh		;mask high nibble 
	call	pnib
	pop	psw
	ani	0fh
	jmp	pnib
	;
openfile:
	mvi	c,openf
	lxi	d,fcb
	call	bdos		;open file
	sta	keepa
	mov	a,h
	cpi	07		;check password status
	jz	getpasswd	;Reg. H contains '7' if password exists
	lda	keepa
	cpi	0ffh		;ACC.=FF if there is no file found
	jz	nofile
	ret
	;
getpasswd:
	lda	tpasswd
	cpi	255		;check if already tried password
	jz	wrngpass
	call	space		;set password memory area too blanks
	lxi	d,quest
	call	print		;print question
	mvi	a,8		;max # of characters able to input
	sta	buf		;for password is eight (8) 
	mvi	c,rstring
	lxi	d,buf
	call	bdos		;get password
	lda	buf+1
	sta	len		;store length of password
	cpi	0		
	jz	stop		;if <cr> entered then stop program
	call	cap		;cap the password
	lxi	d,buf+2
	call	setdma
	mvi	a,255
	sta	tpasswd		;set Tried Password Flag
	mvi	a,0
	jmp	openfile
	;
space:				;this routine fills the memory	
	mvi	a,8		;locations from 82-89H with
	lxi	h,buf+2		;a space
space2:
	mvi	m,' '		;put a (blank) into the memory
	inx	h		;location where HL are pointing 
	dcr	a		
	jnz	space2
	ret
	;
cap:				;this routine takes the inputed 
	mvi	b,8		;Password and converts it to
	lxi	h,buf+2		;upper-case letters
cap2:
	mov	a,m		;move into the ACC. where the
	cpi	'a'		;current HL position points to
	jc	skip		;and if it is a lower-case letter
	cpi	'{'		;make it upper case
	jnc	skip
	sui	20h
	mov	m,a
skip:
	inx	h		;inc the pointer to the next letter
	dcr	b
	jnz	cap2
delchar:			;this routine deletes the last
	lda	len		;character in the input because
	adi	82h		;an extra character is added to
	sta	len2		;the input when using BDOS function 10
	lhld	len2		
	mvi	m,' '
	ret
	;
fillbuff:
	lxi	d,buff		;current position
fillbuff2:
	sta	keepa
	push	d		
	call	setdma		;set DMA for file reading
	call	readbuff	;read file and fill BUFF
	lda	norec		;# records read in current loop
	inr	a
	sta	norec
	cpi	8		;check if '8' records read in loop
	jz	loop2
	pop	d
	lxi	h,80h		;80h=128(decimal)= # bytes in 1 record read
	dad	d
	xchg			;changes DMA = DMA+80h
	jmp	fillbuff2
	;
setdma:
	mvi	c,dmaf
	call	bdos		;set DMA
	ret
	;
readbuff:
	mvi	c,readf
	lxi	d,fcb
	call	bdos		;fill buffer
	cpi	0		;ACC. <> 0 if unsuccessful
	rz			;return if not End Of File 
	lda	norec		
	cpi	0		;this check is needed to see if
	jz	stop		;the record is the first in the
	mvi	a,255		;loop
	sta	eof		;set End Of File flag
	jmp	loop2		;no more buff reading
	;
break:
	push	b
	push	d		;see if character ready 
	push	h		;if so then quit program
	mvi	c,chkio		;if character is a ^C	
	call	bdos		;check console status
	ora	a		;zero flag is set if no character
	push	psw		;save all registers
	mvi	c,conin		;console in function
	cnz	bdos		;eat character if not zero
	pop	psw		;restore all registers
	pop	h
	pop	d
	pop	b
	ret			
	;
paddr:
	lhld	aloc		;current display address
	mov	a,h
	call	pbyte		;high byte
	mov	a,l
	lhld	disloc
	call	pbyte		;low byte
	mvi	a,':'
	jmp	pchar
	;
page$check:
	lda	page$on
	cpi	0
	cz	page$count	;if page mode on call routine
	ret
	;
crlf:	
	mvi	a,cr
	call	pchar
	mvi	a,lf
	jmp	pchar
	;
blank:	
	mvi	a,' '
	jmp	pchar
	;
page$count:
	lda	page$size	;relative to zero
	mov	e,a		
	lda	count		;current number of lines 
	cmp	e
	jz	stop$display	;if xx lines then stop display
	inr	a
	sta	count		;count=count+1
	ret
	;
stop$display:
	mvi	a,0
	sta	count		;count=0
	lxi	d,con$mess
	call	print
stop$display2:
	mvi	c,input
	mvi	e,0fdh
	call	bdos
	cpi	ctrlc
	jz	stop
	cpi	cr		;compare character with <CR>
	jnz	stop$display2	;wait until <CR> is encountered
	mvi	a,ctrlx
	jmp	pchar
	;
discom:				;check line format
	xchg
	lhld	dismax
	mov	a,l
	sub	e
	mov	l,a
	mov	a,h
	sbb	d
	xchg
	ret
	;
display:
	lhld	size		;[(norec)x(128)]-1
	xchg
	lxi	h,buff		;buffer location
	shld	disloc
	dad	d
	;
display2:
	shld	dismax
	;
display3:
	call	page$check
	call	crlf
	call	break
	jnz	stop		;if key pressed then quit
	lhld	disloc
	shld	tdisp
	call	paddr		;print the line address
	;
display4:
	call	blank
	mov	a,m		
	call	pbyte		;print byte
	inx	h		;increment the current buffer location
	push	h		
	lhld	aloc		;aloc is current address for the display
	mov	a,l
	ani	0fh
	cpi	0fh		;check if 16 bytes printed	
	inx	h		;increment current display address
	shld	aloc		;save it
	pop	h		
	jnz	display4	;if not then continue
	;
display5:
	shld	disloc		;save the current place
	lhld	tdisp		;load current place - 16
	xchg
	call	blank
	call    blank
	;
display6:
	ldax	d		;get byte
	call	pgraph		;print if graphic character
	inx	d
	lhld	disloc
	mov	a,l
	sub	e
	jnz	display6
	mov	a,h
	sub	d
	jnz	display6
	lhld	disloc
	call	discom		;end of display ?
	rc
	jmp	display3
	;
pintro:
	lxi	d,intromess
	call	print
	ret
	;
setmode:			;this routine allows error codes
	mvi	c,errmode	;to be detected in the ACC. and
	mvi	e,255		;Reg. H instead of BDOS ERROR
	call	bdos		;Messages
	mvi	c,conmode	;and also sets the console status
	lxi	d,1		;so that only a ^C can affect
	call	bdos		;function 11
	ret
	;
check$page:
	mvi	c,getscb	;Get/Set SCB function
	lxi	d,page$mode
	call	bdos
	cpi	0
	rnz			;return if mode is off (false)
	sta	page$on		;set 'on' byte
	mvi	c,getscb
	lxi	d,page$len
	call	bdos
	dcr	a
	sta	page$size	;store page length (relative to zero)
	ret
	;
checkfile:
	mvi	c,fsize
	lxi	d,fcb
	call	bdos
	lda	fcb+33
	cpi	0
	rnz
	lxi	d,norecmess
	call	print
	jmp	stop
	;
chngsize:			;if odd number of records read
	sta 	keepa		;this routine adds 128 or
	mvi	a,80h		;80h to the display size
	mov	l,a		;because the ACC. cannot deal
	lda	keepa		;with decimals
	ret
	;
print:				;prints the string where
	mvi	c,pstring	;DE are pointing to
	call	bdos
	ret
	;
nofile:
	mvi	c,pstring
	lxi	d,nofmess
	call	bdos		;print 'FILE NOT FOUND'
	jmp	stop
	;
wrngpass:	
	lxi	d,badpass
	call    print		;print 'False Password'
	;
stop:				;stop program execution
	mvi	c,reset
	call	bdos
	mvi	c,return
	call	bdos
	;
begin:
	lxi	sp,stack
	call	pintro		;print the intro
	call	setmode		;set ERROR mode
	call	check$page	;check console page mode
	 call	openfile	;open the file
	call	checkfile	;check if reany records exist
	;	
loop:
	jmp 	fillbuff	;fill the buffer(s)	
loop2:
	mvi	l,0		;set L = 0  
	lda	norec		;norec is set by fillbuff routine
	rar			;(x128) or (/2)
	cc	chngsize	;if odd # records read then call this routine
	mov	h,a		
	dcx	h
	shld	size		;number of bytes to display
	pop	d
	call	display		;call display routine
	lda	eof
	cpi	255		
	jz	stop		;jump if End Of File
	mvi	a,0
	sta	norec		;reset # records read to 0
	jmp	loop
	;
	;****************************
	;* Console Messages To User *
	;****************************
intromess:	db cr,lf,lf,'CP/M 3 DUMP - Version 3.0$'
nofmess:	db cr,lf,'ERROR: File Not Found',cr,lf,'$'
quest:		db cr,lf,'Enter Password: $'
badpass:	db cr,lf,'Password Error$'
norecmess:	db cr,lf,'ERROR: No Records Exist$'
con$mess:	db cr,lf,'Press RETURN to continue $'
	;
	;*****************************
	;* Variable and Storage Area *
	;*****************************	
dismax:		ds 2		;Max.# reference
tdisp:  	ds 2		;Current buffer location (for ASCII)
disloc: 	ds 2		;Current buffer loocation
aloc:   	dw 0		;Line address
ploc:   	ds 2		;Current buffer location storage
keepa:  	ds 2		;Storage for ACC.
norec:  	db 0		;# of records read in certain loop (1-8)
eof:		db 0 		;End Of File flag
tpasswd:	dw 0		;Tried Password flag
size:   	dw 0		;Display size 
page$mode:	db 02ch		;page mode offset relative to SCB
		db 00h
page$len:	db 01ch		;page length offset relative to SCB
		db 00h
page$on:	db 0ffh		;page ON/OFF flag (0=ON)
page$size:	db 00h		;page length relative to zero 
count:		db 0		;line counter
len:		dw 0		;Password Input length
len2:		dw 0		;Extra character pointer
		ds 12h	
stack:  	ds 2
buff:		ds 1024		;The buffer (holds up to 400h = 1k)
end:
