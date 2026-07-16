	title 'CP/M 3 Patch - Version 3.0'
	;***************************
	;***************************
	;**                       **
	;**       P A T C H       **
	;**                       **
	;**    AUGUST  15  1982   **
	;**                       **
	;***************************	
	;***************************
	;
	;
	org	100h			;beginning of TPA
	;
	;********************************
	;* BDOS Functions 
	;********************************
return	equ	0			;Return to CCP 
conout	equ	2			;Console Output
conin	equ	6			;Console Input
pstring	equ	9			;Print String
rstring	equ	10			;Read String
version equ     12			;CP/M Version
openf	equ	15			;Open File
closef  equ	16			;Close File
readf	equ	20			;Read File
dmaf	equ	26			;Set DMA
writerf equ	34			;Write Random 
errmode	equ	45			;Set ERROR Mode
	;
	;********************************
	;* Non Graphic Characters 
	;********************************
cr	equ	0dh			;Carriage Return <CR>
lf	equ	0ah			;Line Feed ^J
ctrlx	equ	018h			;^X
ctrlc	equ	03h			;^C
bak	equ	08h			;<-
rub	equ	07fh			;<- (DEL)
	;
	;********************************
	;* FCB, BUFFER and BDOS Locations
	;********************************
bdos	equ	05h			;BDOS entry point
fcb	equ	05ch			;File Control Block
spec	equ	065h			;File Spec Beginning Location
buf	equ	080h			;Password Buffer
	;
	;********************************
	;* Beginning of Program 
	;********************************
	jmp	begin
	;
	;********************************
	;* Patch Header / Patch Area
	;********************************
	dw	0,0,0,0,0,0		;Undefined Area
	db	0			
	db	'PATCH VERSION3.0'	;Program name and version
	db	'   PATCH.COM    '	;Program as Found on Disk
	dw	0,0,0,0,0,0,0,0		;Undefined Area
	dw	0,0,0,0,0,0,0,0
	db	'COPYRIGHT 1982, '	;Copyright and Year
	db	'DIGITAL RESEARCH'	
	db	'151282'		;Version Date [day-month-year]
	db	0,0,0,0			;Patch Bit Map
	db	'654321'		;Serial Number Identifier
	;
	;********************************
	;* Beginning of Program 
	;********************************
	jmp	begin			;Begin Program
	;
	;********************************
	;* Initializing Routine 
	;********************************
init:			
	lda	06fh
	sta	num
	lhld	06dh			;Patch number inputed
	shld	number
	mvi	c,errmode		;Set ERROR Mode so no
	mvi	e,255			;BDOS error messages will
	call	bdos			;appear
	lxi	d,intro$mess
	call	print			;print 'PATCH '
	;
check$ver:
	mvi	c,version
	call	bdos			;get version
	mov	a,h			;'H'=0 -> CP/M
	cpi	0h			;'H'=1 -> MP/M
	jnz	wrngver			;jump if wrong o.s.
	mov	a,l
	cpi	030h
	jc	wrngver			;jump if wrong version
	ret
	;
	;********************************
	;* Check File Types 
	;* Check Default Routines
	;********************************
check$file:
	lda	fcb+1
	cpi	' '
	rnz
	lxi	d,file$prmt$mess
	call	print
	mvi	a,18
	sta	file$buff
	lxi	d,file$buff
	mvi	c,rstring
	call	bdos			;ask for file name
	lda	file$buff+1
	cpi	0
	jz	stop
parse:
	lda	file$buff+3
	cpi	':'
	jz	parse$drive
	lxi	h,file$buff+2
parse2:	
	mvi	b,8
	lxi	d,fcb+1
parse$name:
	mov	a,m
	cpi	'.'
	jz	parse3
	cpi	' '
	jz	parse$num
	cpi	'a'
	jc	parse$name2
	cpi	'{'
	jnc	parse$name2
	sui	020h
parse$name2:
	stax	d
	inx	h
	inx	d
	dcr	b
	jnz	parse$name
parse3:
	inx	h
	lxi	d,fcb+9
	mvi	b,3
parse$type:
	mov	a,m
	cpi	' '
	jz	parse$num2
	cpi	'a'
	jc	parse$type2
	cpi	'{'
	jnc	parse$type2
	sui	020h
parse$type2:
	stax	d
	inx	h
	inx	d
	dcr	b
	jz	parse$num
	jmp	parse$type
	;
parse$num:
	inx	h
parse$num2:
	mov	a,m
	cpi	' '
	rz
	sta	6dh
	inx	h
	mov	a,m
	sta	6eh
	lhld	6dh
	shld	number
	ret
	;
parse$drive:
	lda	file$buff+2
	cpi	'a'
	jc	parse$drive2
	cpi	'{'
	jnc	parse$drive2
	sui	020h
parse$drive2:
	sui	040h
	sta	fcb
	lxi	h,file$buff+4
	lda	fcb
	cpi	17
	jnc	bad$drive
	jmp	parse2
	;
check$spec:	
	lxi	h,spec
	mvi	b,3
check$spec2:
	mov	a,m
	cpi	' '
	rnz
	inx	h
	dcr	b
	jnz	check$spec2
	lxi	h,04f43h		;='CO'
	shld    spec
	mvi	a,'M'
	sta	spec+2
	mvi	a,1
	sta	type
	ret
	;
check$type:
	call	check$com		;check file type
	jmp	check$prl		;if .COM or .PRL 
	;				;1=>COM 2=>PRL 
check$com:				
	lda	type
	cpi	1
	lhld	spec
	mov	a,l
	cpi	'C'
	rnz
	mov	a,h
	cpi	'O'
	rnz
	lda	spec+2
	cpi	'M'
	rnz
	mvi	a,1
	sta	type
	ret
	;
check$prl:				;check if .PLR
	lhld	spec
	mov	a,l
	cpi	'P'
	jnz	no$com
	mov	a,h
	cpi	'R'
	jnz	no$com
	lda	spec+2
	cpi	'L'
	jnz	no$com
	mvi	a,2
	sta	type
	ret
	;
	;********************************
	;* Open File / Password Routine
	;********************************
open$fileº				
	mvi	c,openf			;also check if PASSWORD
	lxi	d,fcb
	call	bdos			;open the file
	sta	keepa
	mov	a,h
	cpi	7			;if PASSWORD status exists
	jz	get$passwd		;then H=7
	lda	keepa
	cpi	255			;if nofile then ACC.=FFh
	jz	no$file
	ret
	;
get$passwd:
	lda	tpasswd
	cpi	255			;check if user has already
	jz	wrng$pass		;tried PASSWORD
	call	space
	lxi	d,quest
	call	print			;print 'Password ?'
	call	input			;get the PASSWORD
	sta	len			;len = length of PASSWORD
	call	cap			;CAP the PASSWORD
	lxi	d,buf+2
	call	set$dma2		;tell where PASSWORD can
	mvi	a,255			;be found
	sta	tpasswd			;set Tried PASSWORD Flag
	jmp	open$file
	;
input:
	lxi	h,buf+2			;Buf+2 = buffer area for
	mvi	a,0			;PASSWORD
input2:					;ACC. is the counter
	push	h
	sta 	keepa			;Save the registers
	mvi	c,conin
	mvi	e,0fdh
	call	bdos			;get raw character
	cpi	ctrlx
	jz	input3			;restart input routine if ^X
	cpi	ctrlc		
	jz	stop			;stop if ^C
	cpi	cr
	jz	input4			;return if <CR>
	cpi	bak
	jz	back$space		;jump if DEL or BAK
	cpi	rub			;jump if RUB
	jz	back$space
	pop	h	
	mov	m,a			;move into memory the char.
	lda     keepa			;restore the counter
	inx	h			;inc. Memory location
	inr	a			;inc. Counter
	cpi	8			;check if 8 chars. read
	jnz	input2
	ret
	;
back$space:				;BACK SPACE (^H)
 	pop	h			;restore buffer pointer
	lda	keepa			;restore counter
	dcx	h			;set memory back 1
	dcr	a			;set counter back 1
	mvi	m,' '			;blank out the unwanted
	jmp	input2			;character
	;
input3:					;CTRL - X (^X)
    	pop	h			;restore buffer pointer
	call	space			;blank out the buffer
	jmp	input  			;for PASSWORD and start again
	;
input4:					;Restore STACK and return
	pop	h			;to GETPASSWD routine
	ret
	;
space:					;This routine blanks
	mvi	a,8			;out the buffer that
	lxi	h,buf+2			;contains the PASSWORD
space2:	;
	mvi	m,' '			;move into the pointer ' '
	inx	h			;do it 8 times
	dcr	a
	jnz	space2
	ret
	;
cap:					;This routine changes
	mvi	d,8			;the'PASSWORD' to upper-case
	lxi	h,buf+2
cap2:
	mov	a,m			
	cpi	'a'			;check if character is
	jc	skip			;between 'a' and 'z'
	cpi	'{'			;if so then change it
	jnc	skip			;to uppercase by subtracting
	sui	20h			;20 hex
	mov	m,a
skip:
	inx	h
	dcr	d
	jnz	cap2
	;
del$char:				;This routine deletes
	lda	len			;the character after
	adi	082h			;the PASSWORD because 
	sta	len			;BDOS function 10 adds
	lhld	len			;an extra character to the
	mvi	m,' '			;input
	ret
	;
	;********************************
	;* Serail Number Check Routines 
	;********************************
check$ser:				;This routine checks to see
	lda	type			;if the program to be PATCHED
	cpi	1			;is a CP/M 3 program
	jz	com$serial
	cpi	2
	jz	prl$serial
	jmp	no$com
	;
change$type:
	mvi	a,2			;This routine tells PATCH
change$type2:				;to treat the current COM
	sta	keepa			;file in the FCB as a PRL
	call	set$dma			;file so it will search the
	call	read$buff		;third record instead of
	lda	keepa			;the first
	dcr	a
	jnz	change$type2
	call	serial
	mvi	a,2
	sta	type
	ret
	;
com$serial:				;check for .COM serial #
	call	set$dma
	call	read$buff
	lda	buff
	cpi	0c9h			;check if a 'ret' statement
	jz	change$type		;if so treat it as a PRL file
	call	serial
	ret
	;
prl$serial:				;check for .PRl serial #
	mvi	a,3			;counter
prl$serial2:
	sta	keepa			;must read in the 3rd
	call	set$dma			;record of the .PRL file
	call	read$buff		;inorder to check for the
	lda	keepa			;serial number and retreive
	dcr	a			;the patch bit map
	jnz	prl$serial2
	call	serial
	ret
	;
serial:					;this routine checks a certain
	lxi 	h,buff+122		;memory block and searches 6 
	lxi	d,ser$table
	mvi	b,5			;certain bytes to see if it is
serial2:
	ldax	d
	cmp	m
	jnz	wrng$ser		;it checks the last 6 bytes
	inx	h
	inx	d
	dcr	b
	jnz	serial2
	ret
	;
	;********************************
	;* Branching Routines 
	;********************************
check$rw:				;this routine checks to
	lda	num			;see if the user wants
	cpi	' '			;to 'WRITE' or 'READ'
	jnz	not$num			;a patch
	lhld	number			
	mov	a,l			
	cpi	' '			
	jz	set$read			
	cpi	'1'
	jc	not$num
	cpi	':'
	jnc	not$num 
	mvi	a,2
	sta	rw			;set the'WRITE' flag
	ret
	;
set$read:				;set the 'READ' flag
	mvi	a,1
	sta	rw
	ret
	;
branch:					;branch if 'READ or WRITE'
	call	get$patchbits		;get bit map patch
	lda	rw			;into actual numbers
	cpi	1
	jz	read			;if '1' then 'READ'
	cpi	2
	jz	write			;if '2' then 'WRITE'
	jmp	stop
	;
	;********************************
	;* Multiply Routine
	;********************************
get$num:				;get inputed number
	lhld	number			;by user and transfer it
	mov	a,h			;into a non ASCII number
	cpi	' '
	jz	set$val
	sui	030h			;change into actual number
	sta	numtwo			;store it
	mov	a,l			
	sui	030h			;change into actual number
	;
multiply:
	mvi	b,9			;times to add number
	mov	e,a			
multiply2:
	add	e			;A=A+E
	dcr	b
	jnz	multiply2
	mov	e,a			;E=A*10
	lda	numtwo
	add	e
	sta	val
	cpi	33
	jnc	not$num
	ret
	;
set$val:				;if inputed patch number is
	mov     a,l			;only one character then
	sui	030h			;change it into a 'NO ASCII'
	sta	val			;number
	ret
	;
get$patchbits:				;get the 4 bytes in the bit map
	lhld	buff+118		;and save them for later
	shld	patch1
	lhld	buff+120
	shld	patch2
	ret
	;
	;********************************
	;* READ Routine
	;********************************
read:					;This routine checks the PATCH
	lxi	d,crpatc$mess		;Bit Map and displays any
	call	print			;current patches
	call	disp$file		;display the file
	lxi	d,col$cr$lf$sp
	call	print
	call	check$bits		;check if any patches exist
	mvi	e,0			;hex total counter
	lhld	patch2
	mov	a,h
	call	rot			;see if any 1-8
	lhld	patch2
	mov	a,l
	call	rot			;see if any 9-16	
	lhld	patch1
	mov	a,h
	call	rot			;see if any 17-24
	lhld	patch1
	mov	a,l
	call	rot			;see if any 25-32
	jmp	stop
	;
rot:
	mvi	d,8			;loop counter (1-8)
	sta	keepa
	mvi	a,0
rot2:
	lda	ct			;decimal counter
	adi	1
	daa				;add 1 (decimal)
	sta	ct
	inr	e
	mov	a,e
	sta	keepe			;E is the hex counter
	mov	a,d
	sta	keepd
	lda	keepa
	rrc				;rotate the byte
	sta	keepa
	cc	disp$ct			;Call routine if bit is on
	lda	keepd
	mov	d,a
	dcr	d			;check if loop is done
	rz
	jmp	rot2
	;
check$bits:
	lhld	patch2			;This routine checks the
	mov	a,h			;Patch Bit Map area to see
	cpi	0			;if any of the bytes have
	rnz				;a bit that is on
	mov	a,l
	cpi	0
	rnz
	lhld	patch1
	mov	a,h
	cpi	0
	rnz
	mov	a,l
	cpi	0
	jz	no$patches		;jump if no bits are on
	ret
	;
	;********************************
 	;* WRITE Routine
	;********************************
write:
	call	check$same		;check to see if the inputed
	lda	hpatch			;number by the user
	mov	e,a			;already exists for the file
	lda	val
	cmp	e
	cc	lesser
	jmp	greater
	;
check$same:
	lda	val			;This routine takes the inputed
	mov	b,a			;number and compares it
	mvi	e,0			;to an incrementing number
	lhld	patch2			;every time a bit is on
	mov	a,h
	call	rotate
	mov	a,l
	call	rotate
	lhld	patch1
	mov	a,h
	call	rotate
	mov	a,l
	call	rotate
	ret
	;
rotate:
	mvi	d,8			;counter to rotate eight times
rotate2:
	inr	e
	rrc	
	sta	keepa
	cc	compare			;if bit is on then check if
	lda	keepa			;the counter equals the inputed
	dcr	d			;number by the user
	jnz	rotate2
	ret
	;
compare:
	mov	a,e
	sta	hpatch			;store the current higest patch
	cmp	b			;found for later use
	jz	already
	ret
	;
greater:				;This routine displays the
	lxi	d,instl$mess		;user's inputed patch number
	call	disp$num		;display inputed number
	lxi	d,has$mess
	call	print
	lxi	d,betw$mess
	call	print
	call	disp$file		;display the file in the 'FCB'
	mvi	e,' '
	call	pbyte2
	mvi	e,'?'			;make it a question
	call	pbyte2
	mvi	e,' '
	call	pbyte2
	mvi	c,rstring
	mvi	a,4			;length of input
	sta	answer			;buffer
	lxi	d,answer		;pont 'DE' to buffer
	call	bdos			;wait for input
	lda	answer+2
	cpi	'Y'
	jz	greater2		;with the patch
	cpi	'y'
	jz 	greater2 
	jmp	quit$ptch
greater2:	
	call	plc$patch		;place the patch in the buffer
	call	writ$ptch		;write the patch into the file
	mvi	c,closef			
	lxi	d,fcb
	call	bdos			;close the file
	lxi	d,ok$mess		;tell user that the patch
	jmp 	pr$stop			;is finished
	;
lesser:
	lxi	d,less$mess		;This routine cautions the user
	call	disp$num		;that patches greater than
	lxi	d,less2$mess
	call	print
	call	disp$file		;display the file
	lxi	d,cr$lf
	call	print
	ret
	;
plc$patch:				;This routine checks to see
	lda	val			;what byte to alter 
	cpi	9
	jc	byte3			;inputed # is 1-8
	cpi	24
	jnc	byte0			;inputed # is 25-32
	cpi	17
	jc	byte2			;inputed # is 9-16
	jmp	byte1			;inputed # is 17-24
	;
byte0:					;This routine is done
	lda	val			;if the input was between
	sui	25			;25-32
	sta	bit$pos
	mvi	a,0
	jmp	table$load
	;
byte1:					;This routine is done
	lda	val			;if the input was between
	sui	17			;17-24
	sta	bit$pos
	mvi	a,1
	jmp	table$load
	;
byte2:					;This routine is done
	lda	val			;if the input was between
	sui	9			;9-16
	sta	bit$pos
	mvi	a,2
	jmp	table$load
	;
byte3:					;This routine is done
	lda	val			;if the input was between
	dcr	a			;1-8
	sta	bit$pos
	mvi	a,3
	jmp	table$load
	;
table$load:
	sta	byte$pos
	lxi	h,buff+118		;patch bit map
	mvi	b,0
	lda	byte$pos		;patch area
	mov	c,a
	dad	b			;'HL' = location to get byte
	shld	patch$pos		;place to get/put patch
	lxi	h,table
	mvi	b,0
	lda	bit$pos			;bit position (0-7)
	mov	c,a
	dad	b
	mov	b,m			;'HL' contains the byte to alter
	lhld	patch$pos
	mov	a,m
	ora	b			;turn the bit on
	mov	m,a			;save it
	ret
	;
already:				;this routine tells the user
	lxi	d,alread$mess2		;that the inputed patch number
	call	disp$num		;has already been installed
	lxi	d,alread$mess
	call	print
	call    disp$file
	jmp	stop
	;
writ$ptch:				;This routine branches depending
	lda	type			;what type of file type the file
	cpi	1			;has so it can write
	jz	com$patch		;correctly
	jmp	prl$patch
	;
com$patch:				;Tell that the record postion
	mvi	a,0			;is the first record
	jmp	write$ran
	;
prl$patch:
	mvi	a,2			;Tell that the record postion
	jmp	write$ran		;is the third record
	;
write$ran:				;This routine writes a record
	sta	fcb+33
	lxi	h,00
	shld	fcb+34
	mvi	c,writerf		;to the file in the FCB
	lxi	d,fcb			;at the record position found
	call	bdos			;at FCB+33
	cpi	0			;And the data to be written
	rz				;is found from the BUFF+128(80h)
	cpi	255
	jz	phys$err		;jump if physical error
	jmp	quit$ptch		
	;
	;********************************
	;* SUBROUTINES
	;********************************
set$dma:
	lxi	d,buff			;This routine set the DMA
set$dma2:
	mvi	c,dmaf
	call	bdos
	ret
	;
read$buff:				;this routine reads a block 
	mvi	c,readf			;from a file and places it into
	lxi	d,fcb			;memory
	call	bdos
	ret
	;
perror:					;print 'ERROR :'
	lxi	d,err$mess
	;
print:					;this routine prints a string
	mvi	c,pstring		;pointed by registers 'DE'
	call	bdos			;until a '$' is found
	ret
	;
pbyte:					;this routine prints the 
	mov	e,a			;'ASCII' character found
pbyte2:					;in register 'E'
	mvi	c,conout
	call	bdos
	ret
	;
displayit:				;this routine displays
	shld	keeph			;the invalid patch number
	sta	keepa			;that was inputed
	mov	a,m
	cpi	' '
	rz
	call	pbyte
	lhld	keeph
	lda	keepa
	inx	h
	dcr	a
	jnz	displayit
	rz
	;
displayit2:				;this routine displays
	shld	keeph			;the invalid filespec
	sta	keepa			;when an ERROR occurs
	mov	e,m
	call	pbyte2
	lhld	keeph
	lda	keepa
	inx	h
	dcr	a
	jnz	displayit2
	ret
	;
disp$num:
	call	print			;print string pointed earlier
	lhld	number
	mov	a,l
	call	pbyte
	lhld	number
	mov	a,h
	cpi	' '
	rz
	call	pbyte
	ret
	;
disp$file:				;this routine displays the file
	lxi	h,fcb+1			;name and spec. found in the FCB
	mvi	a,8			;(5dhex)
	call	displayit	
	mvi	e,'.'
	call	pbyte2
	lxi	h,fcb+9
	mvi	a,3
	call	displayit
	ret
	;
disp$drv:
	call	print
	lda	fcb			;get the current logged on drive
	cpi	0			;see if it is drive 'A'
	cz	chngdrv			;if so change the number
	adi	040h			;make the number ASCII
	call	pbyte			;display it
	mvi	e,':'
	call	pbyte2
	ret
	;
chngdrv:
	inr	a			;This routine adds 1 to the drive
	ret				;number so it can be displayed
	;
disp$ct:				;This routine is called
	lda	keepe			;every time a bit is found
	cpi	10			;on in the 'READ' routine
	jnc	disp$ct2		;and displays the current
	adi	030h			;patch according to what
	call	pbyte			;bit is on
	mvi	e,' '
	call	pbyte2
	ret
disp$ct2:
	lda	ct			;current decimal count
	ani	0fh			;get rid of the high nibble
	sta	lowdig			;store the lower 
	lda	ct
	ani	0f0h			;get rid of the low nibble
	rrc
	rrc
	rrc
	rrc				;rotate four times
	adi	030h			;make it an ASCII character
	cpi	'0'			;if character =0 then skip
	jz	disp$ct3		;the display routine
	call	pbyte			;print the byte
disp$ct3:
	lda	lowdig			;get the second digit
	adi	030h			;make it an ASCII character
	call	pbyte
	mvi	e,' '			;print a space
	call	pbyte2
	ret
	;
no$com:					;this routine tells the user
	lda	type			;that the file was not the
	cpi	3			;proper file type and displays
	rc	      			;the file type the user
	call	perror
	lxi	d,ncom$mess		;inputed
	call	print
	lxi	h,spec
	mvi	a,3
	call	displayit2
	lxi	d,pos$type
	jmp	pr$stop
	;
no$patches:				;this routine tells the user
	lxi	d,none$mess		;that no patches have been
	jmp 	pr$stop			;made for the file
	;
wrng$ser:				;this routine tells the user
	call	perror
	lxi	d,ser$mess		;that the serial # does not
	jmp 	pr$stop			;match
	;
wrngver:				;this routine informs the user
	call	perror			;that the wrong version of CP/M
	lxi	d,ver$mess		;is being used
	jmp	pr$stop
	;
wrng$pass:				;this routine tells the user
	call	perror
	lxi	d,pass$mess		;that the inputed password
	call	print			;was false
	lxi	h,buf+2
	mvi	a,8
	call	displayit2
	jmp	stop
	;
no$file:				;this routine tells the user
	call	perror
	lxi	d,nf$mess		;that the file was not found
	call	print			;and it gives the name of the
	lda	fcb+1			;file
	cpi	' '
	jz	stop
	call	disp$file
	lxi	d,drv$mess
	call	disp$drv
	jmp	stop
	;
not$num:				;this routine tells the user
	call	perror
	lxi	d,nonum$mess		;that the inputed number to patch
	call	print			;is illegal
	lxi	h,06dh
	mvi	a,5
	call	displayit
	lxi	d,pos$num
	jmp	pr$stop
	;
quit$ptch:				;This routine tells the user
	lxi	d,not$ptchd		;that the patch was not installed
	jmp	pr$stop
	;
disk$ro:				;This routine tells the user
	lxi	d,cr$lf			;that the disk is Read/Only 
	call	print
	call	perror
	lxi	d,drive$mess
	call	disp$drv
	lxi	d,ro$mess
	call	print
	jmp	stop
	;
file$ro:				;This routine tells the user that
	lxi	d,cr$lf			;the file he/she is trying to 
	call	print			;Patch is Read/Only
	call	perror
	call	disp$file
	lxi	d,ro$mess
	jmp	pr$stop
	;
bad$drive:
	call	perror
	lxi	d,baddrv$mess
	call	print
	lda	fcb
	adi	040h
	call	pbyte
	mvi	e,':'
	call	pbyte2
	jmp	stop
	;
phys$err:				;This routine tells the user
	mov	a,h			;that when performing the
	cpi	2			;write routine a permanent error
	jz	disk$ro			;was detected
	cpi	3
	jz	file$ro
	jmp	quit$ptch
	;
pr$stop:
	call	print
	jmp	stop
	;
stop:					;this routine ends the program
	lxi	d,cr$lf$lf
	call	print
	mvi	c,return
	call	bdos			;return to CCP
	;
	;********************************
	;* Program Calling Routine 
	;********************************
begin:
	lxi	sp,stack		;set stack to 'STACK'
	call	init			;initialize
	call	check$file		;check if file in the FCB
	call	check$spec		;check if .COM inplied
	call	check$type		;check file type
begin2:
	call	check$rw		;check if 'READ' or 'WRITE'
	call	get$num			;get/change inputed number
	call	open$file		;open the file
	call	check$ser		;check serial number
	call	branch			;branch too READ or WRITE
	call	stop			;stop program
	;
	;********************************
	;* CONSOLE MESSAGES TO USER 
	;********************************
	;* INTRO MESSAGE *
intro$mess:	db cr,lf,'CP/M 3 PATCH - Version 3.0$'
	;* ERROR MESSAGES *
err$mess:	db cr,lf,'ERROR: $'
ver$mess:	db 'PATCH requires CP/M 3$'
ncom$mess:	db 'Invalid file type: .$'
ser$mess:	db 'Serial number mismatch$'
nf$mess:	db 'No file: $'
pass$mess:	db 'False password: $'
nonum$mess:	db 'Invalid patch number: $'
drive$mess:	db 'Drive $'
ro$mess:	db ' is R/O$'
drv$mess:	db ' on $'
baddrv$mess:	db 'Illegal drive: $'
  	;* QUESTIONS *
file$prmt$mess: db cr,lf,'Enter File: $'
quest:		db cr,lf,'Enter Password: $'
instl$mess:	db cr,lf,'Do you want to indicate that patch $'
	;* STATUS MESSAGES *
pos$type:	db cr,lf,'Valid file types: COM or PRL$'
pos$num:	db cr,lf,'Valid patch numbers: 1-32$'
crpatc$mess:	db cr,lf,'Current patches for $'
less$mess:	db cr,lf,'WARNING: Patches greater than $'
less2$mess:	db cr,lf,'  exist for $'
has$mess:	db cr,lf,'  has been installed$'
alread$mess:	db ' already exists for $'
alread$mess2:	db cr,lf,'Patch $'
none$mess:	db 'None$'
betw$mess:	db ' for $'
ok$mess:	db cr,lf,lf,'Patch installed$'
not$ptchd:	db cr,lf,lf,'Patch not installed$'
cr$lf:		db cr,lf,'$'
cr$lf$lf:	db cr,lf,lf,'$'
col$cr$lf$sp: 	db ':',cr,lf,'  $'
	;
	;********************************
	;* VARIABLE AND DATA STORAGE AREA
	;********************************
keepa:		db 0			;Storage for 'ACC'
keepe:		db 0			;Storage for 'E'
keepd:		db 0			;Storage for 'D'
keeph:		dw 0			;Storage for 'HL'
file$buff:	db 32,32,32,32,32,32	;File Buffer for default
		db 32,32,32,32,32,32
		db 32,32,32,32,32,32,32,32
tpasswd:	db 0			;'Tried Password' Flag
len:		db 0			;Length of password input
type:		db 255			;File type (.COM,. PRL or .SPR)
rw:		db 0			;'READ/WRITE' flag
hpatch:		db 0			;highest patch
number:		dw 00			;Inputed (ASCII) number
patch1:		dw 00			;'PATCH' bit map storage area
patch2:		dw 00			;'PATCH' bit map storage area
num:		db 0			;Third number of input by user
ct:		db 0			;Actual 'PATCHES' after rotate (dec)
numtwo:		db 0			;Input # -30h
val:		db 0			;Actual input value after multiply
answer:		ds 3			;Storage for input to question
lowdig:		ds 1			;Storage for lower digit to display
bit$pos:	db 0			;Position of bit (0-7)
byte$pos:	db 0			;Postion of byte (0-3)
patch$pos:	dw 0			;Holds address of patch byte
com$table:	db 'COM'		;These tables are used to 
prl$table:	db 'PRL'		;  compare file types in         
ser$table:	db '654321'		;This table is for serial checker
table:		db 1,2,4,8,16,32,64,128	;This table is for bit manipulation
		ds 16			;Stack area
stack:
		ds 2
buff:		ds 128			;Buffer (holds one record;128 bytes)
