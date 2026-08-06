	title	'console command processor (CCP), ver 2.0'
;	assembly language version of the CP/M console command processor
;
;	version 2.2 February, 1980
;
;	Copyright (c) 1976, 1977, 1978, 1979, 1980
;	Digital Research
;	Box 579, Pacific Grove,
;	California, 93950
;
false	equ	0000h
true	equ	not false
testing	equ	false	;true if debugging
;
;
	if	testing
	org	3400h
bdosl	equ	$+800h		;bdos location
	else
	org	000h
bdosl	equ	$+800h		;bdos location
	endif
tran	equ	100h
tranm	equ	$
ccploc	equ	$
;
;	********************************************************
;	*	Base of CCP contains the following code/data   *
;	*	ccp:	jmp ccpstart	(start with command)   *
;	*		jmp ccpclear    (start, clear command) *
;	*	ccp+6	127		(max command length)   *
;	*	ccp+7	comlen		(command length = 00)  *
;	*	ccp+8	' ... '		(16 blanks)	       *
;	********************************************************
;	* Normal entry is at ccp, where the command line given *
;	* at ccp+8 is executed automatically (normally a null  *
;	* command with comlen = 00).  An initializing program  *
;	* can be automatically loaded by storing the command   *
;	* at ccp+8, with the command length at ccp+7.  In this *
;	* case, the ccp executes the command before prompting  *
;	* the console for input.  Note that the command is exe-*
;	* cuted on both warm and cold starts.  When the command*
;	* line is initialized, a jump to "jmp ccpclear" dis-   *
;	* ables the automatic command execution.               *
;	********************************************************
;
	jmp	ccpstart	;start ccp with possible initial command
	jmp	ccpclear	;clear the command buffer
maxlen:	db	127	;max buffer length
comlen:	db	0	;command length (filled in by dos)
;	(command executed initially if comlen non zero)
combuf:
	db	'        '	;8 character fill
	db	'        '	;8 character fill
	db	'COPYRIGHT (C) 1979, DIGITAL RESEARCH  '; 38
	ds	128-($-combuf)
;	total buffer length is 128 characters
comaddr:dw	combuf	;address of next to char to scan
staddr:	ds	2	;starting address of current fillfcb request
;
diska	equ	0004h	;disk address for current disk
bdos	equ	0005h	;primary bdos entry point
buff	equ	0080h	;default buffer
fcb	equ	005ch	;default file control block
;
rcharf	equ	1	;read character function
pcharf	equ	2	;print character function
pbuff	equ	9	;print buffer function
rbuff	equ	10	;read buffer function
breakf	equ	11	;break key function
liftf	equ	12	;lift head function (no operation)
initf	equ	13	;initialize bdos function
self	equ	14	;select disk function
openf	equ	15	;open file function
closef	equ	16	;close file function
searf	equ	17	;search for file function
searnf	equ	18	;search for next file function
delf	equ	19	;delete file function
dreadf	equ	20	;disk read function
dwritf	equ	21	;disk write function
makef	equ	22	;file make function
renf	equ	23	;rename file function
logf	equ	24	;return login vector
cself	equ	25	;return currently selected drive number
dmaf	equ	26	;set dma address
userf	equ	32	;set user number
;
;	special fcb flags
rofile	equ	9	;read only file
sysfile	equ	10	;system file flag
;
;	special characters
cr	equ	13	;carriage return
lf	equ	10	;line feed
la	equ	5fh	;left arrow
eofile	equ	1ah	;end of file
;
;	utility procedures
printchar:
	mov e,a! mvi c,pcharf! jmp bdos
;
printbc:
	;print character, but save b,c registers
	push b! call printchar! pop b! ret
;
crlf:
	mvi a,cr! call printbc
	mvi a,lf! jmp  printbc
;
blank:
	mvi a,' '! jmp printbc
;
print:	;print string starting at b,c until next 00 entry
	push b! call crlf! pop h ;now print the string
prin0:	mov a,m! ora a! rz ;stop on 00
		inx h! push h ;ready for next
		call printchar! pop h ;character printed
		jmp prin0 ;for another character
;
initialize:
	mvi c,initf! jmp bdos
;
select:
	mov e,a! mvi c,self! jmp bdos
;
bdos$inr:
	call bdos! sta dcnt! inr a! ret
;
open:	;open the file given by d,e
	mvi c,openf! jmp bdos$inr
;
openc:	;open comfcb
	xra a! sta comrec ;clear next record to read
	lxi d,comfcb! jmp open
;
close:	;close the file given by d,e
	mvi c,closef! jmp bdos$inr
;
search:	;search for the file given by d,e
	mvi c,searf! jmp bdos$inr
;
searchn:
	;search for the next occurrence of the file given by d,e
	mvi c,searnf! jmp bdos$inr
;
searchcom:
	;search for comfcb file
	lxi d,comfcb! jmp search
;
delete:	;delete the file given by d,e
	mvi c,delf! jmp bdos
;
bdos$cond:
	call bdos! ora a! ret
;
diskread:
	;read the next record from the file given by d,e
	mvi c,dreadf! jmp bdos$cond
;
diskreadc:
	;read the comfcb file
	lxi d,comfcb! jmp diskread
;
diskwrite:
	;write the next record to the file given by d,e
	mvi c,dwritf! jmp bdos$cond
;
make:	;create the file given by d,e
	mvi c,makef! jmp bdos$inr
;
renam:	;rename the file given by d,e
	mvi c,renf! jmp bdos
;
getuser:
	;return current user code in a
	mvi e,0ffh ;drop through to setuser
;
setuser:
        mvi c,userf! jmp bdos ;sets user number
;
saveuser:
	;save user#/disk# before possible ^c or transient
	call getuser ;code to a
	add a! add a! add a! add a ;rot left
	lxi h,cdisk! ora m ;4b=user, 4b=disk
	sta diska ;stored away in memory for later
	ret
;
setdiska:
	lda cdisk! sta diska ;user/disk
	ret
;
translate:
	;translate character in register A to upper case
	cpi 61h! rc ;return if below lower case a
	cpi 7bh! rnc ;return if above lower case z
	ani 5fh! ret ;translated to upper case
;
readcom:
	;read the next command into the command buffer
	;check for submit file
	lda submit! ora a! jz nosub
		;scanning a submit file
		;change drives to open and read the file
		lda cdisk! ora a! mvi a,0! cnz select
		;have to open again in case xsub present
                lxi d,subfcb! call open! jz nosub ;skip if no sub
		lda subrc! dcr a ;read last record(s) first
		sta subcr ;current record to read
		lxi d,subfcb! call diskread ;end of file if last record
		jnz nosub
			;disk read is ok, transfer to combuf
			lxi d,comlen! lxi h,buff! mvi b,128! call move0
			;line is transferred, close the file with a
			;deleted record
			lxi h,submod! mvi m,0 ;clear fwflag
			inx h! dcr m ;one less record
			lxi d,subfcb! call close! jz nosub
			;close went ok, return to original drive
			lda cdisk! ora a! cnz select
			;print to the 00
			lxi h,combuf! call prin0
			call break$key! jz noread
			call del$sub! jmp ccp ;break key depressed
			;
	nosub:	;no submit file! call del$sub
	;translate to upper case, store zero at end
	call saveuser ;user # save in case control c
	mvi c,rbuff! lxi d,maxlen! call bdos
	call setdiska ;no control c, so restore diska
	noread:	;enter here from submit file
	;set the last character to zero for later scans
	lxi h,comlen! mov b,m ;length is in b
	readcom0: inx h! mov a,b! ora a ;end of scan?
		jz readcom1! mov a,m ;get character and translate
		call translate! mov m,a! dcr b! jmp readcom0
		;
	readcom1: ;end of scan, h,l address end of command
		mov m,a ;store a zero
		lxi h,combuf! shld comaddr ;ready to scan to zero
	ret
;
break$key:
	;check for a character ready at the console
	mvi c,breakf! call bdos
	ora a! rz
	mvi c,rcharf! call bdos ;character cleared
	ora a! ret
;
cselect:
	;get the currently selected drive number to reg-A
	mvi c,cself! jmp bdos
;
setdmabuff:
	;set default buffer dma address
	lxi d,buff ;(drop through)
;
setdma:
	;set dma address to d,e
	mvi c,dmaf! jmp bdos
;
del$sub:
	;delete the submit file, and set submit flag to false
	lxi h,submit! mov a,m! ora a! rz ;return if no sub file
	mvi m,0 ;submit flag is set to false
	xra a! call select ;on drive a to erase file
	lxi d,subfcb! call delete
	lda cdisk! jmp select ;back to original drive
;
serialize:
	;check serialization
	lxi d,serial! lxi h,bdosl! mvi b,6 ;check six bytes
	ser0:	ldax d! cmp m! jnz badserial
		inx d! inx h! dcr b! jnz ser0
		ret ;serial number is ok
;
comerr:
	;error in command string starting at position
	;'staddr' and ending with first delimiter
	call crlf ;space to next line
	lhld staddr ;h,l address first to print
	comerr0: ;print characters until blank or zero
		mov a,m! cpi ' '! jz comerr1; not blank
		ora a! jz comerr1; not zero, so print it
		push h! call printchar! pop h! inx h
		jmp comerr0; for another character
	comerr1: ;print question mark,and delete sub file
		mvi a,'?'! call printchar
		call crlf! call del$sub
		jmp ccp ;restart with next command
;
; fcb scan and fill subroutine (entry is at fillfcb below)
	;fill the comfcb, indexed by A (0 or 16)
	;subroutines
	delim:	;look for a delimiter
		ldax d! ora a! rz ;not the last element
		cpi ' '! jc comerr ;non graphic
		rz ;treat blank as delimiter
		cpi '='! rz
		cpi la!  rz ;left arrow
		cpi '.'! rz
		cpi ':'! rz
		cpi ';'! rz
		cpi '<'! rz
		cpi '>'! rz
		ret	;delimiter not found
;
	deblank: ;deblank the input line
		ldax d! ora a! rz ;treat end of line as blank
		cpi ' '! rnz! inx d! jmp deblank
;
	addh: ;add a to h,l
		add l! mov l,a! rnc
		inr h! ret
		;
fillfcb0:
	;equivalent to fillfcb(0)
	mvi a,0
;
fillfcb:
	lxi h,comfcb! call addh! push h! push h ;fcb rescanned at end
	xra a! sta sdisk ;clear selected disk (in case A:...)
	lhld comaddr! xchg ;command address in d,e
	call deblank ;to first non-blank character
	xchg! shld staddr ;in case of errors
	xchg! pop h ;d,e has command, h,l has fcb address
	;look for preceding file name A: B: ...
	ldax d! ora a! jz setcur0 ;use current disk if empty command
	sbi 'A'-1! mov b,a ;disk name held in b if : follows
	inx d! ldax d! cpi ':'! jz setdsk ;set disk name if :
	;
	setcur: ;set current disk
		dcx d ;back to first character of command
	setcur0:
		lda cdisk! mov m,a! jmp setname
	;
	setdsk: ;set disk to name in register b
		mov a,b! sta sdisk ;mark as disk selected
		mov m,b! inx d ;past the :
	;
	setname: ;set the file name field
		mvi b,8 ;file name length (max)
		setnam0: call delim! jz padname ;not a delimiter
			inx h! cpi '*'! jnz setnam1 ;must be ?'s
			mvi m,'?'! jmp setnam2 ;to dec count
		;
		setnam1: mov m,a ;store character to fcb! inx d
		setnam2: dcr b ;count down length! jnz setnam0
		;
	;end of name, truncate remainder
	trname: call delim! jz setty ;set type field if delimiter
		inx d! jmp trname
		;
	padname: inx h! mvi m,' '! dcr b! jnz padname
		;
	setty: ;set the type field
		mvi b,3! cpi '.'! jnz padty ;skip the type field if no .
		inx d ;past the ., to the file type field
		setty0: ;set the field from the command buffer
			call delim! jz padty! inx h! cpi '*'! jnz setty1
			mvi m,'?' ;since * specified! jmp setty2
			;
		setty1: ;not a *, so copy to type field
			mov m,a! inx d
		setty2: ;decrement count and go again
			dcr b! jnz setty0
			;
		;end of type field, truncate
	trtyp: ;truncate type field
		call delim! jz efill! inx d! jmp trtyp
		;
		padty:	;pad the type field with blanks
			inx h! mvi m,' '! dcr b! jnz padty
		;
	efill: ;end of the filename/filetype fill, save command address
		;fill the remaining fields for the fcb
		mvi b,3
		efill0: inx h! mvi m,0! dcr b! jnz efill0
		xchg! shld comaddr ;set new starting point
		;
		;recover the start address of the fcb and count ?'s
		pop h! lxi b,11 ;b=0, c=8+3
		scnq: inx h! mov a,m! cpi '?'! jnz scnq0
		;? found, count it in b! inr b
		scnq0: dcr c! jnz scnq
		;
		;number of ?'s in c, move to a and return with flags set
		mov a,b! ora a! ret
;
intvec:
	;intrinsic function names (all are four characters)
	db	'DIR '
	db	'ERA '
	db	'TYPE'
	db	'SAVE'
	db	'REN '
        db      'USER'
	intlen equ ($-intvec)/4 ;intrinsic function length
	serial: db 0,0,0,0,0,0
;
;
intrinsic:
	;look for intrinsic functions (comfcb has been filled)
	lxi h,intvec! mvi c,0 ;c counts intrinsics as scanned
	intrin0: mov a,c! cpi intlen ;done with scan?! rnc
		;no, more to scan
		lxi d,comfcb+1 ;beginning of name
		mvi b,4 ;length of match is in b
		intrin1: ldax d! cmp m ;match?
			jnz intrin2 ;skip if no match
			inx d! inx h! dcr b
			jnz intrin1 ;loop while matching
		;
		;complete match on name, check for blank in fcb
		ldax d! cpi ' '! jnz intrin3 ;otherwise matched
		mov a,c! ret ;with intrinsic number in a
		;
		intrin2: ;mismatch, move to end of intrinsic
			inx h! dcr b! jnz intrin2
		;
		intrin3: ;try next intrinsic
			inr c ;to next intrinsic number
			jmp intrin0 ;for another round
;
ccpclear:
	;clear the command buffer
	xra	a
	sta	comlen
	;drop through to start ccp
ccpstart:
	;enter here from boot loader
	lxi sp,stack! push b ;save initial disk number
        ;(high order 4bits=user code, low 4bits=disk#)
	mov a,c! rar! rar! rar! rar! ani 0fh ;user code
	mov e,a! call setuser ;user code selected
	;initialize for this user, get $ flag
        call initialize ;0ffh in accum if $ file present
        sta submit ;submit flag set if $ file present
        pop b ;recall user code and disk number
	mov a,c! ani 0fh ;disk number in accumulator
        sta cdisk ;clears user code nibble
	call select ;proper disk is selected, now check sub files
	;check for initial command
	lda comlen! ora a! jnz ccp0	;assume typed already
;
ccp:
	;enter here on each command or error condition
	lxi sp,stack
	call crlf ;print d> prompt, where d is disk name
	call cselect ;get current disk number
	adi 'A'! call printchar
	mvi a,'>'! call printchar
	call readcom ;command buffer filled
ccp0:	;(enter here from initialization with command full)
	lxi d,buff! call setdma ;default dma address at buff
	call cselect! sta cdisk ;current disk number saved
	call fillfcb0 ;command fcb filled
	cnz comerr ;the name cannot be an ambiguous reference
	lda sdisk! ora a! jnz userfunc
		;check for an intrinsic function
		call intrinsic
		lxi h,jmptab ;index is in the accumulator
		mov e,a! mvi d,0! dad d! dad d ;index in d,e
		mov a,m! inx h! mov h,m! mov l,a! pchl
		;pc changes to the proper intrinsic or user function
		jmptab:
			dw	direct	;directory search
			dw	erase	;file erase
			dw	type	;type file
			dw	save	;save memory image
			dw	rename	;file rename
			dw	user	;user number
			dw	userfunc;user-defined function
		badserial:
			lxi h,di or (hlt shl 8)
			shld ccploc! lxi h,ccploc! pchl
			;
;
	;utility subroutines for intrinsic handlers
	readerr:
		;print the read error message
		lxi b,rdmsg! jmp print
		rdmsg: db 'READ ERROR',0
	;
	nofile:
		;print no file message
		lxi b,nofmsg! jmp print
		nofmsg: db 'NO FILE',0
	;
	getnumber: ;read a number from the command line
		call fillfcb0 ;should be number
		lda sdisk! ora a! jnz comerr ;cannot be prefixed
		;convert the byte value in comfcb to binary
		lxi h,comfcb+1! lxi b,11 ;(b=0, c=11)
		;value accumulated in b, c counts name length to zero
		conv0:	mov a,m! cpi ' '! jz conv1
			;more to scan, convert char to binary and add
			inx h! sui '0'! cpi 10! jnc comerr ;valid?
			mov d,a ;save value! mov a,b ;mult by 10
			ani 1110$0000b! jnz comerr
			mov a,b ;recover value
			rlc! rlc! rlc ;*8
			add b! jc comerr
			add b! jc comerr ;*8+*2 = *10
			add d! jc comerr ;+digit
			mov b,a! dcr c! jnz conv0 ;for another digit
			ret
		conv1:	;end of digits, check for all blanks
			mov a,m! cpi ' '! jnz comerr ;blanks?
			inx h! dcr c! jnz conv1
			mov a,b ;recover value! ret
		;
	movename:
		;move 3 characters from h,l to d,e addresses
		mvi b,3
		move0: mov a,m! stax d! inx h! inx d
			dcr b! jnz move0
		ret
	;
	addhcf:	;buff + a + c to h,l followed by fetch
		lxi h,buff! add c! call addh! mov a,m! ret
	;
	setdisk:
		;change disks for this command, if requested
		xra a! sta comfcb ;clear disk name from fcb
		lda sdisk! ora a! rz ;no action if not specified
		dcr a! lxi h,cdisk! cmp m! rz ;already selected
		jmp select
	;
	resetdisk:
		;return to original disk after command
		lda sdisk! ora a! rz ;no action if not selected
		dcr a! lxi h,cdisk! cmp m! rz ;same disk
		lda cdisk! jmp select
;
	;individual intrinsics follow
direct:
	;directory search
	call fillfcb0 ;comfcb gets file name
	call setdisk ;change disk drives if requested
	lxi h,comfcb+1! mov a,m ;may be empty request
	cpi ' '! jnz dir1 ;skip fill of ??? if not blank
		;set comfcb to all ??? for current disk
		mvi b,11 ;length of fill ????????.???
		dir0: mvi m,'?'! inx h! dcr b! jnz dir0
	;not a blank request, must be in comfcb
	dir1:	mvi e,0! push d ;E counts directory entries
		call searchcom ;first one has been found
		cz nofile ;not found message
	dir2:	jz endir
		;found, but may be system file
		lda dcnt ;get the location of the element
		rrc! rrc! rrc! ani 110$0000b! mov c,a
		;c contains base index into buff for dir entry
		mvi a,sysfile! call addhcf ;value to A
		ral! jc dir6 ;skip if system file
		;c holds index into buffer
		;another fcb found, new line?
		pop d! mov a,e! inr e! push d
		;e=0,1,2,3,...new line if mod 4 = 0
		ani 11b! push psw ;and save the test
			jnz dirhdr0 ;header on current line
			call crlf
			push b! call cselect! pop b
			;current disk in A
			adi 'A'! call printbc
			mvi a,':'! call printbc
			jmp dirhdr1 ;skip current line hdr
		dirhdr0:call blank ;after last one
			mvi a,':'! call printbc
		dirhdr1:
			call blank
		;compute position of name in buffer
		mvi b,1 ;start with first character of name
		dir3:	mov a,b! call addhcf ;buff+a+c fetched
			ani 7fh ;mask flags
			;may delete trailing blanks
			cpi ' '! jnz dir4 ;check for blank type
			pop psw! push psw ;may be 3rd item
			cpi 3! jnz dirb ;place blank at end if not
			mvi a,9! call addhcf ;first char of type
			ani 7fh! cpi ' '! jz dir5
			;not a blank in the file type field
		dirb:	mvi a,' ' ;restore trailing filename chr
		dir4:
			call printbc ;char printed
			inr b! mov a,b! cpi 12! jnc dir5
			;check for break between names
			cpi 9! jnz dir3 ;for another char
			;print a blank between names
			call blank! jmp dir3
		;
	dir5:	;end of current entry
		pop psw ;discard the directory counter (mod 4)
	dir6:	call break$key ;check for interrupt at keyboard
		jnz endir ;abort directory search
		call searchn! jmp dir2 ;for another entry
	endir:	;end of directory scan
		pop d ;discard directory counter
		jmp retcom
;
;
erase:
	call fillfcb0 ;cannot be all ???'s
	cpi 11
	jnz erasefile
		;erasing all of the disk
		lxi b,ermsg! call print!
		call readcom
		lxi h,comlen! dcr m! jnz ccp ;bad input
		inx h! mov a,m! cpi 'Y'! jnz ccp
		;ok, erase the entire diskette
		inx h! shld comaddr ;otherwise error at retcom
	erasefile:
		call setdisk
		lxi d,comfcb! call delete
		inr a ;255 returned if not found
		cz nofile ;no file message if so
		jmp retcom
;
	ermsg:	db	'ALL (Y/N)?',0
;
type:
	call fillfcb0! jnz comerr ;don't allow ?'s in file name
	call setdisk! call openc ;open the file
	jz typerr ;zero flag indicates not found
		;file opened, read 'til eof
		call crlf! lxi h,bptr! mvi m,255 ;read first buffer
		type0:	;loop on bptr
			lxi h,bptr! mov a,m! cpi 128 ;end buffer
			jc type1! push h ;carry if 0,1,...,127
			;read another buffer full
			call diskreadc! pop h ;recover address of bptr
			jnz typeof ;hard end of file
			xra a! mov m,a ;bptr = 0
		type1:	;read character at bptr and print
			inr m ;bptr = bptr + 1
			lxi h,buff! call addh ;h,l addresses char
			mov a,m! cpi eofile! jz retcom
			call printchar
			call break$key! jnz retcom ;abort if break
			jmp type0 ;for another character
		;
		typeof:	;end of file, check for errors
			dcr a! jz retcom
			call readerr
		typerr:	call resetdisk! jmp comerr
;
save:
		call getnumber; value to register a
		push psw ;save it for later
		;
		;should be followed by a file to save the memory image
		call fillfcb0
		jnz comerr ;cannot be ambiguous
		call setdisk ;may be a disk change
		lxi d,comfcb! push d! call delete ;existing file removed
		pop d! call make ;create a new file on disk
		jz saverr ;no directory space
		xra a! sta comrec; clear next record field
		pop psw ;#pages to write is in a, change to #sectors
		mov l,a! mvi h,0! dad h! 
		lxi d,tran ;h,l is sector count, d,e is load address
	save0:	;check for sector count zero
		mov a,h! ora l! jz save1 ;may be completed
		dcx h ;sector count = sector count - 1
		push h ;save it for next time around
		lxi h,128! dad d! push h ;next dma address saved
		call setdma ;current dma address set
		lxi d,comfcb! call diskwrite
		pop d! pop h ;dma address, sector count
		jnz saverr ;may be disk full case
		jmp save0 ;for another sector
		;
	save1:	;end of dump, close the file
		lxi d,comfcb! call close
		inr a; 255 becomes 00 if error
		jnz retsave ;for another command
	saverr:	;must be full or read only disk
		lxi b,fullmsg! call print
	retsave:
		;reset dma buffer
		call setdmabuff
		jmp retcom
		fullmsg: db 'NO SPACE',0
		;
;
rename:
	;rename a file on a specific disk
	call fillfcb0! jnz comerr ;must be unambiguous
	lda sdisk! push psw ;save for later compare
	call setdisk ;disk selected
	call searchcom ;is new name already there?
	jnz renerr3
		;file doesn't exist, move to second half of fcb
		lxi h,comfcb! lxi d,comfcb+16! mvi b,16! call move0
		;check for = or left arrow
		lhld comaddr! xchg! call deblank
		cpi '='! jz ren1 ;ok if =
		cpi la! jnz renerr2
	ren1:	xchg! inx h! shld comaddr ;past delimiter
		;proper delimiter found
		call fillfcb0! jnz renerr2
		;check for drive conflict
			pop psw! mov b,a ;previous drive number
			lxi h,sdisk! mov a,m! ora a! jz ren2
			;drive name was specified.  same one?
			cmp b! mov m,b! jnz renerr2
	ren2:	mov m,b ;store the name in case drives switched
		xra a! sta comfcb! call searchcom ;is old file there?
		jz renerr1
		;
		;everything is ok, rename the file
		lxi d,comfcb! call renam
		jmp retcom
		;
	renerr1:; no file on disk
		call nofile! jmp retcom
	renerr2:; ambigous reference/name conflict
		call resetdisk! jmp comerr
	renerr3:; file already exists
		lxi b,renmsg! call print! jmp retcom
		renmsg: db 'FILE EXISTS',0
;
user:
	;set user number
	call getnumber; leaves the value in the accumulator
	cpi 16! jnc comerr; must be between 0 and 15
	mov e,a ;save for setuser call
	lda comfcb+1! cpi ' '! jz comerr
	call setuser ;new user number set
	jmp endcom
;
userfunc:
	call serialize ;check serialization
	;load user function and set up for execution
	lda comfcb+1! cpi ' '! jnz user0
		;no file name, but may be disk switch
		lda sdisk! ora a! jz endcom ;no disk name if 0
		dcr a! sta cdisk! call setdiska ;set user/disk
		call select! jmp endcom
	user0:	;file name is present
		lxi d,comfcb+9! ldax d! cpi ' '! jnz comerr ;type ' '
		push d! call setdisk! pop d! lxi h,comtype ;.com
		call movename ;file type is set to .com
		call openc! jz userer
		;file opened properly, read it into memory
		lxi h,tran ;transient program base
		load0:	push h ;save dma address
			xchg! call setdma
			lxi d,comfcb! call diskread! jnz load1
			;sector loaded, set new dma address and compare
			pop h! lxi d,128! dad d
			lxi d,tranm ;has the load overflowed?
			mov a,l! sub e! mov a,h! sbb d! jnc loaderr
			jmp load0 ;for another sector
			;
		load1:	pop h! dcr a! jnz loaderr ;end file is 1
			call resetdisk ;back to original disk
			call fillfcb0! lxi h,sdisk! push h
			mov a,m! sta comfcb ;drive number set
			mvi a,16! call fillfcb ;move entire fcb to memory
			pop h! mov a,m! sta comfcb+16
			xra a! sta comrec ;record number set to zero
			lxi d,fcb! lxi h,comfcb! mvi b,33! call move0
			;move command line to buff
			lxi h,combuf
		bmove0:	mov a,m! ora a! jz bmove1! cpi ' '! jz bmove1
			inx h! jmp bmove0 ;for another scan
			;first blank position found
		bmove1:	mvi b,0! lxi d,buff+1! ;ready for the move
		bmove2:	mov a,m! stax d! ora a! jz bmove3
			;more to move
			inr b! inx h! inx d! jmp bmove2
		bmove3:	;b has character count
			mov a,b! sta buff
			call crlf
			;now go to the loaded program
			call setdmabuff ;default dma
			call saveuser ;user code saved
			;low memory diska contains user code
			call tran ;gone to the loaded program
			lxi sp,stack ;may come back here
			call setdiska! call select
			jmp ccp
		;
		userer:	;arrive here on command error
			call resetdisk! jmp comerr
			;
		loaderr:;cannot load the program
			lxi b,loadmsg! call print
			jmp retcom
			loadmsg: db 'BAD LOAD',0
		comtype:	db 'COM' ;for com files
;
;
retcom:	;reset disk before end of command check
	call resetdisk
;
endcom:	;end of intrinsic command
	call fillfcb0 ;to check for garbage at end of line
	lda comfcb+1! sui ' '! lxi h,sdisk! ora m
	;0 in accumulator if no disk selected, and blank fcb
	jnz comerr
	jmp ccp
;
;
;
;	data areas
	ds	16	;8 level stack
stack:
;
;	'submit' file control block
submit:	db	0	;00 if no submit file, ff if submitting
subfcb:	db	0,'$$$     '	;file name is $$$
	db	'SUB',0,0	;file type is sub
submod:	db	0	;module number
subrc:	ds	1	;record count filed
	ds	16	;disk map
subcr:	ds	1	;current record to read
;
;	command file control block
comfcb:	ds	32	;fields filled in later
comrec:	ds	1	;current record to read/write
dcnt:	ds	1	;disk directory count (used for error codes)
cdisk:	ds	1	;current disk
sdisk:	ds	1	;selected disk for current operation
			;none=0, a=1, b=2 ...
bptr:	ds	1	;buffer pointer
	end	ccploc
