title	'CP/M 3 - Console Command Processor - November 1982'
;	version 3.00  Nov 30 1982 - Doug Huskey


;  Copyright (C) 1982
;  Digital Research
;  P.O. Box 579
;  Pacific Grove, CA 93950

;  Revised: (date/name of person modifying this source)

;	****************************************************
;	*****  The following equates must be set to 100H ***
;	*****  + the addresses specified in LOADER.PRN   ***
;	*****                                            ***
equ1	equ	rsxstart  ;does this adr match loader's?
equ2	equ	fixchain  ;does this adr match loader's?
equ3	equ	fixchain1 ;does this adr match loader's?
equ4	equ	fixchain2 ;does this adr match loader's?
equ5	equ	rsx$chain ;does this adr match loader's?
equ6	equ	reloc     ;does this adr match loader's?
equ7	equ	calcdest  ;does this adr match loader's?
equ8	equ	scbaddr   ;does this adr match loader's?
equ9	equ	banked    ;does this adr match loader's?
equ10	equ	rsxend    ;does this adr match loader's?
equ11	equ	ccporg    ;does this adr match loader's?
equ12	equ	ccpend    ;This should be 0D80h
	rsxstart	equ	0100h
	fixchain	equ	01D0h
	fixchain1	equ	01EBh
	fixchain2	equ	01F0h
	rsx$chain	equ	0200h
	reloc		equ	02CAh
	calcdest	equ	030Fh
	scbaddr		equ	038Dh
	banked		equ	038Fh
	rsxend		equ	0394h
	ccporg		equ	041Ah
;	****************************************************
;	NOTE: THE ABOVE EQUATES MUST BE CORRECTED IF NECESSARY
;	AND THE JUMP TO START AT THE BEGINNING OF THE LOADER
;	MUST BE SET TO THE ORIGIN ADDRESS BELOW:

	org	ccporg		;LOADER is at 100H to 3??H

;	(BE SURE THAT THIS LEAVES ENOUGH ROOM FOR THE LOADER BIT MAP)


;  Conditional Assembly toggles:

true	equ	0ffffh
false	equ	0h
newdir	equ	true
newera	equ	true		;confirm any ambiguous file name
dayfile	equ	true		
prompts	equ	false
func152	equ	true
multi	equ	true		;multiple command lines
				;also shares code with loader (100-2??h)
;
;************************************************************************
;
;	GLOBAL EQUATES
;
;************************************************************************
;
;
;	CP/M BASE PAGE
;
wstart	equ	0		;warm start entry point
defdrv	equ	4		;default user & disk
bdos	equ	5		;CP/M BDOS entry point
osbase	equ	bdos+1		;base of CP/M BDOS
cmdrv	equ	050h		;command drive
dfcb	equ	05ch		;1st default fcb
dufcb	equ	dfcb-1		;1st default fcb user number
pass0	equ	051h		;1st default fcb password addr
len0	equ	053h		;1st default fcb password length
dfcb1	equ	06ch		;2nd default fcb
dufcb1	equ	dfcb1-1		;2nd default fcb user number
pass1	equ	054h		;2nd default fcb password addr
len1	equ	056h		;2nd default fcb password length
buf	equ	80h		;default buffer
tpa	equ	100h		;transient program area
	if multi
comlen	equ	100h-19h	;maximum size of multiple command
				;RSX buffer with 16 byte header &
				;terminating zero
	else
comlen	equ	tpa-buf
	endif
;
;	BDOS FUNCTIONS
;
vers	equ	31h		;BDOS vers 3.1
cinf	equ	1		;console input
coutf	equ	2		;console output
crawf	equ	6		;raw console input 
pbuff	equ	9		;print buffer to console
rbuff	equ	10		;read buffer from console
cstatf	equ	11		;console status
resetf	equ	13		;disk system reset
self	equ	14		;select drive
openf	equ	15		;open file
closef	equ	16		;close file
searf	equ	17		;search first
searnf	equ	18		;search next
delf	equ	19		;delete file
readf	equ	20		;read file
makef	equ	22		;make file
renf	equ	23		;rename file
dmaf	equ	26		;set DMA address
userf	equ	32		;set/get user number
rreadf	equ	33		;read file
flushf	equ	48		;flush buffers
scbf	equ	49		;set/get SCB value
loadf	equ	59		;program load
allocf	equ	98		;reset allocation vector
trunf	equ	99		;read file
parsef	equ	152		;parse file
;
;	ASCII characters
;
ctrlc:	equ	'C'-40h
cr:	equ	'M'-40h
lf:	equ	'J'-40h
tab:	equ	'I'-40h
eof:	equ	'Z'-40h
;
;
;	RSX MEMORY MANAGEMENT EQUATES
;
;     	RSX header equates
;	
entry		equ	06h		;RSX contain jump to start
nextadd		equ	0bh		;address of next RXS in chain
prevadd		equ	0ch		;address of previous RSX in chain
warmflg		equ	0eh		;remove on wboot flag
endchain	equ	18h		;end of RSX chain flag
;
;	LOADER.RSX equates
;
module		equ	100h		;module address
;
;	COM file header equates
;
comsize		equ	tpa+1h		;size of the COM file
rsxoff		equ	tpa+10h		;offset of the RSX in COM file
rsxlen		equ	tpa+12h		;length of the RSX
;
;
;	SYSTEM CONTROL BLOCK OFFSETS
;
pag$off		equ	09ch
;
olog		equ	pag$off-0ch	; removeable media open vector
rlog		equ	pag$off-0ah	; removeable media login vector
bdosbase	equ	pag$off-004h	; real BDOS entry point
hashl		equ	pag$off+000h	; system variable
hash		equ	pag$off+001h	; hash code
bdos$version	equ	pag$off+005h	; BDOS version number
util$flgs	equ	pag$off+006h	; utility flags
dspl$flgs	equ	pag$off+00ah	; display flags
clp$flgs	equ	pag$off+00eh	; CLP flags
clp$drv		equ	pag$off+00fh	; submit file drive
prog$ret$code	equ	pag$off+010h	; program return code
multi$rsx$pg	equ	pag$off+012h	; multiple command buffer page
ccpdrv		equ	pag$off+013h	; ccp default drive
ccpusr		equ	pag$off+014h	; ccp default user number
ccpconbuf	equ	pag$off+015h	; ccp console buffer address
ccpflag1	equ	pag$off+017h	; ccp flags byte 1
ccpflag2	equ	pag$off+018h	; ccp flags byte 2
ccpflag3	equ	pag$off+019h	; ccp flags byte 3
conwidth	equ	pag$off+01ah	; console width
concolumn	equ	pag$off+01bh	; console column position
conpage		equ	pag$off+01ch	; console page length (lines)
conline		equ	pag$off+01dh	; current console line number
conbuffer	equ	pag$off+01eh	; console input buffer address
conbuffl	equ	pag$off+020h	; console input buffer length
conin$rflg	equ	pag$off+022h	; console input redirection flag
conout$rflg	equ	pag$off+024h	; console output redirection flag
auxin$rflg	equ	pag$off+026h	; auxillary input redirection flag
auxout$rflg	equ	pag$off+028h	; auxillary output redirection flag
listout$rflg	equ	pag$off+02ah	; list output redirection flag
page$mode	equ	pag$off+02ch	; page mode flag 0=on, 0ffH=off
page$def	equ	pag$off+02dh	; page mode default
ctlh$act	equ	pag$off+02eh	; ctl-h active
rubout$act	equ	pag$off+02fh	; rubout active (boolean)
type$ahead	equ	pag$off+030h	; type ahead active
contran		equ	pag$off+031h	; console translation subroutine
con$mode	equ	pag$off+033h	; console mode (raw/cooked)
ten$buffer	equ	pag$off+035h	; 128 byte buffer available
					; to banked BIOS
outdelim	equ	pag$off+037h	; output delimiter
listcp		equ	pag$off+038h	; list output flag (ctl-p)
q$flag		equ	pag$off+039h	; queue flag for type ahead
scbad		equ	pag$off+03ah	; system control block address
dmaad		equ	pag$off+03ch	; dma address
seldsk		equ	pag$off+03eh	; current disk
info		equ	pag$off+03fh	; BDOS variable "info"
resel		equ	pag$off+041h	; disk reselect flag
relog		equ	pag$off+042h	; relog flag
fx		equ	pag$off+043h	; function number
usrcode		equ	pag$off+044h	; current user number
dcnt		equ	pag$off+045h	; directory record number
searcha		equ	pag$off+047h	; fcb address for searchn function
searchl		equ	pag$off+049h	; scan length for search functions
multcnt		equ	pag$off+04ah	; multi-sector I/O count
errormode	equ	pag$off+04bh	; BDOS error mode
drv0		equ	pag$off+04ch	; search chain - 1st drive
drv1		equ	pag$off+04dh	; search chain - 2nd drive
drv2		equ	pag$off+04eh	; search chain - 3rd drive
drv3		equ	pag$off+04fh	; search chain - 4th drive
tempdrv		equ	pag$off+050h	; temporary file drive
patch$flag	equ	pag$off+051h	; patch flags
date		equ	pag$off+058h	; date stamp 
com$base	equ	pag$off+05dh	; common memory base address
error		equ	pag$off+05fh	; error jump...all BDOS errors
top$tpa		equ	pag$off+062h	; top of user TPA (address at 6,7)
;
;	CCP FLAG 1 BIT MASKS
;	(used with getflg, setflg and resetflg routines)
;
chainflg	equ	080h		; program chain (funct 49)
not$chainflg	equ	03fh		; mask to reset chain flags
chainenv	equ	040h		; preserve usr/drv for chained prog
comredirect	equ	0b320h		; command line redirection active
menu		equ	0b310h		; execute ccp.ovl for menu systems
echo		equ	0b308h		; echo commands in batch mode
userparse	equ	0b304h		; parse user numbers in commands
subfile		equ	0b301h		; $$$.SUB file found or active
subfilemask	equ	subfile-0b300h
rsx$only$set	equ	02h		; RSX only load (null COM file)
rsx$only$clr	equ 	0FDh		; reset RSX only flag
;
;	CCP FLAG 2 BIT MASKS
;	(used with getflg, setflg and resetflg routines)
;
ccp10		equ	0b4a0h		; CCP function 10 call (2 bits)
ccpsub		equ	0b420h		; CCP present (for SUBMIT, PUT, GET)
ccpbdos		equ	0b480h		; CCP present (for BDOS buffer save)
dskreset	equ	20h		; CCP does disk reset on ^C from prompt
submit		equ	0b440h		; input redirection active
submitflg	equ	40h		; input redirection flag value
order		equ	0b418h		; command order
					;  0 - COM only
					;  1 - COM,SUB
					;  2 - SUB,COM
					;  3 - reserved
datetime	equ	0b404h		; display date & time of load
display		equ	0b403h		; display filename & user/drive
filename	equ	02h		; display filename loaded 
location	equ	01h		; display user & drive loaded from

;
;	CCP FLAG 3 BIT MASKS
;	(used with getflg, setflg and resetflg routines)
;
rsxload		equ	1h		; load RSX, don't fix chain
coldboot	equ	2h		; try to exec profile.sub
;
;   	CONMODE BIT MASKS
;
ctlc$stat	equ	0cf01h		;conmode CTL-C status

;
;
;************************************************************************
;
;	Console Command Processor - Main Program
;
;************************************************************************
;
;
;
start:
;
	lxi	sp,stack
	lxi	h,ccpret		;push CCPRET on stack, in case of
	push	h			; profile error we will go there
	lxi	d,scbadd
	mvi	c,scbf
	call	bdos
	shld	scbaddr			;save SCB address
	mvi	l,com$base+1
	mov	a,m			;high byte of commonbase
	sta	banked			;save in loader
	mvi	l,bdosbase+1		;HL addresses real BDOS page
	mov	a,m			;BDOS base in H
	sta 	realdos			;save it for use in XCOM routine
;
	lda	osbase+1		;is the LOADER in memory?
	sub	m			;compare link at 6 with real BDOS
	jnz	reset$alloc		;skip move if loader already present
;
;
movldr:
	lxi	b,rsxend-rsxstart	;length of loader RSX
	call	calcdest	;calculate destination and (bias+200h)
	mov	h,e		;set to zero
	mov	l,e
;	lxi	h,module-100h	;base of loader RSX (less 100h)
	call	reloc		;relocate loader
	lhld	osbase		;HL = BDOS entry, DE = LOADER base
	mov	l,e		;set L=0
	mvi	c,6
	call	move		;move the serial number down
	mvi	e,nextadd
	call	fixchain1
;
;
reset$alloc:
	mvi	c,allocf
	call	bdos
;
;	
;
;************************************************************************
;
;	INITIALIZE SYSTEM CONTROL BLOCK
;
;************************************************************************
;
;
scbinit:
	;
	;	# dir columns, page size & function 9 delimiter
	;
	mvi 	b,conwidth	
	call	getbyte
	inr	a		;get console width (rel 1)
	rrc
	rrc	
	rrc
	rrc
	ani	0fh		;divide by 16
	lxi	d,dircols
	stax	d		;dircols = conwidth/16
	mvi	l,conpage
	mov	a,m
	dcr	a		;subtract 1 for space before prompt
	inx	d
	stax	d		;pgsize = conpage
	xra	a
	inx	d
	stax	d		;line=0
	mvi	a,'$'
	inx	d
	stax	d		;pgmode = nopage (>0)
	mvi	l,outdelim
	mov	m,a		;set function 9 delimiter 
	;
	;	multisector count, error mode, console mode 
	;		& BDOS version no.
	;
	mvi 	l,multcnt 
	mvi 	m,1 		;set multisector I/O count = 1
	inx	h		;.errormode
	xra 	a
	mov	m,a		;set return error mode = 0
	mvi	l,con$mode
	mvi	m,1		;set ^C status mode
	inx	h
	mov	m,a		;zero 2nd conmode byte
	mvi	l,bdos$version
	mvi	m,vers		;set BDOS version no.
	;
	;	disk reset check 
	;
	mvi	l,ccpflag2
	mov	a,m
	ani	dskreset	;^C at CCP prompt?
	mvi	c,resetf
	push	h
	cnz	bdos		;perform disk reset if so
	pop	h
	;
	;	remove temporary RSXs (those with remove flag on)
	;
rsxck:
	mvi	l,ccpflag1	;check CCP flag for RSX only load
	mov	a,m
	ani	rsx$only$set	;bit = 1 if only RSX has been loaded
	push	h
	cz	rsx$chain	;don't fix-up RSX chain if so
	pop	h
	mov	a,m
	ani	rsx$only$clr	;clear RSX only loader flag
	mov	m,a		;replace it
	;
	;	chaining environment
	;
	ani	chain$env	;non-zero if we preserve programs
	push	h		;user & drive for next transient
	;
	;	user number
	;
	mvi 	l,ccpusr	; HL = .CCP USER (saved in SCB)
	lxi	b,usernum	; BC = .CCP'S DEFAULT USER
	mov	d,h
	mvi	e,usrcode	; DE = .BDOS USER CODE
	ldax	d
	stax	b		; usernum = bdos user number
	mov 	a,m		; ccp user
	jnz	scb1		; jump if chaining env preserved
	stax	b		; usernum = ccp default user
scb1:	stax	d		; bdos user = ccp default user
	;
	;	transient program's current disk
	;
	inx	b		;.CHAINDSK
	mvi	e,seldsk	;.BDOS CURRENT DISK
	ldax	d
	jnz	scb2		; jump if chaining env preserved
	mvi	a,0ffh
;	cma			; make an invalid disk
scb2:	stax 	b		; chaindsk = bdos disk (or invalid)
	;
	;	current disk
	;
	dcx	h		;.CCP's DISK (saved in SCB)
	inx	b		;.CCP's CURRENT DISK
	mov	a,m
	stax	b
	stax	d		; BDOS current disk
	;
	;	$$$.SUB drive 
	;
	mvi 	l,tempdrv 
	inx 	b 		;.SUBFCB
	mov 	a,m
	stax 	b		; $$$.SUB drive = temporary drive
	;	
	;	check for program chain
	;
	pop	h		;HL =.ccpflag1
	mov	a,m
	ani	chainflg	;is it a chain function (47)
	jz 	ckboot		;jump if not
	lxi 	h,buf 
chain:	lxi 	d,cbufl 
	mvi 	c,tpa-buf-1
	mov	a,c
	stax	d
	inx	d
	call 	move		;hl = source, de = dest, c = count
	jmp 	ccpparse
	;	
	;	execute profile.sub ?
	;
ckboot:	mvi	l,ccpflag3
	mov	a,m
	ani	coldboot	;is this a cold start
	jnz	ccpcr		;jump if not
	mov	a,m
	ori	coldboot	;set flag for next time
	mov	m,a
	sta	errflg		;set to ignore errors
	lxi	h,profile
	jmp	chain		;attempt to exec profile.sub
profile:
	db	'PROFILE.S',0
;
;
;
;************************************************************************
;
;	BUILT-IN COMMANDS (and errors) RETURN HERE
;
;************************************************************************
;
;
ccpcr:
	;	enter here on each command or error condition
	call	setccpflg
	call 	crlf
ccpret:
	lxi	h,stack-2	;reset stack in case of error
	sphl			;preserve CCPRET on stack
	xra	a
	sta	line
	lxi	h,ccpret	;return for next builtin
	push	h
	call	setccpflg
	dcx	h		;.CCPFLAG1
	mov	a,m
	ani 	subfilemask	;check for $$$.SUB submit
	jz 	prompt
;
;
;
;************************************************************************
;
;	$$$.SUB file processing
;
;************************************************************************
;
;
	lxi	d,cbufl		;set DMA to command buffer
	call	setbuf
	mvi 	c,openf
	call 	sudos		;open it if flag on
	mvi	c,cstatf	;check for break if successful open
	cz	sudos		;^C typed?
	jnz	subclose	;delete $$$.SUB if break or open failed
	lxi	h,subrr2
	mov	m,a		;zero high random record #
	dcx	h
	mov	m,a		;zero middle random record #
	dcx	h
	push	h
	lda 	subrc 
	dcr 	a 	
	mov	m,a		;set to read last record of file
	mvi	c,rreadf
	cp	sudos
	pop	h
	dcr	m		;record count (truncate last record)
	mvi	c,delf
	cm	sudos
	ora	a		;error on read?
	;
	;
subclose:
	push	psw
	mvi	c,trunf		;truncate file (& close it)
	call	sudos
	pop	psw		;any errors ?
	jz	ccpparse	;parse command if not
	;
	;
subkill:
	lxi 	b,subfile
	call 	resetflg	;turn off submit flag
	mvi 	c,delf
	call 	sudos		;kill submit
;
;
;
;************************************************************************
;
;	GET NEXT COMMAND
;
;************************************************************************
;
;
	;
	; 	prompt user
	;
prompt:
	lda 	usernum
	ora 	a 
	cnz 	pdb		;print user # if non-zero
	call	dirdrv1
	mvi 	a,'>' 
	call 	putc
	;
	if multi
	;move ccpconbuf addr to conbuffer addr
	lxi	d,ccpconbuf*256+conbuffer
	call	wordmov		;process multiple command, unless in submit
	ora	a		;non-zero => multiple commands active
	push	psw		;save A=high byte of ccpconbuf
	lxi	b,ccpbdos
	cnz	resetflg	;turn off BDOS flag if multiple commands
	endif
	call	rcln		;get command line from console
	call	resetccpflg	;turn off BDOS, SUBMIT & GET ccp flags
	if multi
	pop	psw		;D=high byte of ccpconbuf
	cnz	multisave	;save multiple command buffer
	endif
;
;
;
;************************************************************************
;
;	PARSE COMMAND
;
;************************************************************************
;
;
ccpparse:	
	;
	;	reset default page mode 
	;	(in case submit terminated)
	;
	call	subtest		;non-zero if submit is active
	jnz	get$pg$mode	;skip, if so
set$pg$mode:
	mvi	l,page$def
	mov	a,m		;pick up default
	dcx	h
	mov	m,a		;place in mode
get$pg$mode:
	mvi	l,page$mode
	mov	a,m
	sta	pgmode
	;
	;check for multiple commands
	;convert to upper case
	;reset ccp flag, in case entered from a CHAIN (or profile)
	;
	call	uc		;convert to upper case, ck if multiple command
	rz			;get another line if null or comment
	;
	;transient or built-in command?
	;
	lxi	d,ufcb		;include user number byte in front of FCB
	call	gcmd		;parse command name
	lda	fcb+9		;file type specified?
	cpi	' '
	jnz	ccpdisk2	;execute from disk, if so
	lxi	h,ufcb		;user or drive specified?
	mov	a,m		;user number
	inx	h
	ora	m		;drive
	inx	h
	mov	a,m		;get 1st character of filename
	jnz	ccpdisk3	;jump if so
	;
	;BUILT-IN HANDLER
	;
ccpbuiltin:
	lxi	h,ctbl		;search table of internal commands
	lxi	d,fcb+1
	lda	fcb+3
	cpi	' '+1		;is it shorter that 3 characters?
	cnc	tbls		;is it a built-in?
	jnz	ccpdisk0	;load from disk if not
	lda	option		;[ in command line?
	ora	a		;options specified?
	mov	a,b		;built-in index from tbls
	lhld	parsep
	shld	errsav		;save beginning of command tail
	lxi	h,ptbl		;jump to processor if options not
	jz	tblj		;specified
	cpi	4
	jc	trycom
	lxi	h,fcb+4
	jnz	ccpdisk0	;if DIRS then look for DIR.COM
	mvi	m,' '
	;
	;LOAD TRANSIENT (file type unspecified)
	;
ccpdisk0:
	lxi	b,order
	call	getflg		;0=COM   8=COM,SUB  16=SUB,COM
	jz	ccpdisk2	;search for COM file only
	mvi	b,8		;=> 2nd choice is SUB
	sub	b		;now a=0 (COM first) or 8 (SUB first)
	jz	ccpdisk1	;search for COM first then SUB
	mvi	b,0		;search for SUB first then COM

ccpdisk1:
	push	b		;save 2nd type to try
	call	settype		; A = offset of type in type table
	call	exec		;try to execute, return if unsuccessful
	pop	psw		;try 2nd type 
	call	settype
	;
	;LOAD TRANSIENT (file type specified)
	;
ccpdisk2:
	call	exec
	jmp	perror		;error if can't find it
	;
	;DRIVE SPECIFIED (check for change drives/users command)
	;
ccpdisk3:
	cpi	' '		;check for filename
	jnz	ccpdisk0	;execute from disk if specified
	call	eoc		;error if not end of command
	lda	ufcb		;user specified?
	sui	1
	jc	ccpdrive

ccpuser:
	sta	usernum		;CCP's user number
	mvi	b,ccpusr
	call	setbyte		;save it in SCB
	call	setuser		;set current user

ccpdrive:
	lda	fcb		;drive specified?
	dcr	a
	rm			;return if not
	push	psw
	call	select
	pop	psw
	sta	disk		;CCP's drive
	mvi	b,ccpdrv
	jmp	setbyte		;save it in SCB

;;
;
;************************************************************************
;
;	BUILT-IN COMMANDS 
;
;************************************************************************
;
;
;	Table of internal ccp commands
;
;
ctbl:	db	'DIR '
	db	'TYPE '
	db	'ERASE '
	db	'RENAME '
	db	'DIRSYS '
	db	'USER '
	db	0
;
ptbl:	dw	dir
	dw	type
	dw	era
	dw	ren
	dw	dirs
	dw	user
;;
;;-----------------------------------------------------------------------
;;
;;	DIR Command
;;
;;	DIR		list directory of current default user/drive
;;	DIR <X>:	list directory of user/drive <X>
;;	DIR <AFN>	list all files on the current default user/drive
;;			with names that match <AFN>
;;	DIR <X>:<AFN>	list all files on user/drive <X> with names that
;;			match <AFN>
;;
;;-----------------------------------------------------------------------
;;
;
	if newdir
dirdrv:
	lda	dfcb		;get disk number
	endif

dirdrv0:
	dcr	a
	jp	dirdrv2

dirdrv1:
	lda	disk		;get current disk
dirdrv2:
	adi	'A'
	jmp	pfc		;print it (save BC,DE)
;
;
	if newdir
dir:
	mvi	c,0		;flag for DIR (normal)
	lxi	d,sysfiles
	jmp	dirs1
;
;
dirs:
	mvi	c,080h		;flag for DIRS (system)
	lxi	d,dirfiles

dirs1:	push	d
	call	direct
	pop	d		;de = .system files message
	jz	nofile		;jump if no files found
	mov	a,l		;A = number of columns
	cmp	b		;did we print any files?
	cnc	crlf		;print crlf if so
	lxi	h,anyfiles
	dcr	m
	inr	m
	rz			;return if no files 
				;except those requested
	dcr	m		;set to zero
	jmp	pmsgnl		;tell the operator other files exist
;
;
direct:
	push	b		;save DIR/DIRS flag
	call	sbuf80		;set DMA = 80h
	call	gfn		;parse file name
	lxi	d,dfcb+1
	ldax	d
	cpi	' '
	mvi	b,11
	cz	setmatch	;use "????????.???" if none
	call	eoc		;make sure there's nothing else
	call	srchf		;search for first directory entry
	pop	b
	rz			;if no files found
dir0:
	lda	dircols		;number of columns for dir
	mov	l,a
	mov	b,a
	inr	b		;set # names to print per line (+1)
dir1:
	push	h		;L=#cols, B=curent col, C=dir/dirs 
	lxi	h,10		;get byte with SYS bit
	dad	d
	mov	a,m
	pop	h
	ani	80h		;look at SYS bit
	cmp	c		;DIR/DIRS flag in C
	jz	dir2		;display, if modes agree
	mvi	a,1		;set anyfiles true
	sta	anyfiles
	jmp	dir3		;don't print anything
;
;	display the filename
;
dir2:
	dcr	b
	cz	dirln		;sets no. of columns, puts crlf
	mov	a,b		;number left to print on line
	cmp	l		;is current col = number of cols
	cz	dirdrv		;display the drive, if so
	mvi	a,':'
	call	pfc		;print colon
	call	space
	call	pfn		;print file name
	call	space		;pad with space
dir3:	
	push	b		;save current col(B), DIR/DIRS(C)
	push	h		;save number of columns(L)
	call	break		;drop out if keyboard struck
	call	srchn		;search for another match
	pop	h
	pop	b
	jnz	dir1
direx:
	inr	a		;clear zero flag 
	ret

	else

dirs:	; display system files only
	mvi	a,0d2h		; JNC instruction
	sta	dir11		; skip on non-system files
;
dir:	; display non-system files only
	lxi	h,ccpcr
	push	h		; push return address
	call	gfn		;parse file name
	inx	d
	ldax	d
	cpi	' '
	mvi	b,11
	cz	setmatch	;use "????????.???" if none
	call	eoc		;make sure there's nothing else
	call	findone		;search for first directory entry
	jz	dir4
	mvi	b,5		;set # names to print per line
dir1:	lxi	h,10		;get byte with SYS bit
	dad	d
	mov	a,m
	ral			;look at SYS bit
dir11:	jc	dir3		;don't print it if SYS bit set
	mov	a,b
	push	b
dir2:	lxi	h,9		;get byte with R/O bit
	dad	d
	mov	a,m
	ral			;look at R/O bit
	mvi	a,' '		;print space if not R/O
	jnc	dir21		;jump if not R/O
	mvi	a,'*'		;print star if R/O
dir21:	call	pfc		;print character
	call	pfn		;print file name
	mvi	a,13		;figure out how much padding is needed
	sub	c
dir25:	push	psw
	call	space		;pad it out with spaces
	pop	psw
	dcr	a
	jnz	dir25		;loop if more required
	pop	b
	dcr	b		;decrement # names left on line
	jnz	dir3
	call	crlf		;go to new line
	mvi	b,5		;set # names to print on new line
dir3:	push	b
	call	break		;drop out if keyboard struck
	call	srchn		;search for another match
	pop	b
	jnz	dir1

dir4:	mvi	a,0dah		;JC instruction
	sta	dir11		;restore normal dir mode (skip system files)
	jmp	ccpcr

	endif

;;
;;-----------------------------------------------------------------------
;;
;;	TYPE command
;;
;;	TYPE <UFN>	Print the contents of text file <UFN> on
;;			the console.
;;
;;-----------------------------------------------------------------------
;;
type:	lxi	h,ccpcr
	push	h		;push return address
	call	getfn		;get and parse filename
	mvi	a,127		;initialize buffer pointer
	sta	bufp
	mvi	c,openf
	call	sbdosf		;open file if a filename was typed
type1:	call	break		;exit if keyboard struck
	call	getb		;read byte from file
	rnz			;exit if physical eof or read error
	cpi	eof		;check for eof character
	rz			;exit if so
	call	putc		;print character on console
	jmp	type1		;loop
;
;;-----------------------------------------------------------------------
;;
;;	USER command
;;
;;	USER <NN>	Set the user number
;;
;;-----------------------------------------------------------------------
;;
user:
	lxi	d,unmsg		;Enter User #:
	call	getprm
	call	gdn		;convert to binary
	rz			;return if nothing typed
	jmp	ccpuser		;set user number 
;
;;-----------------------------------------------------------------------
;;
;;	ERA command
;;
;;	ERA <AFN>	Erase all file on the current user/drive
;;			which match <AFN>.
;;	ERA <X>:<AFN>	Erase all files on user/drive <X> which
;;			match <AFN>.
;;
;;-----------------------------------------------------------------------
;;
era:	call	getfn		;get and parse filename
	jz	era1
	call	ckafn		;is it ambiguous?
	jnz	era1
	lxi	d,eramsg
	call	pmsg
	lhld	errorp
	mvi	c,' '		;stop at exclamation mark or 0
	call	pstrg		;echo command
	lxi	d,confirm
	call	getc
	call	crlf
	mov	a,l		;character in L after CRLF routine
	ani	5fh		;convert to U/C
	cpi	'Y'		;Y (yes) typed?
	rnz			;return, if not
	ora	a		;reset zero flag
era1:	mvi	c,delf	
	jmp	sbdosf

;;-----------------------------------------------------------------------
;;
;;
;;	REN command
;;
;;-----------------------------------------------------------------------
;;
ren:	call	gfn		;zero flag set if nothing entered
	push	psw		
	lxi	h,16
	dad	d
	xchg
	push	d		;DE = .dfcb+16
	push	h		;HL = .dfcb
	mvi	c,16
	call	move		;DE = dest, HL = source
	call	gfn
	pop	h		;HL=.dfcb
	pop	d		;DE=.dfcb+16
	call	drvok
	mvi	c,renf		;make rename call
	pop	psw		;zero flag set if nothing entered
;
;;-----------------------------------------------------------------------
;;
;;	BUILT-IN COMMAND BDOS CALL & ERROR HANDLERS
;;
;;-----------------------------------------------------------------------
;
sbdosf:
	push	psw
	cnz	eoc		;make sure there's nothing else
	pop	psw
	lxi	d,dfcb
	mvi	b,0ffh
	mvi	h,1		;execute disk command if we don't call
	cnz	bdosf		;call if something was entered
	rnz			;return if successful

ferror:
	dcr	h		;was it an extended error?
	jm	nofile
	lhld	errsav
	shld	parsep
trycom:	call	exec
	call 	pfn
	lxi	d,required
	jmp	builtin$err
;
;;-----------------------------------------------------------------------
;
;
;	check for drive conflict
;	HL =  FCB 
;	DE =  FCB+16
;
drvok:	ldax	d		;get byte from 2nd fcb
	cmp	m		;ok if they match
	rz
	ora	a		;ok if 2nd is 0
	rz
	inr	m		;error if the 1st one's not 0
	dcr	m
	jnz	perror
	mov	m,a		;copy from 2nd to 1st
	ret
;;-----------------------------------------------------------------------
;;
;;	check for ambiguous reference in file name/type
;;
;;	entry:	b  = length of string to check (ckafn0)
;;		de = fcb area to check (ckafn0) - 1
;;	exit:	z  = set if any ? in file reference (ambiguous)
;;		z  = clear if unambiguous file reference
;;
ckafn:
		mvi	b,11		;check entire name and type
ckafn0:		inx	d
		ldax	d
		cpi	'?'		;is it an ambiguous file name
if newera
		rz			;return true if any afn
else
		rnz			;return true only if *.*
endif
		dcr	b
		jnz	ckafn0
if newera
		dcr	b		;clear zero flag to return false
endif
		ret			;remove above DCR to return true
;;
;;-----------------------------------------------------------------------
;;
;;	get parameter (generally used to get a missing one)
;;
getprm:
	call	skps		;see if already there
	rnz			;return if so
getp0:
	if prompts
	push	d
	lxi	d,enter
	call	pmsg
	pop	d
	endif
	call	pmsg		;print prompt
	call	rcln		;get response
	jmp	uc		;convert to upper case
;
;;
;;-----------------------------------------------------------------------
	if	not newdir
;;
;;	search for first file, print "No File" if none
;;
findone:
	call	srchf
	rnz			;found
	endif
;;-----------------------------------------------------------------------

nofile:
	lxi	d,nomsg		;tell user no file found
builtin$err:
	call	pmsgnl
	jmp	ccpret

;
;
;************************************************************************
;
;	EXECUTE DISK RESIDENT COMMAND
;
;************************************************************************
;
;
xfcb:	db	0,'SUBMIT  COM'	;processor fcb
;
;
;	execute submit file  (or any other processor)
;
xsub:				;DE = .fcb
	ldax	d
	mvi	b,clp$drv
	call	setbyte		;save submit file drive
	lxi	h,xfcb
	mvi	c,12
	call	move		;copy processor into fcb
	lxi	h,cbufl		;set parser pointer back to beginning
	mvi	m,' '
	inx	h		;move past blank
	shld	parsep
;				 execute SUBMIT.COM
;
;	
;	execute disk resident command (return if not found or error)
;
exec:
	;try to open and execute fcb
	lxi	d,fcb+9
	lxi	h,typtbl
	call	tbls		;search for type in type table
	rnz			;return if no match
	lxi	d,ufcb
	ldax	d		;check to see if user specified
	ora	a
	rnz			;return if so
	inx	d
	ldax	d		;check if drive specified
	mov	c,a
	push	b		;save type (B) and drive (C)
	mvi	c,0		;try only 1 open if drive specified
	ora	a
	jnz	exec1		;try to open as specified
	lxi	b,(drv0-1)*256+4;try upto four opens from drv chain
	lda	disk
	inr	a
	mov	h,a		;save default disk in H
	mvi	l,1		;allow only 1 match to default disk
exec0:	inr	b		;next drive to try in SCB drv chain
	dcr	c		;any more tries?
	mov	a,c
	push	h
	cp	getbyte
	pop	h
	ora	a
	jm	exec3
	jz	exec01		;jump if drive is 0 (default drive)
	cmp	h		;is it the default drive
	jnz	exec02		;jump if not
exec01:	mov	a,h		;set drive explicitly
	dcr	l		;is it the 2nd reference 
	jm	exec0		;skip, if so
exec02:	stax	d		;put drive in FCB
exec1:	push	b		;save drive offset(B) & count(C)
	push	h
	call	opencom		;on default drive & user
	pop	h
	pop	b
	jz	exec0		;try next if open unsuccessful
;
;	successful open, now jump to processor
;	
exec2:
	if	dayfile
	lxi	b,display
	call	getflg
	jz	exec21
	ldax	d
	call	dirdrv0
	mvi	a,':'
	call	pfc
	push	d
	call	pfn
	pop	d
	push	d
	lxi	h,8
	dad	d
	mov	a,m
	ani	80h
	lxi	d,userzero
	cnz	pmsg
	call	crlf
	pop	d
	endif
exec21:	pop	psw		;recover saved command type
	lxi	h,xptbl
;
;	table jump
;
;	entry:	hl = address of table of addresses
;		a  = entry # (0 thru n-1)
;
tblj:	add	a		;adjust for two byte entries
	call	addhla		;compute address of entry
	push	d
	mov	e,m		;fetch entry
	inx	h
	mov	d,m
	xchg
	pop	d
	pchl			;jump to it
;
typtbl:	db	'COM '
	db	'SUB '
	db	'PRL '
	db	0
;
xptbl:	dw	xcom
	dw	xsub
	dw	xcom


;
;	unsuccessful attempt to open command file
;
exec3:	pop	b		;recover drive
	mov	a,c
	stax	d		;replace in fcb
	ret
;
;
settype:
	;set file type specified from type table
	;a = offset (x2) of desired type (in bytes)
	rrc
	lxi	h,typtbl
	call	addhla		;hl = type in type table
	lxi	d,fcb+9
	mvi	c,3
	jmp	move		;move type into fcb
;
;
;
;	EXECUTE COM FILE
;
xcom:				;DE = .fcb
	;
	;	set up FCB for loader to use
	;
	lxi	h,tpa
	shld	fcbrr		;set load address to 100h
	lhld	realdos-1	;put fcb in the loader's stack
	dcr	h		;page below LOADER (or bottom RSX)
	mvi	l,0C0h		;offset for FCB in page below the BDOS
	push	h		;save for LOADER call
	ldax	d		;get drive from fcb(0)
	sta	cmdrv		;set command drive field in base page
	xchg
	mvi	c,35
	call	move		;now move FCB to the top of the TPA
	;	
	;	set up base page
	;
	lxi	h,errflg	;tell parser to ignore errors
	inr	m
xcom3:	lhld	parsep
	dcx	h		;backup over delimiter
	lxi	d,buf+1
	xchg
	shld	parsep		;set parser to 81h
	call	copy0		;copy command tail to 81h with
				;terminating 0 (returns A=length)
	sta	buf		;put command tail length at 80h
xcom5:	call	gfn		;parse off first argument
	shld	pass0
	mov	a,b
	sta	len0
	lxi	d,dfcb1
	call	gfn0		;parse off second argument
	shld	pass1
	mov	a,b
	sta	len1
xcom7:	lxi	h,chaindsk		;.CHAINDSK
	mov	a,m
	ora	a
	cp	select
	lda	usernum
	call	setuser		;set default user, returns H=SCB
	add	a		;shift user to high nibble
	add	a
	add	a
	add	a
	mvi	l,seldsk
	ora	m		;put disk in low nibble
	sta	defdrv		;set location 4 
	;
	; 	initialize stack
	;
xcom8:	pop	d			;DE = .fcb
	lhld	realdos-1		;base page of BDOS
	xra	a
	mov	l,a			;top of stack below BDOS
	sphl				;change the stack pointer for CCP
	mov 	h,a			;push warm start address on stack
	push 	h			;for programs returning to the CCP
	inr	h			;Loader will return to TPA
	push	h			;after loading a transient program
	;
	;	initialize fcb0(CR), console mode, program return code
	;	& removable media open and login vectors
	;
xcom9:	sta	7ch			;clear next record to read
	mvi	b,con$mode
	call	setbyte			;set to zero (turn off ^C status)
	mvi	l,olog
	mov	m,a			;zero removable open login vector
	inx	h
	mov	m,a
	inx	h
	mov	m,a			;zero removable media login vector
	inx	h
	mov	m,a
	mvi	l,ccpflag1
	mov	a,m
	ani	chain$flg		;chaining?
	jnz	loader			;load program without clearing
	mvi	l,prog$ret$code		;the program return code
	mov	m,a			;A=0
	inx	h
	mov	m,a			;set program return = 0000h
	;
	;	call loader
	;
loader:
	mov	a,m			;reset chain flag if set,
	ani	not$chainflg		;has no effect if we fell through
	mov	m,a
	mvi	c,loadf			;use load RSX to load file
	jmp	bdos			;now load it
;
;
;
;
;************************************************************************
;
;	BDOS FUNCTION INTERFACE - Non FCB functions
;
;************************************************************************
;
;
;
;;-----------------------------------------------------------------------
;;
;;
;;
;;	print character on terminal
;;	pause if screen is full
;;	(BDOS function #2)
;;
;;	entry:	a  = character (putc entry)
;;		e  = character (putc2 entry)
;;

putc:	cpi	lf		;end of line?
	jnz	putc1		;jump if not
	lxi	h,pgsize	;.pgsize
	mov	a,m		;check page size
	inx	h		;.line
	inr	m		;line=line+1
	sub	m		;line=page?
	jnz	putc0		
	mov	m,a		;reset line=0 if so
	inx	h		;.pgmode
	mov	a,m		;is page mode off?
	ora	a		;page=0 if so
	lxi	d,more
	cz	getc		;wait for input if page mode on
	cpi	ctrlc
	jz	ccpcr
	mvi	e,cr
	call	putc2		;print a cr
putc0:	mvi	a,lf		;print the end of line char
putc1:	mov	e,a
putc2:	mvi	c,coutf
	jmp	bdos

;;
;;-----------------------------------------------------------------------
;;
;;	get character from console
;;	(BDOS function #1)
;;
getc:	call	pmsg
getc1:	mvi	c,cinf
	jmp	bdos
;;
;;-----------------------------------------------------------------------
;;
;;	print message string on terminal
;;	(BDOS function #9)
;;
pmsg:	mvi	c,pbuff
	jmp	bdos
;;
;;-----------------------------------------------------------------------
;;
;;	read line from console
;;	(calls BDOS function #10)
;;
;;	exit:	z  = set if null line
;;
;;	This function uses the buffer "cbuf" (see definition of
;;	function 10 for a description of the buffer).  All input
;;	is converted to upper case after reading and the pointer
;;	"parsep" is set to the begining of the first non-white
;;	character string.
;;
rcln:	lxi	h,cbufmx	;get line from terminal
	mvi	m,comlen	;set maximum buffer size
	xchg
	mvi	c,rbuff
	call	bdos
	lxi	h,cbufl		;terminate line with zero byte
	mov	a,m
	inx	h
	call	addhla
	mvi	m,0		;put zero at the end 
	jmp	crlf		;advance to next line
;
;;
;;-----------------------------------------------------------------------
;;
;;	exit routine if keyboard struck
;;	(calls BDOS function #11)
;;
;;	Control is returned to the caller unless the console
;;	keyboard has a character ready, in which case control
;;	is transfer to the main program of the CCP.
;;
break:	call	break1	
	rz
	jmp	ccpcr

break1:	mvi	c,cstatf
	call	rw
	rz
	mvi	c,cinf
	jmp	rw


;;
;;-----------------------------------------------------------------------
;;
;;	set disk buffer address
;;	(BDOS function #26)
;;
;;	entry:	de -> buffer ("setbuf" only)
;;
sbuf80:	lxi	d,buf
setbuf:	mvi	c,dmaf
	jmp	bdos
;;
;;-----------------------------------------------------------------------
;;
;;	select disk
;;	(BDOS function #14)
;;
;;	entry:	a  = drive
;;
select:
	mov	e,a
	mvi 	c,self
	jmp 	bdos
;
;;
;;-----------------------------------------------------------------------
;;
;;	set user number
;;	(BDOS function #32)
;;
;;	entry:	a  = user # 
;;	exit:	H  = SCB page
;;
setuser:
	mvi 	b,usrcode 
	jmp 	set$byte
;
;
;
;************************************************************************
;
;	BDOS FUNCTION INTERFACE - Functions with a FCB Parameter
;
;************************************************************************
;
;
;;
;;	open file 
;;	(BDOS function #15)
;;
;;	exit:	z  = set if file not found
;;
;;
opencom:			;open command file (SUB, COM or PRL)
	lxi	b,openf		;b=0 => return error mode of 0
	lxi	d,fcb		;use internal FCB

;;	BDOS CALL ENTRY POINT   (used by built-ins)
;;
;;	entry:	b  = return error mode (must be 0 or 0ffh)
;;		c  = function no.
;;		de = .fcb
;;	exit:	z  = set if error
;;		de = .fcb
;;
bdosf:	lxi	h,32		;offset to current record
	dad	d		;HL = .current record
	mvi	m,0		;set to zero for read/write
	push	b		;save function(C) & error mode(B)
	push	d		;save .fcb
	ldax	d		;was a disk specified?
	ana	b		;and with 0 or 0ffh
	dcr	a		;if so, select it in case
	cp	select		;of permanent error (if errmode = 0ffh)
	lxi	d,passwd
	call	setbuf		;set dma to password
	pop	d		;restore .fcb
	pop	b		;restore function(C) & error mode(B)
	push	d
	lhld	scbaddr
	mvi	l,errormode
	mov	m,b		;set error mode
	push	h		;save .errormode
	call	bdos
	pop	d		;.errormode
	xra	a
	stax	d		;reset error mode to 0
	lda	disk
	mvi	e,seldsk
	stax	d		;reset current disk to default
	push	h		;save bdos return values
	call	sbuf80
	pop	h		;bdos return
	inr	l		;set z flag if error
	pop	d		;restore .fcb
	ret
;;
;;-----------------------------------------------------------------------
;;
;;	close file 
;;	(BDOS function #16)
;;
;;	exit:	z  = set if close error
;;
;;close:	mvi	c,closef
;;		jmp	oc
;;
;;-----------------------------------------------------------------------
;;
;;	delete file 
;;
;;	exit:	z  = set if file not found
;;
;;	The match any character "?" may be used without restriction
;;	for this function.  All matched files will be deleted.
;;
;;
;;delete:
;;	mvi	c,delf
;;	jmp	oc
;;
;;-----------------------------------------------------------------------
;;
;;	create file 
;;	(BDOS function #22)
;;
;;	exit:	z  = set if create error
;;
;;make:		mvi	c,makef
;;		jmp	oc
;;-----------------------------------------------------------------------
;;
;;	search for first filename match (using "DFCB" and "BUF")
;;	(BDOS function #17)
;;
;;	exit:	z  = set if no match found
;;		z  = clear if match found
;;		de -> directory entry in buffer
;;
srchf:	mvi	c,searf		;set search first function
	jmp	srch
;;
;;-----------------------------------------------------------------------
;;
;;	search for next filename match (using "DFCB" and "BUF")
;;	(BDOS function #18)
;;
;;	exit:	z  = set if no match found
;;		z  = clear if match found
;;		de -> directory entry in buffer
;;
srchn:	mvi	c,searnf	;set search next function
srch:	lxi	d,dfcb		;use default fcb
	call	bdos
	inr	a		;return if not found
	rz
	dcr	a		;restore original return value
	add	a		;shift to compute buffer pos'n
	add	a
	add	a
	add	a
	add	a
	lxi	h,buf		;add to buffer start address
	call	addhla
	xchg			;de -> entry in buffer
	xra	a		;may be needed to clear z flag
	dcr	a		;depending of value of "buf"
	ret
;;
;;-----------------------------------------------------------------------
;;
;;	read file 
;;	(BDOS function #20)
;;
;;	entry:	hl = buffer address (readb only)
;;	exit	z  = set if read ok
;;
read:	xra	a		;clear getc pointer
	sta	bufp
	mvi	c,readf
	lxi	d,dfcb
rw:	call	bdos
	ora	a
	ret
;
;;
;;-----------------------------------------------------------------------
;;
;;	$$$.SUB interface
;;
;;	entry:	c = bdos function number
;;	exit	z  = set if successful

sudos:	lxi	d,subfcb
	jmp	rw
;
;
;
;************************************************************************
;
;	COMMAND LINE PARSING SUBROUTINES 
;
;************************************************************************
;
;------------------------------------------------------------------------
;
;	COMMAND LINE PREPARSER
;	reset function 10 flag
;	set up parser
;	convert to upper case
;
;	All input is converted to upper case and the pointer
;	"parsep" is set to the begining of the first non-blank
;	character string.  If the line begins with a ; or :, it
;	is treated specially:
;
;		;	comment 	the line is ignored
;		:	conditional	the line is ignored if a fatal
;					error occured during the previous
;					command, otherwise the : is 
;					ignored
;
;	An exclamation point is used to separate multiple commands on a 
;	a line.  Two adjacent exclaimation points translates into a single 
;	exclaimation point in the command tail for compatibility.
;------------------------------------------------------------------------
;
;
uc:
	call	resetccpflg
	xchg			;DE = .SCB
	xra	a
	sta	option		;zero option flag
	lxi	h,cbuf
	call	skps1		;skip leading spaces/tabs
	xchg
	cpi	';'		;HL = .scb
	rz
	cpi	'!'
	jz	uc0
	cpi	':'
	jnz	uc1
	mvi	l,prog$ret$code
	inr	m
	inr	m		;was ^C typed? (low byte 0FEh)
	jz	uc0		;successful, if so
	inx	h
	inr	m		;is high byte 0FFh?
	rz			;skip command, if so
uc0:	inx	d		;skip over 1st character
uc1:	xchg			;HL=.command line
	shld	parsep		;set parse pointer to beginning of line
uc3:	mov	a,m		;convert lower case to upper
	cpi	'['
	jnz	uc4
	sta	option		;'[' is the option delimiter => command option
uc4:	cpi	'a'
	jc	uc5
	cpi	'z'+1
	jnc	uc5
	sui	'a'-'A'
	mov	m,a
uc5:
	if multi
	cpi	'!'
	cz	multistart	;HL=.char, A=char
	endif
	inx	h		;advance to next character
	ora	a		;loop if not end of line
	jnz	uc3
;
;	skip spaces
;	return with zero flag set if end of line
;
skps:	lhld	parsep		;get current position
skps1:	shld	parsep		;save position
	shld	errorp		;save position for error message
	mov	a,m
	ora	a		;return if end of command
	rz
	cpi	' '
	jz	skps2
	cpi	tab		;skip spaces & tabs
	rnz
skps2:	inx	h		;advance past space/tab
	jmp	skps1		;loop
;
;-----------------------------------------------------------------------
;
;	MULTIPLE COMMANDS PER LINE HANDLER
;
;-----------------------------------------------------------------------
	if multi

multistart:
	;
	;	A  = current character in command line
	;	HL = address of current character in command line
	;
	;double exclaimation points become one
	mov	e,l
	mov	d,h
	inx	d
	ldax	d
	cpi	'!'		;double exclaimation points
	push	psw
	push	h
	cz	copy0		;convert to one, if so
	pop	h
	pop	psw
	rz
	;we have a valid multiple command line
	mvi	m,0		;terminate command line here
	xchg
	;multiple commands not allowed in submits
	;NOTE: submit unravels multiple commands making the
	;following test unnecessary.  However, with GET[system]
	;or CP/M 2.2 SUBMIT multiple commands will be posponed 
	;until the entire submit completes...  
;	call	subtest		;submit active
;	mvi	a,0		
;	rnz			;return with A=0, if so
	;set up the RSX buffer
	lhld	osbase		;get high byte of TPA address
	dcr	h		;subtract 1 page for buffer
	mvi	l,endchain	;HL = RSX buffer base-1
	mov	m,a		;set end of chain flag to 0
	push	h		;save it 
multi0:	inx	h
	inx	d
	ldax	d		;get character from cbuf
	mov	m,a		;place in RSX
	cpi	'!'
	jnz	multi1
	mvi	m,cr		;change exclaimation point to cr
multi1:	ora	a
	jnz	multi0
	mvi	m,cr		;end last command with cr
	inx	h
	mov	m,a		;terminate with a zero
	;set up RSX prefix
	mvi	l,6		;entry point
	mvi	m,jmp		;put a jump instruction there
	inx	h
	mvi	m,9		;make it a jump to base+9 (RSX exit)
	inx	h
	mov	m,h	
	inx	h		;HL = RSX exit point
	mvi	m,jmp		;put a jump instruction there
	mvi	l,warmflg	;HL = remove on warm start flag
	mov	m,a		;set (0) for RSX to remain resident
	mov	l,a		;set low byte to 0 for fixchain
	xchg			;DE = RSX base
	call	fixchain	;add the RSX to the chain
	;save buffer address
	lhld	scbaddr
	mvi	l,ccpconbuf	;save buffer address in CCP conbuf field
	pop	d		;DE = RSX base
	inx	d
	mov	m,e
	inx	h
	mov	m,d
	mvi	l,multi$rsx$pg
	mov	m,d		;save the RSX base
	xra	a		;zero in a to fall out of uc
	ret
	;
	;
	;	save the BDOS conbuffer address and
	;	terminate RSX if necessary.
	;
multisave:
	lxi	d,conbuffer*256+ccpconbuf
	call	wordmov		;first copy conbuffer in case SUBMIT 
	ora	a		;and/or GET are active
	lxi	d,conbuffl*256+ccpconbuf
	cz	wordmov		;if conbuff is zero then conbufl has the 
	push	h		;next address
	call	break1
	pop	h		;H = SCB page
	mvi	l,ccpconbuf
	jnz	multiend
	mov	e,m
	inx	h
	mov	d,m		;DE = next conbuffer address
	inr	m
	dcr	m		;is high byte zero? 
	dcx	h		;HL = .ccpconbuf
	jz	multiend	;remove multicmd RSX if so
	ldax	d		;check for terminating zero
	ora	a
	rnz			;return if not
	;
	;	we have exhausted all the commands
multiend:
	;	HL = .ccpconbuf
	xra	a
	mov	m,a		;set buffer to zero
	inx	h
	mov	m,a
	mvi	l,multi$rsx$pg
	mov	h,m
	mvi	l,0eh		;HL=RSX remove on warmstart flag
	dcr	m		;set to true for removal
	jmp	rsx$chain	;remove the multicmd rsx buffer

	endif
;;
;************************************************************************
;
;	FILE NAME PARSER
;
;************************************************************************
;
;
;
;	get file name (read in if none present)
;
;
;;	The file-name parser in this CCP implements
;;	a user/drive specification as an extension of the normal
;;	CP/M drive selection feature.  The syntax of the
;;	user/drive specification is given below.  Note that a
;;	colon must follow the user/drive specification.
;;
;;	<a>:	<a> is an alphabetic character A-P specifing one
;;		of the CP/M disk drives.
;;
;;	<n>:	<n> is a decimal number 0-15 specifying one of the
;;		user areas.
;;
;;	<n><a>:	A specification of both user area and drive.
;;
;;	<a><n>:	Synonymous with above.
;;
;;	Note that the user specification cannot be included
;;	in the parameters of transient programs or precede a file
;;	name.  The above syntax is parsed by gcmd (get command).
;;
;; ************************************************************

getfn:
	if prompts
	lxi	d,fnmsg
getfn0:
	call	getprm
	endif
gfn:	lxi	d,dfcb
gfn0:	call	skps		;sets zero flag if eol
	push	psw
	call 	gfn2
	pop	psw
	ret
	;
	;	BDOS FUNCTION 152 INTERFACE
	;
	;entry:	DE = .FCB
	;	HL = .buffer
	;flags/A reg preserved
	;exit:  DE = .FCB
	;
	;
gfn2:	shld	parsep
	shld	errorp
	push	d		;save .fcb
	lxi	d,pfncb
	mvi	c,parsef
if func152
	call	bdos
else
	call	parse
endif
	pop	d		;.fcb
	mov	a,h
	ora	l		;end of command? (HL = 0)
	mov	b,m		;get delimiter
	inx	h		;move past delimiter
	jnz	gfn3
	lxi	h,zero+2	;set HL = .0
gfn3:	mov	a,h
	ora	l		;parse error? (HL = 0ffffh)
	jnz	gfn4
	lxi	h,zero+2
	call	perror		
gfn4:	mov	a,b
	cpi	'.'
	jnz	gfn6
	dcx	h
gfn6:	shld	parsep		;update parse pointer
gfnpwd:	mvi	c,16
	lxi	h,pfcb
	push	d
	call	move
	lxi	d,passwd	;HL = .disk map in pfcb
	mvi	c,10
	call	move		;copy to passwd
	pop	d		;HL = .password len
	mov	a,m
zero:	lxi	h,0		;must be an "lxi h,0"
	ora	a		;is there a password?
	mov	b,a
	jz	gfn8
	lhld	errorp		;HL = .filename
gfn7:	mov	a,m
	cpi	';'
	inx	h
	jnz	gfn7
gfn8:	ret			;B = len, HL = .password

;
;	PARSE CP/M 3 COMMAND
;	entry:	DE  = .UFCB  (user no. byte in front of FCB)
;		PARSEP = .command line
gcmd:
	push	d
	xra	a
	stax	d		;clear user byte
	inx	d
	stax	d		;clear drive byte
	inx	d
	call	skps		;skip leading spaces
;
;	Begin by looking for user/drive-spec.  If none if found,
;	fall through to main file-name parsing section.  If one is found
;	then branch to the section that handles them.  If an error occurs
;	in the user/drive spec; treat it as a filename for compatibility
;	with CP/M 2.2.  (e.g. STAT VAL: etc.)
;
	lhld	parsep		;get pointer to current parser position
	pop	d
	push	d		;DE = .UFCB
	mvi	b,4		;maximum length of user/drive spec
gcmd1:	mov	a,m		;get byte
	cpi	':'		;end of user/drive-spec?
	jz	gcmd2		;parse user/drive if so
	ora	a		;end of command?
	jz	gcmd8		;parse filename (Func 152), if so 
	dcr	b		;maximum user/drive spec length exceeded?
	inx	h
	jnz	gcmd1		;loop if not
	;
	;	Parse filename, type and password
	;
gcmd8:
	pop	d
	xra	a
	stax	d		;set user = default
	lhld	parsep
gcmd9:	inx	d		;past user number byte
	ldax	d		;A=drive
	push 	psw
	call	gfn2		;BDOS function 152 interface
	pop	psw
	stax	d
	ret
	;
	;	Parse the user/drive-spec
	;
gcmd2:
	lhld	parsep		;get pointer to beginning of spec
	mov	a,m		;get character
gcmd3:	cpi	'0'		;check for user number
	jc	gcmd4		;jump if not numeric
	cpi	'9'+1
	jnc	gcmd4
	call	gdns		;get the user # (returned in B)
	pop	d
	push	d
	ldax	d		;see if we already have a user #
	ora	a
	jnz	gcmd8		;skip if we do
	mov	a,b		;A = specified user number 
	inr	a		;save it as the user-spec
	stax	d
	jmp	gcmd5
gcmd4:	cpi	'A'		;check for drive-spec
	jc	gcmd8		;skip if not a valid drive character
	cpi	'P'+1
	jnc	gcmd8
	pop	d
	push	d
	inx	d
	ldax	d		;see if we already have a drive
	ora	a
	jnz	gcmd8		;skip if so
	mov	a,m
	sui	'@'		;convert to a drive-spec
	stax	d
	inx	h
gcmd5:	mov	a,m		;get next character
	cpi	':'		;end of user/drive-spec?
	jnz	gcmd3		;loop if not
	inx	h
	pop	d		;.ufcb
	jmp	gcmd9		;parse the file name


;
;************************************************************************
;
;		TEMPORARY PARSE CODE
;
;************************************************************************
;
if not func152
;	version 3.0b  Oct 08 1982 - Doug Huskey
;
;

passwords	equ	true

parse:	; DE->.(.filename,.fcb)
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
	xchg
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
	call	pad
	call	skip
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
	ldax	d		;get character
	ani	5fh		;convert to upper case
	sui	'A'
	jc	perr1
	cpi	16
	jnc	perr1
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
if passwords
	lxi	b,7*256
else
	mvi	b,7
endif
parse6:	ldax	d		;get a character
	cpi	'.'		;file-type next?
	jz	parse$type	;branch to file-type processing
	cpi	';'
	jz	parsepw
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
if passwords
	lxi	b,2*256
else
	mvi	b,2
endif
parse8:	ldax	d
	cpi	';'
	jz	parsepw
	call	gfc		;process one character
	jnz	parse8		;loop if not end of type
;
parse$ok:
	pop	b
	push	d
	call	skip
	call	delim
	pop	h
	rnz
	lxi	h,0
	ora	a
	rz
	cpi	cr
	rz
	xchg
	ret
;
;	handle parser error
;
perr:
	pop	b			;throw away return addr
perr1:
	pop	b
	lxi	h,0ffffh
	ret
;
if passwords
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
else
;
;	skip over password
;
parsepw:
	inx	d
	call	delim
	jnz	parsepw
	jmp	parse$ok
endif
;
;	get next character of name, type or password
;
gfc:	call	delim		;check for end of filename
	rz			;return if so
	cpi	' '		;check for control characters
	inx	d
	jc	perr		;error if control characters encountered
	inr	b		;error if too big for field
	dcr	b
	jm	perr
if passwords
	inr	c
	dcr	c
	jnz	gfc1
endif
	cpi	'*'		;trap "match rest of field" character
	jz	setwild
gfc1:	mov	m,a		;put character in fcb
	inx	h
	dcr	b		;decrement field size counter
	ora	a		;clear zero flag
	ret
;;
setwild:
	mvi	m,'?'		;set match one character
	inx	h
	dcr	b
	jp	setwild
	ret
;
;	skip spaces
;
skip0:	inx	d
skip:	ldax	d
	cpi	' '		;skip spaces & tabs
	jz 	skip0
	cpi	tab
	jz	skip0
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
;	pad with blanks
;
pad:	mov	m,b
	inx	h
	dcr	c
	jnz	pad
	ret
;
endif
;
;
;************************************************************************
;
;	SUBROUTINES 
;
;************************************************************************
;
	if multi
;
;	copy SCB memory word
;	d = source offset e = destination offset
;
wordmov:
	lhld	scbaddr
	mov	l,d
	mov	d,h
	mvi 	c,2
;
	endif
;
;	copy memory bytes 
;	de = destination  hl = source  c = count
;
move:
	mov 	a,m 
	stax 	d 		;move byte to destination
	inx 	h 
	inx 	d		;advance pointers
	dcr 	c		;loop if non-zero
	jnz	move
	ret
;
;	copy memory bytes with terminating zero
;	hl = destination  de = source  
;	returns c=length

copy0:	mvi	c,0
copy1:	ldax	d
	mov	m,a
	ora	a
	mov	a,c
	rz
	inx	h
	inx	d
	inx	b
	jmp	copy1

;;
;;-----------------------------------------------------------------------
;;
;;	get byte from file
;;
;;	exit:	z  = set if byte gotten
;;		a  = byte read
;;		z  = clear if error or eof
;;		a  = return value of bdos read call
;;
getb:	xra	a		;clear accumulator
	lxi	h,bufp		;advance buffer pointer
	inr	m
	cm	read		;read sector if buffer empty
	ora	a
	rnz			;return if read error or eof
	lda	bufp		;compute pointer into buffer
	lxi	h,buf
	call	addhla
	xra	a		;set zero flag
	mov	a,m		;get byte
	ret
;;
;;-----------------------------------------------------------------------
;;
;;
;;	system control block flag routines
;;
;;	entry:	c  = bit mask (1 bit on)
;;		b  = scb byte offset
;;
subtest:
	lxi	b,submit
getflg:
;	return flag value
;	exit:	zero flag set if flag reset
;		c  = bit mask
;		hl = flag byte address
;
	lhld 	scbaddr 
	mov 	l,b
	mov 	a,m
	ana 	c 		; a = bit
	ret
;
setccpflg:
	lxi	b,ccp10

;
setflg:
;	set flag on (bit = 1)
;
	call 	getflg
	mov 	a,c
	ora 	m
	mov 	m,a
	ret
;
resetccpflg:
	lxi	b,ccp10
;
resetflg:
;	reset flag off (bit = 0)
;
	call 	getflg
	mov 	a,c
	cma 
	ana 	m 
	mov 	m,a
	ret
;;
;;
;;	SET/GET SCB BYTE
;;
;;	entry:	 A  = byte ("setbyte" only)
;;		 B  = SCB byte offset from page
;;
;;	exit:	 A  = byte ("getbyte" only)
;;
setbyte:
	lhld 	scbaddr 
	mov 	l,b 
	mov 	m,a
	ret
;
getbyte:
	lhld 	scbaddr 
	mov 	l,b 
	mov 	a,m
	ret
;



;;-----------------------------------------------------------------------
;;
;;
;;	print message followed by newline
;;
;;	entry:	de -> message string
;;
pmsgnl:	call	pmsg
;
;	print crlf
;
dirln:	mov	b,l			;number of columns for DIR
crlf:	mvi	a,cr
	call	pfc
	mvi	a,lf
	jmp	pfc
;;
;;-----------------------------------------------------------------------
;;
;;	print decimal byte
;;
pdb:	sui	10
	jc	pdb2
	mvi	e,'0'
pdb1:	inr	e
	sui	10
	jnc	pdb1
	push	psw
	call	putc2
	pop	psw
pdb2:	adi	10+'0'
	jmp	putc
;;-----------------------------------------------------------------------
;;
;;
;;	print string terminated by 0 or char in c
;;
pstrg:	mov	a,m		;get character
	ora	a
	rz
	cmp	c
	rz
	call	pfc		;print character
	inx	h		;advance pointer
	jmp	pstrg		;loop
;;
;;-----------------------------------------------------------------------
;;
;;	check for end of command (error if extraneous parameters)
;;
eoc:	call	skps
	rz
;
;	handle parser error
;
perror:
	lxi	h,errflg
	mov	a,m
	ora	a		;ignore error????
	mvi	m,0		;clear error flag
	rnz			;yes...just return to CCPRET
	lhld	errorp		;get pointer to what we're parsing
	mvi	c,' '
	call	pstrg
perr2:	mvi	a,'?'		;print question mark
	call	putc
	jmp	ccpcr
;
;;-----------------------------------------------------------------------
;;
;;
;;	print error message and exit processor
;;
;;	entry:	bc -> error message
;;
;;msgerr:	push	b
;;	call	crlf
;;	pop	d
;;	jmp	pmsgnl
;;
;;-----------------------------------------------------------------------
;;
;;	get decimal number (0 <= N <= 255)
;;
;;	exit:	a  = number
;;
gdn:	call	skps		;skip initial spaces
	lhld	parsep		;get pointer to current character
	shld	errorp		;save in case of parsing error
	rz			;return if end of command
	mov	a,m		;get it
	cpi	'0'		;error if non-numeric
	jc	perror
	cpi	'9'+1
	jnc	perror
	call	gdns		;convert number
	shld	parsep		;save new position
	ori	1		;clear zero and carry flags
	mov	a,b
	ret
;
gdns:	mvi	b,0
gdns1:	mov	a,m
	sui	'0'
	rc
	cpi	10
	rnc
	push	psw
	mov	a,b		;multiply current accumulator by 10
	add	a
	add	a
	add	b
	add	a
	mov	b,a
	pop	psw
	inx	h		;advance to next character
	add	b		;add it in to the current accumulation
	mov	b,a
	cpi	16
	jc	gdns1		;loop unless >=16
	jmp	perror		;error if invalid user number
;;
;;-----------------------------------------------------------------------
;;
;;	print file name
;;
	if newdir
pfn:	inx	d		;point to file name
	mvi	h,8		;set # characters to print, clear # printed
	call	pfn1		;print name field
	call	space
	mvi	h,3		;set # characters to print
pfn1:	ldax	d		;get character
	ani	7fh
	call	pfc		;print it if not
	inx	d		;advance pointer
	dcr	h		;loop if more to print
	jnz	pfn1
	ret
;
space:	mvi	a,' '
;
pfc:	push	b
	push	d
	push	h
	call	putc
	pop	h
	pop	d
	pop	b
	ret
	
	else

pfn:	inx	d		;point to file name
	lxi	b,8*256		;set # characters to print, clear # printed
	call	pfn1		;print name field
	ldax	d		;see if there's a type
	ani	7fh
	cpi	' '
	rz			;return if not
	mvi	a,'.'		;print dot
	call	pfc
	mvi	b,3		;set # characters to print
pfn1:	ldax	d		;get character
	ani	7fh
	cpi	' '		;is it a space?
	cnz	pfc		;print it if not
	inx	d		;advance pointer
	dcr	b		;loop if more to print
	jnz	pfn1
	ret
;
space:	mvi	a,' '
;
pfc:	inr	c		;increment # characters printed
	push	b
	push	d
	call	putc
	pop	d
	pop	b
	ret
	endif
;;
;;-----------------------------------------------------------------------
;;
;;	add a to hl
;;
addhla:	add	l
	mov	l,a
	rnc
	inr	h
	ret
;;
;;-----------------------------------------------------------------------
;;
;;	set match-any string into fcb
;;
;;	entry:	de -> fcb area
;;		b  = # bytes to set
;;
setmatch:
	mvi	a,'?'		;set match one character
setm1:	stax	d		;fill rest of field with match one
	inx	d
	dcr	b		;loop if more to fill
	jnz	setm1
	ora	a
	ret
;;
;;-----------------------------------------------------------------------
;;
;;	table search
;;
;;	Search table of strings separated by spaces and terminated 
;;	by 0.  Accept abbreviations, but set string = matched string
;;	on exit so that we don't try to execute abbreviation.
;;
;;	entry:	de -> string to search for
;;		hl -> table of strings to match (terminate table with 0)
;;	exit:	z  = set if match found
;;		a  = entry # (0 thru n-1)
;;		z  = not set if no match found
;;
tbls:	lxi	b,0ffh		;clear entry & entry length counters
tbls0:	push	d		;save match string addr
	push	h		;save table string addr
tbls1:	ldax	d		;compare bytes
	ani	7fh		;kill upper bit (so SYS + R/O match)
	cpi	' '+1		;end of search string?
	jc	tbls2		;skip compare, if so
	cmp	m
	jnz	tbls3		;jump if no match
tbls2:	inx	d		;advance string pointer
	inr	c		;increment entry length counter
	mvi	a,' '
	cmp	m
	inx	h		;advance table pointer
	jnz	tbls1		;continue with this entry if more
	pop	h		;HL = matched string in table
	pop	d		;DE = string address
	call	move		; C = length of string in table
	mov	a,b		;return current entry counter value
	ret
;
tbls3:	mvi	a,' '		;advance hl past current string
tbls4:	cmp	m
	inx	h
	jnz	tbls4
	pop	d		;throw away last table address
	pop	d		;DE = string address
	inr	b		;increment entry counter
	mvi	c,0ffh
	mov	a,m		;check for end of table
	sui	1
	jnc	tbls0		;loop if more entries to test
	ret
;
;************************************************************************
;************************************************************************
;
;************************************************************************
;
;	DATA AREA
;
;************************************************************************
;	;Note uninitialized data placed at the end (DS)
;
;
	if	prompts
enter:	db	'Enter $'
unmsg:	db	'User #: $'
fnmsg:	db	'File: $'
	else
unmsg:	db	'Enter User #: $'
	endif
nomsg:	db	'No File$'
required:
	db	' required$'
eramsg:
	db	'ERASE $'
confirm:
	db	' (Y/N)? $'
more:	db	cr,lf,cr,lf,'Press RETURN to Continue $'
	if	dayfile
userzero	db	'  (User 0)$'
	endif
;
;
;
	if 	newdir
anyfiles:	db	0	;flag for SYS or DIR files exist
dirfiles:	db	'NON-'
sysfiles:	db	'SYSTEM FILE(S) EXIST$'
	endif

errflg:	db	0		;parse error flag
	if multi
multibufl:
	dw	0		;multiple commands buffer length
	endif
scbadd:	db	scbad-pag$off,0
	;********** CAUTION FOLLOWING DATA MUST BE IN THIS ORDER *********
pfncb:				;BDOS func 152 (parse filename)
parsep:	dw	0		;pointer to current position in command
pfnfcb:	dw	pfcb		;.fcb for func 152
usernum:			;CCP current user
	db	0
chaindsk:
	db	0		;transient's current disk
disk:	db	0		;CCP current disk
subfcb:	db	1,'$$$     SUB',0
ccpend:				;end of file (on disk)
	ds	1
submod:	ds	1
subrc:	ds	1
	ds	16
subcr:	ds	1
subrr:	ds	2
subrr2:	ds	1

dircols:
	ds	1		;number of columns for DIR/DIRS
pgsize:	ds	1		;console page size
line:	ds	1		;console line #
pgmode:	ds	1		;console page mode
	;*****************************************************************
errorp:	ds	2		;pointer to beginning of current param.
errsav:	ds	2		;pointer to built-in command tail
bufp:	ds	1		;buffer pointer for getb
realdos:
	ds	1		;base page of BDOS
;
option:	ds	1		;'[' in line?
passwd:	ds	10		;password
ufcb:	ds	1		;user number (must procede fcb)
FCB:
	ds	1		; drive code
	ds	8		; file name
	ds	3		; file type
	ds	4		; control info
	ds	16		; disk map
fcbcr:	ds	1		; current record
fcbrr:	ds	2		; random record
pfcb:	ds	36		; fcb for parsing
;
;
;
;
; 	command line buffer
;
cbufmx:	ds	1
cbufl:	ds	1
cbuf:	ds	comlen
	ds	50h
stack:
ccptop: 		;top page of CCP
	end

