;	cp/m symbolic debugger main module
	title	'Symbolic Interactive Debugger (demon) 7/12/82'
;
;	copyright (c) 1976,1977,1982
;	Digital Research
;	box 579 Pacific Grove
;	California 93950
;
false	equ	0
true	equ	not false
isis2	equ	false	;true if running under is interface
debug	equ	false	;true if debugging in cp/m environment
reloc	equ	true	;true if relocation image
	if	debug
	org	8000h	;base if debugging
	else
	if	isis2
	org	0e500h
	else
	if	reloc	;building relocation image
	org	0000h	;base for relocation
	else
	org	0d000h	;testing in 64 k
	endif
	endif
	endif
;
;
modbas	equ	$	;base of assem/disassem/debug
	ds	680h	;space for disassem/assem module
demon	equ	$	;base of debugging monitor
disin	equ	modbas+3
bdose	equ	0005h	;primary bdos entry point
;
	if	isis2
bdos	equ	103h	;real bdos entry
pcbase	equ	3180h
spbase	equ	3180h
dstart	equ	107h	;start of debugger code
dbase	equ	dstart+2;start of loaded program
dnext	equ	dbase+2	;next free address
bdbase	equ	100h	;low bdos location
bdtop	equ	3180h	;high bdos location
	else
bdos	equ	modbas+1806h
bdbase	equ	bdos	;base of bdos
bdtop	equ	bdbase+0d00h	;top of bdos
pcbase	equ	100h	;default pc
spbase	equ	100h	;default sp
	endif
;
disen	equ	disin+3		;disassembler entry point
assem	equ	disen+3	;assembler entry point
dispc	equ	assem+3		;disassembler pc value
dispm	equ	dispc+2		;disassembler pc max value
dispg	equ	dispm+2		;disassembler page mode if non zero
psize	equ	12		;number of assembly lines to list with 'l'
csize	equ	64		;command buffer size
ssize	equ	50		;local stack size
pbsize	equ	8		;number of permanent breaks
pbelt	equ	4		;size of each perm break element
;
;	basic disk operating system constants
cif	equ	1
cof	equ	2
rif	equ	3
pof	equ	4
lof	equ	5
;
ids	equ	7
getf	equ	10	;fill buffer from console
chkio	equ	11	;check io status
lift	equ	12	;lift head on disk
opf	equ	15	;disk file open
DELF	equ	19	;delete file func
rdf	equ	20	;read disk file
WRITF	equ	21	;sequential write func
dmaf	equ	26	;set dma address
;
dbp	equ	5bh	;disk buffer pointer
dbf	equ	80h	;disk buffer address
dfcb	equ	5ch	;disk file control block
fcb	equ	dfcb
fcbl	equ	32	;length of file control block
fcb2	equ	fcb+16	;second file control block
fdn	equ	0	;disk name
ffn	equ	1	;file name
ffnl	equ	8	;length of file name
fft	equ	9	;file type
fftl	equ	3	;length of file type
frl	equ	12	;reel number
frc	equ	15	;record count
fcr	equ	32	;current record
fln	equ	fcbl+1	;fcb length including current rec
;
deof	equ	1ah	;control-z (eof)
eof	equ	deof	;eof=deof
tab	equ	09h	;horizontal tab
cr	equ	0dh
lf	equ	0ah
;
	if	debug
rstnum	equ	6	;use restart 6 for debug mode
	else
rstnum	equ	7	;restart number
	endif
rstloc	equ	rstnum*8	;restart location
rstin	equ	0c7h or (rstnum shl 3)	;restart instruction
;
;	template for programmed breakpoints
;		---------
;		pch : pcl
;		hlh : hll
;		sph : spl
;		ra  : flg
;		b   : c
;		d   : e
;		---------
;	flg field:  mz0i0e1c (minus,zero,idc,even,carry)
;
aval	equ	5	;a register count in header
bval	equ	6
dval	equ	7
hval	equ	8
sval	equ	9
pval	equ	10
;
;
;	demon entry points
TPATOP:
	jmp	trapad	;trap address for return in case interrupt
	jmp	begin
breaka:
	jmp	breakp
;	useful entry points for programs running with ddt
	jmp	getbuff	;get another buffer full
	jmp	gnc	;get next character
	jmp	pchar	;print a character from a
	jmp	pbyte	;print byte in register a
	jmp	paddsy	;print address/symbol reference
	jmp	scanexp	;scan 0,1,2, or 3 expressions
	jmp	getval	;get value to h,l
	jmp	break	;check for console ready
	jmp	prlabel	;print label given by hl, if it exists
;
;
trapad:	;get the return address for this jump to bdos in case of
;	a soft interrupt during bdos processing.
	xthl	;pc to hl
	shld	retloc	;may not need it
	xthl
trapjmp:
;	address field of the following jump is set at "begin"
	jmp	0000h
;
begin:
;	set the bdos entry address to reflect the reduced memory
;	size, as well as to trap the calls on the bdos.  upon
;	entry to "begin" the memory addresses are set as follows-
;		bdose:	jmp	bdos
;		modbas:	jmp	begin
;		demon:	jmp	trapad
;		trapad:	...
;		trapjmp:jmp	xxxx
;		begin:	...
;		bdose:	bdos	(or next module)
;
;	change the memory map to appear as follows-
;		bdose:	jmp	modbas
;		modbas:	jmp	trapad
;		demon:	jmp	trapad
;		trapad:	...
;		trapjmp:jmp	bdos
;			...
;		bdos:	bdos	(or next module)
;
;	note that we do not assume that the next module above
;	the debugger is the bdos.  in fact, the next module up may
;	be another copy of the debugger itself.
;
	lhld	bdose+1	;address of next module in memory
	shld	trapjmp+1;change jump instruction address in trap code
	lxi	h,trapad;address of trap code
	shld	modbas+1	;change address field of jump at beginning
	lxi	h,modbas	;base of dis/assembler code
	shld	bdose+1	;change primary bdos entry address
	shld	sytop		;mark symbol table empty
;
;	note that -a will change the bdose jump address to
;	the base of the debugger module only, which removes the
;	dis/assembler from the memory image.
;	"a-" is implied if the load address exceeds modbas.
;
	if	isis2
	pop	h	;recall return address to is.com
	shld	dbase	;set up as base of program
	lxi	h,beginr;read beginning of ddt
	shld	dstart;mark as debug mode
beginr:
	endif
	xra	a	;zero acc
	sta	breaks	;clears break point count
	sta	dasm	;00 in dasm marks dis/assembler present
	sta	pbtrace	;perm break trace set false
	sta	tmode	;trace mode cleared
;

	if	isis2
	lhld	dbase		;base address of program	
	else
	lxi	h,pcbase
	endif
	shld	dispc		;initial value for disassembler pc
	shld	disloc		;initial value for display
	shld	ploc		;pc in restart template
	if	isis2
	lxi	h,pcbase	;primary entry to ddt, no high addr
	endif
	shld	mload		;max load local
	shld	DEFLOAD
	lxi	h,spbase
	lxi	sp,stack-4
	push	h	;initial sp
	lxi	h,10b	;initial psw
	push	h
	dcx	h
	dcx	h	;cleared
	shld	hloc	;h,l cleared
	push	h	;b,c cleared
	push	h	;d,e cleared
	shld	userbrk	;clear user break during trace/untrace
;
	mvi	a,jmp	;(jmp restart)
	sta	rstloc
	lxi	h,breaka	;break point subroutine
	shld	rstloc+1	;restart location address field
;
;	check for file name passed to demon, and load if present
	lda	fcb+ffn	;blank if no name passed
	cpi	' '
	jz	start
;
;	use a zero bias and read
	lda	FCB+9		;is COM specified?
	cpi	' '		;blank if not
	jnz	DEFREAD		;read it in
;
	call	COMDEF
;
	lda	FCB+010h	;sym file location
	cpi	' '		;is it there?
	jz	DEFREAD		;jump over if no sym file
;
	lda	FCB+019h
	cpi	' '		;is the type specified?
	jnz	DEFREAD		;bypass if present
;
	call	SYMDEF		;insert .SYM file type
;
DEFREAD:	
	lxi	h,0
	jmp	readn
;
;
;	*********************************
;	*				*
;	*	main command loop	*
;	*				*
;	*********************************
;
start:
	lxi	sp,stack-12	;initialize sp in case of error
	call	break	;any active characters?
	mvi	c,cif	;console input function
	cnz	trapad	;to clear the character
	call	crlf	;initial crlf
	if	debug
	mvi	a,'@'
	else
	mvi	a,'#'
	endif
	call	pchar	;output prompt
;
;	get input buffer
	call	getbuff	;fill command buffer
;
	call	gnc	;get character
	cpi	cr
	jz	start
;	check for negated command
	lxi	h,negcom
	mvi	m,0
	cpi	'-'	;preceding "-"?
	jnz	poscom	;skip to positive command if not
;	negated command, mark by negcom=true
	dcr	m	;00 becomes ff
	call	gnc	;to read the command
poscom:
	sui	'A'	;legal character?
	jc	cerror	;command error
	cpi	'Z'-'A'+1
	jnc	cerror
;	character in register a is command, must be in the range a-z
	mov	e,a	;index to e
	mvi	d,0	;double precision index
	lxi	h,jmptab;base of table
	dad	d
	dad	d	;indexed
	mov	e,m	;lo byte
	inx	h
	mov	d,m	;ho byte
	xchg		;to h,l
	pchl	;gone...
;
jmptab:	;jump table to subroutines
	dw	assm	;a enter assembler language
	dw	cerror	;b
	dw	callpr	;c call program
	dw	display	;d display ram memory
	dw	EXECUTE	;e
	dw	fill	;f fill memory
	dw	goto	;g go to memory address
	dw	hexari	;h hexadecimal sum and difference
	dw	infcb	;i fill input file control block
	dw	cerror	;j
	dw	cerror	;k
	dw	lassm	;l list assembly language
	dw	move	;m move memory
	dw	cerror	;n
	dw	cerror	;o
	dw	permbrk	;p
	dw	cerror	;q
	dw	read	;r read hexadecimal file
	dw	setmem	;s set memory command
	dw	trace	;t
	dw	untrace	;u
	dw	VALUE	;v
	dw	WRITE	;w
	dw	examine	;x examine and modify registers
	dw	cerror	;y
	dw	cerror	;z
;

;
;	*********************************
;	*				*
;	*	a - assemble		*
;	*				*
;	*********************************
;
assm:	;assembler language input
;	check for assm present
	call	chkdis	;generate "no carry" if not there
	jnc	cerror	;not there
	call	scanexp	;read the expressions
	ora	a	;none given?
	jnz	assm0	;skip to check for single parameter
;
;	no parms, must be -a or a command
	lda	negcom	;must be set
	ora	a	;ff?
	jz	assm1	;use old dispc for base
	call	nodis	;remove disassembler
	jmp	start	;for another command
;
assm0:
	dcr	a	;one expression expected
	jnz	cerror
	call	getval	;get expression to h,l
	shld	dispc
assm1:	call	assem
	jmp	start

;
;	*********************************
;	*				*
;	*	c - call		*
;	*				*
;	*********************************
callpr:
;	call user program from ddt
	call	scanexp
	jc	cerror	;cannot be ,xxx
	jz	cerror	;cannot be c alone
	call	getval	;address to call in h,l
	push	h	;ready for call
;	get remaining parameters
;	reg-a contains 1,2,or 3 corresponding to number of values
	lxi	b,0
	dcr	a
	jnz	call0
;	no parameters, stack two zeroes
	push	b
	push	b
	jmp	call2
call0:	;at least one parameter
	call	getval
	push	h
	dcr	a
	jnz	call1
;	only one parameter, stack a zero
	push	b
	jmp	call2
call1:	;must be two parameters for the call
	call	getval
	push	h
call2:	;set up parameters in b,c and d,e
	pop	d	;recall second parameter
	pop	b	;recall first parameter
;	ready for the user program call
	lxi	h,start	;return address
	xthl		;call address in h,l return in stack
	pchl		;call user
;
;	*********************************
;	*				*
;	*	d - display RAM		*
;	*				*
;	*********************************
;
;	display memory, forms are
;	d		display from current display line
;	dnnn		set display line and assume d
;	dnnn,mmm	display nnn to mmm
;	new display line is set to next to display
display:
	call	scanword
	jz	disp1		;assume current disloc
	call	getval		;get value to h,l
	jc	disp0		;carry set if ,b form
	shld	disloc		;otherwise dispc already set
disp0:	;get next value
	ani	7fh		;in case ,b
	dcr	a
	jz	disp1		;set half page mode
	call	getval
	dcr	a		;a,b,c not allowed
	jnz	cerror
	jmp	DISP2		;store it
;
;
disp1:	
;0 or 1 expn, display half screen
	lhld	disloc
	lxi	d,psize*16-1
	dad	d
	jnc	DISP2		;this is O.K.
;
	lxi	h,0FFFFh	;end of RAM in this case
disp2:
	shld	dismax
;
;	display memory from disloc to dismax
disp3:	
;
	call	break		;break key?
	jnz	start		;stop current expansion
;
;
	lhld	DISMAX		;check for the end
	xchg			;DE=DISMAX
	lhld	disloc		;HL=current location
	shld	tdisp
	xchg			;get set for check
	call	HLDE		;are we done?
;	jz	START		;yes
	jc	START		;yes
				;no we have more
	call	CRLF		;next line
	lhld	DISLOC		;
	call	paddr		;print line address
	mvi	a,':'
	call	pchar	;to delimit address
	lda	wdisp	;word display?
	ora	a
	jz	disp4	;skip to byte display if not
;
	mvi	c,8	;display 8 items per line (double bytes)
;	full word display, get next value to de
word0:	call	blank	;blank delimiter
	mov	e,m	;low byte
	inx	h
	mov	d,m	;high byte
	inx	h	;ready for next address
	xchg		;hl is address
	call	paddr	;print the address value
	call	blank
	xchg		;back to DE with the address value
	dcr	c		;
	push	a		;save flags
	call	DISCOM
	jc	WORD1	
	pop	a		;restore flags
	jnz	word0	;for another item
	jmp	disch	;to display characters
;
WORD1:
	pop	a
WORD2:
	mov	a,c
	ora	c		;are we at the end of the line?
	jz	DISCH		;yes, branc to char print
				;no, continue
	call BLANK ! call BLANK	! call BLANK
	call BLANK ! call BLANK ! call BLANK
	dcr	c		;finished this char
	jnz	WORD2		;were not done yet
	jmp	DISCH
	
disp4:
	mvi	c,16		;counter
disp5:
	call	blank		;blank byte delimiter
	mov	a,m		;get next data byte
	call	pbyte		;print byte
	dcr	c		;decrement counter
	push	a		;save it
	inx	h
	xchg			;DE = current address
	lhld	DISMAX		;HL = top of ram
	call	HLDE
	xchg

;	jz	DISP6		;end of the line print blanks

	jc	DISP6		;go print the ending characters
	pop	a		;restore status
	jnz	DISP5		;print next byte
	jmp	DISCH
;
DISP6:
	pop	a
DISP7:
	mov	a,c
	ora	c		;are we at the end of the line?
	jz	DISCH		;yes, branc to char print
				;no, continue
	call	BLANK		;
	call	BLANK		;
	call	BLANK		;
	dcr	c		;finished this char
	jnz	DISP7		;were not done yet
;
;
;DISP7:
;	dcr	c		;to adjust the printer count
;	mov	a,c
;	ora	c
;	jz	DISP7

;	call	blank		;print the blank
;	mov	a,m		;print the last character
;	call	PBYTE		;
;	inx	h		;adjust the RAM pointer
;	dcr	c		;decrement counter
;	mvi	a,TRUE
;	sta	DISEND		;end flag
;
;
;
DISCH:	;display area in character form
	shld	disloc	;update for next write
	lda	negcom	;negated command?
	ora	a		;ff if negated
	jnz	DISP3		;to skip the character display
	lhld	tdisp
	xchg
	call	blank
	mvi	c,16		;set up loop counter
;
disch0:	ldax	d		;get byte
	call	pgraph		;print if graphic character
	inx	d
	lhld	DISMAX		;compare for end of line
	call	HLDE		;HL=disloc
	jz	DISP8		;we have reached the end
	jc	DISP3
	dcr	c		;16 characters?
	jnz	DISCH0		;no, do it again
	jmp	DISP3
;
DISP8:
	ldax	d		;get last character
	call	PGRAPH		;print it
;
	lda	DISEND
	cpi	TRUE		;
	jnz	DISP3		;we have finished 
	mvi	a,FALSE
	sta	DISEND
	jmp	START
;
;
;
;
;	*********************************
;	*				*
;	*	e - execute		*
;	*				*
;	*********************************
;
execute:
	lda	CURLEN
	ora	a
	jz	CERROR		;
;
EX1:
	call	FCBIN		;read in the FCBs
; Check for default
	lda	FCB+9
	cpi	' '
	jnz	EX2
	call	COMDEF
EX2:
	lda	FCB+019h
	cpi	' '
	jnz	EX3
	call	SYMDEF
EX3:
	lxi	h,0		;HL = BIAS for load into program
	jmp	readn		;now read it in
;
;
;	*********************************
;	*				*
;	*	f - fill		*
;	*				*
;	*********************************
;
fill:
	call	scan3	;expressions scanned bc , de , hl
	mov	a,h	;must be zero
	ora	a
	jnz	cerror
fill0:
	call	WRPCHK	;check for wrap
;
	jc	START	;back to start
	call	bcde	;end of fill?
	jc	start
	mov	a,l	;data
	stax	b	;to memory
	inx	b	;next to fill
	jmp	fill0
;
;	*********************************
;	*				*
;	*	g - goto		*
;	*				*
;	*********************************
;
goto:
	xra	a	;clear autou flag to indicate goto
	sta	autou	;autou=00 if goto, ff if tr/untr or perm brk
	call	crlf	;ready for go.
	call	scanexp	;0,1, or 2 exps
	sta	gobrks	;save go count
	call	getval
	push	h	;start address
	call	getval
	shld	gobrk1	;primary break point
	push	h	;bkpt1
	call	getval
	shld	gobrk2	;secondary break point
	mov	b,h	;bkpt2
	mov	c,l
	pop	d	;bkpt1
	pop	h	;goto address
	jmp	gopr1	;to skip autou=ff
;
gopr:
;	mark autou with ff to indicate trace/untrace or perm break
	push	h	;save go address
	lxi	h,autou	;00 if "go" ff if tr/untr/perm brk
	mvi	m,0ffh	;mark as tr/untr/perm brk
	pop	h	;recall go address
;
gopr1:	;arrive here from "goto" above with autou=00
	di
	jz	gop1	;no break points
	jc	gop0
;	set pc
	shld	ploc	;into machine state
gop0:	;set breaks
	ani	7fh	;clear , bit
	dcr	a	;if 1 then skip (2,3 if breakpoints)
	jz	gop1
	call	setbk	;break point from d,e
	dcr	a
	jz	gop1
;	second break point
	mov	e,c
	mov	d,b	;to d,e
	call	setbk	;second break point set
;
gop1:	;now check the permanent break points
;	scan the permanent break point table, forms are
;	count low(addr) high(addr) data
	lxi	h,pbtable
	mvi	c,pbsize	;number of elements
setper0:
	push	h		;save next table elt address
	mov	a,m		;low(count)
	ora	a		;00 if not in use
	jz	setper2		;skip if not
	inx	h		;to low(addr)
	mov	e,m
	inx	h		;to high(addr)
	mov	d,m		;de is the break address
	push	h		;save data address-1
;	may be continue from current perm break address
;	or a trace/untrace mode operation
	lda	autou		;00 if not
	ora	a		;set flags
	jz	setper1		;set the break point
;	this is a continuation from a perm break/or a trace/untrace
	lhld	ploc		;auto "u" necessary?
	mov	a,e		;low(addr)
	cmp	l		;=low(ploc)?
	jnz	setper1		;skip if not
	mov	a,d		;high(addr)
	cmp	h		;=high(ploc)?
	jnz	setper1		;skip if addr <> ploc
;
;	address match, set auto "u" command
	pop	h		;recall data address-1
	pop	h		;recall table address
	shld	pbloc		;table location for "u"
	push	h		;save for next iteration
	mov	a,m		;count
	mvi	m,0		;cleared in memory
	sta	pbcnt		;marks as auto u command necessary
	jmp	setper2		;to iterate
;
setper1:
	;break is not at current address
	pop	h		;recall data address-1
	inx	h		;.data
	ldax	d		;memory data
	mov	m,a		;saved in the table
	xchg			;memory addr to hl
	mvi	m,rstin		;set to restart instruction
setper2:
	pop	h		;recall table base
	lxi	d,pbelt		;element size
	dad	d		;incremented to next element
	dcr	c		;end of table?
	jnz	setper0		;for another element
;
gop2:	;permanent break points set, now start the program
	lxi	sp,stack-12
	pop	d
	pop	b
	pop	psw
	pop	h	;sp in hl
	sphl
	lhld	ploc	;pc in hl
	push	h	;into user's stack
	lhld	hloc	;hl restored
	ei
	ret
;
setbk:	;set break point at location d,e
	push	psw
	push	b
	lxi	h,breaks	;number of breaks set so far
	mov	a,m
	inr	m	;count breaks up
	ora	a	;one set already?
	jz	setbk0
;	already set, move past addr,data fields
	inx	h
	mov	a,m	;check = addresses
	inx	h
	mov	b,m	;check ho address
	inx	h
;	don't set two breakpoints if equal
	cmp	e	;low =?
	jnz	setbk0
	mov	a,b
	cmp	d	;high =?
	jnz	setbk0
;	equal addresses, replace real data
	mov	a,m	;get data byte
	stax	d	;put back into code
setbk0:	inx	h	;address field
	mov	m,e	;lsb
	inx	h
	mov	m,d	;msb
	inx	h	;data field
	ldax	d	;get byte from program
	mov	m,a	;to breaks vector
	mvi	a,rstin	;restart instruction
	stax	d	;to code
	pop	b
	pop	psw
	ret
;
;	*********************************
;	*				*
;	*	h - hex arithmetic	*
;	*				*
;	*********************************
;
hexari:
	call	scanexp
	jz	hexlist	;to list the symbol table
	call	getval	;ready the first value
	dcr	a	;1 becomes 0, 2 becomes 1
	jz	hexsym	;print the symbol only
	dcr	a	;2 became 1, now becomes 0
	jnz	cerror
;	first value is in hl
	push	h
	call	getval	;second value to h,l
	pop	d	;first value to d,e
	push	h	;save a copy of second vaalue
	call	crlf	;new line
	dad	d	;sum in h,l
	call	paddr
	call	blank
	pop	h	;restore second value
	xra	a	;clear accum for subtraction
	sub	l
	mov	l,a	;back to l
	mvi	a,0	;clear it again
	sbb	h
	mov	h,a
	dad	d	;difference in hl
	call	paddr
	jmp	start
;

hexsym:	;print symbol name
	xchg
	call	crlf	;new line for symbol
	push	d	;save de (address value) for ascii printout
	push	d	;save de for the decimal printout
	call	paddsy
;	print the value in decimal
	call	blank
	mvi	a,'#'
	call	pchar
;
	mvi	b,1 shl 7 or 5	;five digits, zero suppress on
	lxi	h,dtable	;decimal value table
;	initial/partial dividend is stacked at this point
nxtdig:	;convert first/next digit in dvalue table
	mov	e,m		;low order divisor
	inx	h		;to next value
	mov	d,m		;high order divisor
	inx	h		;ready for next digit
	xthl			;dividend to hl, dtable addr to stack
	mvi	c,'0'		;count c up while subtracting
hdig0:	mov	a,l		;low order dividend
	sub	e		;low order dividend
	mov	l,a		;partial difference
	mov	a,h		;high order dividend
	sbb	d		;high order divisor
	mov	h,a		;hl = hl - decade
	jc	hdig1		;carry gen'ed if too many subtracts
	inr	c		;to next ascii digit
	jmp	hdig0		;for another subtract
;
hdig1:	;counted down too many times
	dad	d		;add decade back
	mov	a,b		;check for zero suppress
	ora	a		;sign bit set?
	jp	hdig2		;skip if 0 bit set
	push	psw		;save the zero suppress / count
;	high order bit set, must be zero suppression
	mov	a,c		;check for ascii zero
	cpi	'0'
	jz	hdig3		;skip print if zero
;	digit is not zero, clear the zero suppress flag
	call	pchar
	pop	psw
	ani	7fh		;remove suppress flag
	mov	b,a		;back to b register
	jmp	hdig4		;to decrement the b register
;
hdig2:	;zero suppression not set, print the digit
	mov	a,c		;ready to print
	call	pchar		;printed to console
	jmp	hdig4		;to decrement the b register
;
hdig3:	;character is zero, suppression set
;	may be the last digit
	pop	psw		;recall digit count
	ani	7fh		;mask low bits
	cpi	1		;last digit?
	jnz	hdig4		;to decrement the b register
	mov	b,a		;clear zero suppression
	jmp	hdig2		;to print the character
;
hdig4:	;digit suppressed or printed, decrement count
	xthl			;dtable address to hl, partial to stack
	dcr	b		;count b down
	jnz	nxtdig		;for another digit
;
;	operation complete, remove partial result
	pop	d		;removed
	pop	d		;original value to de
;	print the character in ascii if graphic
	mov	a,d	;must be zero
	ora	a
	jnz	start	;skip the test
	mov	a,e	;character graphic?
	ani	7fh	;strip parity
	cpi	' '	;below space?
	jc	start	;skip if so
	inr	a	;7fh (rubout) becomes 00
	jz	start	;skip if so
	call	blank	;blank before quotes
	mvi	a,''''	;first quote
	call	pchar
	mov	a,e
	ani	7fh	;remove parity (again)
	call	pchar	;character
	mvi	a,''''
	call	pchar
	jmp	start
;
hexlist:
	;dump the symbol table to the console
	lhld	sytop	;topmost element
	inx	h	;to low address
	inx	h	;to high address
hexlis0:
	mov	d,m	;high address to d
	dcx	h	;move down to low
	mov	e,m	;low address to e
	dcx	h	;move down to length
	mov	c,m	;length  to c
	dcx	h	;to the first character
	mov	a,c	;to accumulator for compare
	cpi	16	;stop if length > 16
	jnc	start	;for the next instruction
;	otherwise, print the symbol
	call	crlf	;newline for symbol
	xchg		;symbol address to hl
	call	paddr	;address is printed
	xchg		;hl is the first symbol
	call	blank	;to print a blank after address
	inr	c	;in case c = 00
hexlis1:
	dcr	c	;count = count - 1
	jz	hexlis2	;skip to end of symbol if so
	mov	a,m	;character in a
	dcx	h	;to next symbol to get
	call	pchar	;to print the character
	jmp	hexlis1	;for another character
hexlis2:
	;end of symbol, carriage return line feed
	call	break
	jnz	start	;to skip the remainder
	jmp	hexlis0	;for another symbol


;
;	*********************************
;	*				*
;	*	i - input fcb		*
;	*				*
;	*********************************
infcb:
	lda	negcom	;negated?
	ora	a
	jnz	cerror	;command error if so
;
	call FCBIN
;

	jmp	start	;for another command
;
;	*********************************
;	*				*
;	*	l - list mnemonics	*
;	*				*
;	*********************************
;
lassm:
; assembler language output listing
;	l<cr> lists from current disassm pc for several lines
;	l<number><cr> lists from <number> for several lines
;	l<number>,<number> lists between locations
	call	chkdis	;disassm present?
	jnc	cerror
;
	call	scanexp	;scan expressions which follow
	jz	spage	;branch if no expressions
	call	getval	;exp1 to h,l
	shld	dispc	;sets base pc for list
	dcr	a	;only expression?
	jz	spage	;sets single page mode
;
;	another expression follows
	call	getval
	shld	dispm	;sets max value
	dcr	a
	jnz	cerror	;error if more expn's
	xra	a	;clear page mode
	jmp	spag0
;
spage:	mvi	a,psize	;screen size for list
spag0:	sta	dispg
	call	disen	;call disassembler
	jmp	start	;for another command
;

;
;	*********************************
;	*				*
;	*	m - move memory		*
;	*				*
;	*********************************
;
move:
	call	scan3	;bc,de,hl
move0:	;has b,c passed d,e?
	call	bcde
	jc	start	;end of move
; Check for wrap around
	push	b	;save state
	push	d
	push	h
	lxi	h,0FFFFh
	mov	a,h		;get high order
	cmp	b		;are they the same?
	jnz	MOVE1		;B < H so keep movin....
;
	mov	a,l		;B = H so check low order
	cmp	c		;set flags
	jnz	MOVE1
;
	jmp	START		;they are equal,BC = FFFFh do not wrap
MOVE1:
	pop	h
	pop	d
	pop	b		;restore registers
; Else continue
	ldax	b	;char to accum
	inx	b	;next to get
	mov	m,a	;move it to memory
	inx	h
	jmp	move0	;for another
;

;
;	*********************************
;	*				*
;	*	p - permanent break 	*
;	*				*
;	*********************************
permbrk:

	call	scanexp	;0,1, or 2 values
	jc	cerror	;p, not allowed
	jz	permzer	;no expressions
;	1 or 2 expressions found
	call	getval	;first value to hl (bp name)
	push	h	;saved to stack
	lxi	h,1	;set to one break if not there
	dcr	a	;item count
	lda	negcom	;ready negated command flag
	jz	setpval	;skip if 1 expression
	ora	a	;negated if ff
	jnz	cerror	;command error if form is -px,y
	call	getval	;may be zero, usually pass count
	jmp	setpval0
setpval:
	;only one expression, may be negated
	lxi	h,0
	ora	a	;negated if ff
	jnz	setpval0;to store the 00
	lxi	h,1	;otherwise the pass count is 1
setpval0:
	mov	a,h	;high byte must be zero
	ora	a	;00?
	jnz	cerror	;command error if not
;
	shld	bias		;held in bias
	lxi	h,pbtable;search for the stacked address
	mvi	c,pbsize
perm0:	push	h	;save current element
	mov	a,m	;is count=00?
	ora	a	;set flags
	jz	perm2
;	count is non-zero, may be current address
	inx	h	;low(addr)
	mov	a,m
	inx	h
	mov	d,m	;da is table address to compare
	pop	h	;table element base to hl
	xthl		;stacked search address to hl
	cmp	l	;low(addr) = low(search)?
	jnz	perm1	;skip if not
	mov	a,d
	cmp	h	;high(addr) = high(search)?
	jnz	perm1	;skip if addr <> search
;
;	found the address to operate upon
	lda	bias	;new count
	pop	h	;table element base to hl
	mov	m,a	;set to memory, may be zero
	ora	a
	jmp	start	;get next command
;
perm1:	xthl		;search address back to stack
	push	h	;table address back to stack
perm2:	pop	h	;table address revived
	lxi	d,pbelt	;element size
	dad	d	;hl is next to scan
	dcr	c	;count down table length
	jnz	perm0	;for another try
;
;	arrive here if item cannot be found, must be setting break
	lda	bias	;=00?
	ora	a	;set flags
	jz	cerror	;error if not found
;	search address is still stacked
;
;	setting non zero permanent pass count, find free entry
	lxi	h,pbtable
	mvi	c,pbsize
lperm0:	push	h	;save current table base
	mov	a,m	;get low(count)
	ora	a	;count=00?
	jnz	lperm1	;skip if in use
;	free location, use it
	lda	bias	;count in reg-a
	pop	h	;table base to hl
	mov	m,a	;non zero count set
	pop	d	;search address
	inx	h
	mov	m,e	;set low search
	inx	h
	mov	m,d	;set high search address
	jmp	start	;for another command
;
lperm1:	pop	h	;recall table base
	lxi	d,pbelt
	dad	d	;hl is next to scan
	dcr	c	;count table size down
	jnz	lperm0
;
;	no table space available
	jmp	cerror
;
;
permzer:
	;no expressions encountered, must be display or clear
	lxi	h,pbtable	;search for display or reset
	mvi	c,pbsize
permz0:	push	h		;save next table element addr
	mov	a,m		;count to a
	ora	a		;skip if zero count
	jz	permz2		;skip if inactive
;	display or clear
	lda	negcom		;-p?
	ora	a
	jz	permz1
;
;	this is a clear, so count = 00
	mvi	m,0		;clear count
	jmp	permz2		;to go to next item
;
permz1:	;this is a display
	push	b		;save pbtable count (c)
	call	crlf		;new line
	mov	a,m		;recall count to register a
	call	pbyte		;print byte
	call	blank		;blank delimiter
	inx	h		;low of address
	mov	e,m
	inx	h
	mov	d,m		;de is address of break point
	call	paddsy		;print symbol reference
	pop	b		;recall pbtable count in c
permz2:	pop	h		;recall table base
	lxi	d,pbelt		;element size
	dad	d		;to hl
	dcr	c		;count table down
	jnz	permz0		;for another
	jmp	start		;for a command
;
;	*********************************
;	*				*
;	*	r - read		*
;	*				*
;	*********************************
read:
	lda	CURLEN
	ora	a
	jz	CERROR		;no file after read command
;
	lxi	h,DFCB		;HL = default fcb
	call	GETFILE		;get filename
	mvi	m,00
	inx	h		;bump FCB pointer
	mvi	a,020h		;Blank in Acc
	mvi	c,11		;counter for file blank
r1:
	mov	m,a		;blank at mem
	inx	h
	dcr	c		;
	jnz	r1		;back if more
	mvi	a,00
	mvi	c,4		;
r2:	mov	m,a		;zero out rest of FCB
	inx	h
	dcr	c
	jnz	r2	
	mvi	m,0
;
	call	scanexp		;check for offset expression
	lxi	h,0		;HL = initial BIAS offset
	jz	readn		;if none to readn
	dcr	a		;one expression?
	jnz	cerror
	lhld	EXPLIST+1	;HL = new BIAS value
;
readn:	
;hl holds bias value for load operation
	shld	bias
;	copy the second half of the file control block to temp
	lxi	h,fcb2
	lxi	d,tfcb
	mvi	c,fcbl/2	;half of the fcb size
read0:	mov	a,m
	stax	d		;store to temp position
	inx	h
	inx	d
	dcr	c	;count to end of fcb
	jnz	read0
;	second half now saved, look at first name
	lda	fcb+1	;* specified?
	cpi	'?'
	jz	checksy	;skip load if so
rinit:	call	opn	;open input file
	cpi	255
	jz	cerror
;	continue if file open went ok
;	disk file opened and initialized
;	check for 'hex' file and load til eof
;
	lxi	h,PCBASE
	shld	DEFLOAD
	mvi	a,'H'	;hex file?
	lxi	b,'XE'	;remainder of name to bc
	call	qtype	;look for 'hex'
	lhld	bias	;recall bias value
	push	h	;save to mem for loader
	jz	hread
;
;	com/utl file, load with offset given by "bias"
	pop	h		;recall bias
	lxi	d,pcbase	;base of transient area
	dad	d
;	reg h holds load address
lcom0:	;load com file
	push	h	;save dma address
	lxi	d,dfcb
	mvi	c,rdf	;read sector
	call	trapad
	pop	h
	ora	a	;set flags to check return code
	jnz	checksy
;	move from 80h to load address in h,l
	lxi	d,dbf
	mvi	c,80h	;buffer size
lcom1:	ldax	d	;load next byte
	inx	d
	mov	m,a	;store next byte
	inx	h
	dcr	c
	jnz	lcom1
;	loaded, check address against mload
	call	ckmload
	call	CKDFLD
	xchg			;HL & DE correct
	lhld	BDOSE+1		;HL = top of memory
	call	HLDE		;is DMA address > base of SID?
	xchg
	jnc	LCOM0		;if so then error.
	lxi	h,PCBASE
	shld	DEFLOAD
	shld	MLOAD
	jmp	CERROR
;
;
;	otherwise assume hex file is being loaded
hread:	call	diskr	;next char to accum
	cpi	deof	;past end of tape?
	jz	cerror	;for another command
	sbi	':'
	jnz	hread	;looking for start of record
;
;	start found, clear checksum
	mov	d,a
	pop	h
	push	h
	call	rbyte
	mov	e,a	;save length
	call	rbyte	;high order addr
	push	psw
	call	rbyte	;low order addr
	pop	b
	mov	c,a
	dad	b	;biased addr in h
	mov	a,e	;check for last record
	ora	a
	jnz	rdtype
;	end of tape, set load address
	mov	a,b
	ora	c	;load address = 00?
	lxi	h,pcbase;default = pcbase if 0000
	jz	setpc
;	otherwise, pc at end of tape non zero
	mov	l,c	;low byte
	mov	h,b	;high byte
setpc:	shld	ploc	;set pc value
	jmp	checksy	;for symbol command
;
rdtype:
	call	rbyte	;record type = 0
;
;	load record
red1:	call	rbyte
	mov	m,a
	inx	h
	dcr	e
	jnz	red1	;for another byte
;	otherwise at end of record - checksum
	call	rbyte
	push	psw	;for checksum check
	call	ckmload	;check against mload
	call	CKDFLD
	pop	psw
	jnz	cerror	;checksum error
	jmp	hread	;for another record
;
rdhex:	;read one hex byte without accumulating checksum
	call	diskr	;get one character
rdhex0:	call	hexcon	;convert to hex
	rlc
	rlc
	rlc
	rlc		;moved to high order nibble
	ani	0f0h	;masked low order to 0000
	push	psw	;and stacked
	call	diskr	;get second character
	call	hexcon	;converted to hex in accum
	pop	b	;old accum to register b
	ora	b	;and'ed into result
	ret
;
rbyte:	;read one byte from buff at wbp to reg-a
;	compute checksum in reg-d
	push	b
	push	h
	push	d
;
	call	rdhex	;read one hex value
	mov	b,a	;value is now in b temporarily
	pop	d	;checksum
	add	d	;accumulating
	mov	d,a	;back to cs
;	zero flag remains set
	mov	a,b	;bring byte back to accumulator
	pop	h
	pop	b	;back to initial state with accum set
	ret
;
checksy:
;	check for dis/assem overload
	lxi	h,modbas
	call	comload	;hl > mload? carry if so
	jc	chksym	;no dis/assem overlay
	lda	dasm	;00 if present
	ora	a
	cz	nodis	;remove if not already
;
chksym:	;check for symbol table file
;	first save utl condition, if present
	mvi	a,'U'	;first character of utl
	lxi	b,'LT'	;remainder of name
	call	qtype	;find the file type - may be utl
	push	psw	;save condition for below
	lxi	h,tfcb	;name held here
	lxi	d,fcb	;source file control block
	mvi	c,fcbl/2
chksy0:	mov	a,m	;get character
	stax	d	;save into fcb
	inx	h
	inx	d	;pointers to next chars
	dcr	c
	jnz	chksy0
;
;	fcb filled with second file name, clear cr field
	xra	a
	sta	fcb+fcr
	lda	fcb+1
	cpi	' '
	jz	prstat	;skip if no file name
;
;	symbol load follows
	lxi	h,symsg	;write ''symbols'
	call	prmsg		;print the message
;	bias value is stored in "bias"
	call	opn	;open the symbol file
	inr	a	;255 becomes 00
	jz	cerror	;cannot open?
;	file opened, load symbol table from file
;
;	symbol table load routine - load elements of the
;	form -
;		(cr/lf/tab)hhhh(space)aaaaa(tab/cr)
;	where hhhh is the hex address, aaaaa is a list of
;	characters of length <16.  add bias address to each loc'n
;
loadsy:	call	diskr	;get next starting character
loadsy0:
	cpi	eof
	jz	prstat	;completes the load
	cpi	' '+1	;graphic?
	jc	loadsy	;until graphic found
;
;	get the symbol address to hl
	call	rdhex0	;pre-read first character
	push	psw	;high order byte saved
	call	rdhex	;second half
	pop	d	;high order byte goes to d
	mov	e,a	;low order byte to e
	lhld	bias	;bias value in r command
	dad	d	;hl is offset address
	push	h	;save the address for later
	call	diskr	;get the blank char
	cpi	' '
	jz	okload	;ok to load symbol if blank
;
;	clear to the next non graphic character
	pop	h	;throw out the load address
skload:
	;skip to non graphic character
	call	diskr	;read the next character
	cpi	' '	;below space if non graphic
	jc	loadsy0	;for the next character test
	jmp	skload	;to bypass another character
;
okload:
	lhld	bdose+1	;pointer to topmost jmp xxx around table
	mvi	e,0	;counts the symbol length
loadch:	;load characters
	dcx	h	;next to fill
	call	diskr	;next char to a
	cpi	tab	;end of symbol?
	jz	syend
	cpi	cr	;may be end of line
	jz	syend
	cpi	' '+1	;graphic?
	jc	cerror	;it must be
	mov	m,a	;save it in memory
	inr	e	;count the length up
	mov	a,e	;past 16?
	cpi	16
	jnc	cerror	;error if longer than 16 chars
	jmp	loadch	;for another character
;
syend:	;end of current symbol, set pointers for this one
;	structure is:
;		high bdos
;		low bdos
;	bjump:	jmp
;		...
;		high bjump
;		low bjump
;	bdose:	jmp
;
;	constructing symbol below bjump of the form
;		high addr
;		low addr
;	bjump:	length
;		char1
;		...
;		char length
;
;	then move jmp bdos down below the symbol
;
	push	d	;save the length
	push	h	;save the next to fill
	xchg		;de contains the next to fill
	lhld	bdose+1	;address of the jmp xxx above symbol
	inx	h	;low jump address
	mov	e,m	;to e for now
	inx	h	;high jump address
	mov	d,m	;de is the xxx for the jmp xxx to install
	pop	h	;next to fill address
	mov	m,d	;high order address
	dcx	h	;.low address
	mov	m,e	;xxx filled below symbol
	dcx	h	;.jmp
	mvi	m,jmp	;jump instruction filled
;	hl address the base of the table, ensure not below mload
	call	comload	;hl > mload ?
	jnc	cerror	;cy if so
	xchg		;jmp xxx address to de
	lhld	bdose+1	;previous jmp xxx address
	xchg		;to de, hl is new jmp xxx address
	shld	bdose+1	;changed jump address in low mem
	xchg		;old jump address back to hl
	pop	d	;length is in e
	mov	m,e	;stored to memory
	inx	h	;low address location
	pop	d	;low address in de
	mov	m,e
	inx	h	;high address location
	mov	m,d
;	now ready for another symbol
	jmp	loadsy
;
;	end of the symbol load subroutine
prstat:	;print the statistics for the load or start utility
	pop	psw	;zero flag set if this is a utility
	jnz	prstat0	;skip if not utility
;
;	this is a ddt utility, start it
	lxi	h,retutl	;return address from utility
	push	h	;to stack
	lhld	ploc	;probably = pcbase
	pchl		;gone to the utility ...
;
retutl:
	;return here to reset the symbol table base
	lhld	bdose+1	;new base of modules
	dad	d	;de is length of symbols inserted by utility
	shld	sytop	;new symbol top
	jmp	start	;for another command
;
;
prstat0:
;	not a ddt utility, print statistics
	lxi	h,lmsg	;'next  pc  end'
	call	prmsg	;printed to console
	lhld	DEFLOAD	;default load address
	call	PADDR
	call	BLANK
	lhld	mload	;next address
	call	paddr
	call	blank	;following blank
	lhld	ploc	;pc value
	call	paddr
	call	blank	;next and pc printed
	lhld	bdose+1	;end of memory+1
	dcx	h	;real end of memory
	call	paddr
	jmp	start	;for the crlf
;

;
;
;	*********************************
;	*				*
;	*	s - set memory 		*
;	*				*
;	*********************************
;
setmem:	;one expression expected
	call	scanword	;sets flags
	dcr	a	;one expression only
	jnz	cerror
	call	getval	;start address is in h,l
setm0:	call	crlf	;new line
	push	h	;save current address
	call	paddr	;address printed
	call	blank	;separator
	pop	h	;get data
	push	h	;save address to fill
;	check for display mode
	lda	wdisp
	ora	a	;word mode?
	jz	setbyte
;	set words of memory
	mov	e,m	;low order byte
	inx	h
	mov	d,m	;high order byte
	xchg
	call	paddr	;address value printed
	jmp	setget	;get value from input
;
setbyte:
;	byte mode set
	mov	a,m
	call	pbyte	;print byte
setget:	call	blank	;another separator
	call	getbuff	;fill input buffer
	call	gnc	;may be empty (no change)
	pop	h	;restore address to fill
	cpi	cr
	jz	setm1
	cpi	'.'
	jnz	chkasc	;skip to check ascii
;	must be length zero (otherwise .symbol)
	lda	curlen
	ora	a
	jz	start		;for next command
	mvi	a,'.'		;otherwise restore
chkasc:
	cpi	'"'	;ascii input?
;	filling ascii/ byte/ address data
	push	h	;save address to fill
	jnz	sethex	;hex single or double precision
;	set ascii data to memory
setasc:	call	gnlc	;next byte to fill
	pop	h	;next address to fill
	cpi	cr	;end of line
	jz	setm0	;for next input
	mov	m,a	;otherwise store it
	inx	h	;to next address to fill
	push	h	;save the address
	jmp	setasc
;
;	byte or address data is being changed
sethex:
	call	scanex	;first character already scanned
	dcr	a	;one item?
	jnz	cerror	;more than one
	call	getval	;value to h,l
	lda	wdisp	;word mode?
	ora	a	;word mode=ff
	jz	setbyt0
;	filling double precision value
	xchg		;value to de
	pop	h	;recall fill address
	mov	m,e	;low order
	inx	h	;addressing high order position
	mov	m,d	;filled
	inx	h	;move to next address
	jmp	setm0	;for the next address
;
;	filling byte value
setbyt0:
	ora	a	;high order must be zero
	jnz	cerror	;data is in l
	mov	a,l
	pop	h	;restore data value
	mov	m,a
setm1:	inx	h	;next address ready
	lda	wdisp
	ora	a	;word mode?
	jz	setm0	;skip inx if so
	inx	h	;to next double word
	jmp	setm0
;
;	*********************************
;	*				*
;	*	u - untrace mode	*
;	*				*
;	*********************************
;
untrace:
	mvi	a,1	;untrace mode = 1
	jmp	etrace
;
;	*********************************
;	*				*
;	*	t - start trace		*
;	*				*
;	*********************************
;
trace:	mvi	a,2	;set trace mode flag=2
etrace:
	sta	tmode
;	allow tw/uw to suppress out-of-line trace
	call	scanword
	lxi	h,0
	shld	userbrk		;clear userbrk
	inx	h		;default to one trace
	jz	trac0
;	expressions were given, forms are
;	tx	trace for x steps	acc = 1
;	tx,brk	trace for x steps, call "brk" at each stop   acc=2
;	t,brk	call "brk"		acc = 1, cy = 1
;
	jc	settr0
	call	getval	;to h,l
	push	psw
	mov	a,l	;check for zero
	ora	h
	jz	cerror
	pop	psw	;recall number of parameters
settr0:	;h,l contains trace count, save it for later
	push	h
;	look for break address
	dcr	a	;if only one specified, then skip userbrk
	jz	settr1
	dcr	a	;must be two values
	jnz	cerror	;more than two specified
	call	getval	;value to h,l
	shld	userbrk
settr1:	;recall trace count
	pop	h
trac0:	shld	tracer
	xra	a	;00 to accum
	sta	gobrks	;mark as no user breaks
	call	dstate	;starting state is displayed
	jmp	gopr	;sets breakpoints and starts execution
;
;	*********************************
;	*				*
;	*	v - value		*
;	*				*
;	*********************************
;
VALUE:
	jmp	PRSTAT0
;
;
;	*********************************
;	*				*
;	*	w - write		*
;	*				*
;	*********************************
;
WRITE:
	lda	CURLEN
	ora	a
	jz	CERROR		;exit if no file present
;
;
	lxi	h,FCB		;load HL with fcb address
	call	GETFILE		;obtain file from command string
	mvi	a,00h
	sta	FCB+32		;zero out the record count
	lxi	h,0100h
	shld	WBEGIN		;store begining address
	lhld	DEFLOAD		;get default end address
	shld	WEND		;store in Write END
;
	call	SCANEXP		;check for specified address
	lda	EXPLIST		;get number of experessions
	ora	a		;
	jz	NOWRPRM
;
	cpi	2
	jnz	CERROR		;error if not two expr
	lhld	EXPLIST+1	;HL = start address
	shld	WBEGIN		;store in begin
	lhld	EXPLIST+3	;HL = finish address
	shld	WEND		;store in end
;
; Continue with WRITE
NOWRPRM:
;
	lhld	WBEGIN		;HL = beginning address
	call	CHKEND		;is end > begin ?
	jc	CERROR		; if so error
;
	lxi	h,00h		;get ready to zero out
	shld	WRTREC		;# of records written

; Now that FCB is set up get ready to write out 
; to the specified file.
;
	lxi	d,DFCB
	call	DELETE
;
	call	MAKE
	inr	a
	jz	CERROR
	lhld	WBEGIN		;get beginning address
;
WLOOP0:
	call	WFLAG
	lxi	d,DBF		;DE = default DMA address
	mvi	c,80h		;counter for loop
;
WLOOP1:
	mov	a,m		;get byte
	inx	h		;bump pointer
	stax	d		;store in buffer
	inx	d		;bump pointer
	dcr	c		;decrement counter
	jnz	WLOOP1		;again if not finished
;
	lxi	d,DFCB
	call	DWRITE		;write it out
	ora	a		;set flags for write check
	jnz	CERROR		;error if not 0
	push	h		;save source address
	lhld	WRTREC		;get # of records written
	inx	h		;bump it by one
	shld	WRTREC		;put it back
	pop	h		;get source address back
;
	call	CHKEND
;
	lda	ONEFLG		;set for flag check
	cpi	TRUE		;last record?
	jnz	WLOOP0		;next record if not finished
WCLOSE:
	lxi	d,DFCB
	call	CLOSE
;
	lxi	h,WRTMSG
	call	PRMSG
	lhld	WRTREC		;# of records
	call	PADDR
	lxi	h,WRTMSG1
	call	PRMSG		;print out end of string
;
	jmp	START		;exit
;
CHKEND:
	lda	WEND		;get high order end byte
	sub	l	;get low order
	sta	rslt	;low order in rslt
	lda	WEND+1	;high order equal check
	sbb	h	;sub high order
	sta	rslt+1	;high order answer
	ret
;
WFLAG:
	mvi	a,FALSE	;zero out flag
	sta	ONEFLG	;store
	lda	RSLT+1
	cpi	00h
	rnz
	lda	RSLT
	cpi	080h	;record length
	jc	WFLAG1
	jz	WFLAG1
	ret
WFLAG1:
	mvi	a,TRUE
	sta	ONEFLG
	ret
;
ONEFLG:	db	0
RSLT:	dw	0
;
;	*********************************
;	*				*
;	*	x - examine 		*
;	*				*
;	*********************************
;
examine:
	call	gnc	;cr?
	cpi	cr
	jnz	exam0
	call	dstate	;display cpu state
	jmp	start
;
exam0:	;register change operation
	lxi	b,pval+1	;b=0,c=pval (max register number)
;	look for register match in rvect
	lxi	h,rvect
exam1:	cmp	m	;match in rvect?
	jz	exam2
	inx	h	;next rvect
	inr	b	;increment count
	dcr	c	;end of rvect?
	jnz	exam1
;	no match
	jmp	cerror
;
exam2:	;match in rvect, b has register number
	call	gnc
	cpi	cr	;only character?
	jnz	cerror
;
;	write contents, and get another buffer
	push	b	;save count
	call	crlf	;new line for element
	call	delt	;element written
	call	blank
	call	getbuff	;fill command buffer
	call	scanexp	;get input expression
	ora	a	;none?
	jz	start
	dcr	a	;must be only one
	jnz	cerror
	call	getval	;value is in h,l
	pop	b	;recall register number
;	check cases for flags, reg-a, or double register
	mov	a,b
	cpi	aval
	jnc	exam4
;	setting flags, must be zero or one
	mov	a,h
	ora	a
	jnz	cerror
	mov	a,l
	cpi	2
	jnc	cerror
;	0 or 1 in h,l registers - get current flags and mask position
	call	flgshf
;	shift count in c, d,e address flag position
	mov	h,a	;flags to h
	mov	b,c	;shift count to b
	mvi	a,0feh	;111111110 in accum to rotate
	call	lrotate	;rotate reg-a left
	ana	h	;mask all but altered bit
	mov	b,c	;restore shift count to b
	mov	h,a	;save masked flags
	mov	a,l	;0/1 to lsb of accum
	call	lrotate	;rotated to changed position
	ora	h	;restore all other flags
	stax	d	;back to machine state
	jmp	start	;for another command
;
lrotate:	;left rotate for flag setting
;	pattern is in register a, count in register b
	dcr	b
	rz	;rotate complete
	rlc	;end-around rotate
	jmp	lrotate
;
exam4:	;may be accumulator change
	jnz	exam5
;	must be byte value
	mov	a,h
	ora	a
	jnz	cerror
	mov	a,l	;get byte to store
	lxi	h,aloc	;a reg location in machine state
	mov	m,a	;store it away
	jmp	start
;
exam5:	;must be double register pair
	push	h	;save value
	call	getdba	;double address to hl
	pop	d	;value to d,e
	mov	m,e
	inx	h
	mov	m,d	;altered machine state
	jmp	start
;
diskr:	;disk read
	push	h
	push	d
	push	b
;
rdi:	;read disk input
	lda	dbp
	ani	7fh
	jz	ndi	;get next disk input record
;
;	read character
rdc:
	mvi	d,0
	mov	e,a
	lxi	h,dbf
	dad	d
	mov	a,m
	cpi	deof
	jz	RRET	;end of file
	lxi	h,dbp
	inr	m
	ora	a
	jmp	rret
;
ndi:	;next buffer in
	mvi	c,rdf
	lxi	d,dfcb
	call	trapad
	ora	a
	jnz	def
;
;	buffer read ok
	sta	dbp	;store 00h
	jmp	rdc
;
def:	;store EOF and return (end file)
	mvi	a,DEOF
rret:
	pop	b
	pop	d
	pop	h
	ret
;
;	*********************************
;	*				*
;	*	ERROR ROUTINES		*
;	*				*
;	*********************************
;
cerror:	
;error in command
	call	crlf
	mvi	a,'?'
	call	pchar
	jmp	start
;
;	*********************************
;	*				*
;	*  general purpose subroutines	*
;	*				*
;	*********************************
;
COMDEF:
	lxi	h,FCB+9		;set up address
	mvi	a,'C'
	mov	m,a		;store it
	inx	h
	mvi	a,'O'
	mov	m,a		;store it
	inx	h
	mvi	a,'M'
	mov	m,a
	ret
;
;
SYMDEF:
	lxi	h,FCB+019h		;set up address
	mvi	a,'S'
	mov	m,a		;store it
	inx	h
	mvi	a,'Y'
	mov	m,a		;store it
	inx	h
	mvi	a,'M'
	mov	m,a
	ret
;
;
fildel:	
;file character delimiter in a?
	cpi	'.'
	rz
fildel0:
	cpi	','		;comma?
	rz
	cpi	cr
	rz
	cpi	'*'
	rz		;series of ?'s
	cpi	' '
	ret		;zero for cr, ., or blank
;
filfield:
	;fill the current fcb field to max c characters
	call	fildel	;delimiter?
	jz	filf1	;skip if so
	mov	m,a
	inx	h	;character filled
	call	gnfcb	;get next character
	dcr	c	;field length exhausted?
	jnz	filfield;for another character
;	clear to delimiter
filf0:	call	fildel
	rz		;return with delimiter in a
	call	gnfcb	;get another char
	jmp	filf0	;to remove it
;
filf1:	;delimiter found before field exhausted
	mvi	d,' '	;fill with blanks?
	cpi	'*'
	jnz	filf2	;yes, if not *
	call	gnfcb	;read past the *
	mvi	d,'?'	;otherwise fill with ?'s
filf2:	mov	m,d	;fill remainder with blanks/questions
	inx	h	;to next character
	dcr	c	;count field length down
	jnz	filf2	;for another blank
	ret		;with delimiter in reg-a
;
;
bcde:	;compare bc > de (carry gen'd if true)
	mov	a,e
	sub	c
	mov	a,d
	sbb	b
	ret
;
WRPCHK:
	push	h
	push	d
	push	b
	mov	d,b
	mov	e,c
	lxi	h,0FFFFh
	call	HLDE
	pop	b
	pop	d
	pop	h
	ret
;
HLDE:
	mov	a,h	;Acc = H
	cmp	d	;is H <= D
	rc		;return if H < D with carry
	rnz		;return if H > D
	mov	a,l	;low order check H = D
	cmp	e	;what is the relationship
; H = D so test lower byte
	rc		;return if L < E with carry
	rnz		;return if L > E
	xra	a	;set zero for equality
	ret
;
nodis:	;remove dis/assembler from memory image
	mvi	a,1
	sta	dasm	;marks dis/assem as missing
	lxi	h,demon
	shld	bdose+1	;exclude dis/assembler
	shld	sytop	;mark top of symbol table
	ret
;

; Scanners for various needs
;
;	move the command buffer to the default area at dbf
FCBIN:	lxi	d,curlen	;current length dec'ed at gnc
	lxi	h,dbf		;default buffer
	ldax	d		;dec'ed length (exclude i)
	mov	c,a		;ready for loop
	mov	m,a		;store dec'ed length
	inr	c		;length ready for looping
	inx	d		;past 'i'
dbfill:	inx	d		;to first/next char
	inx	h		;to first/next to fill
	ldax	d		;get next char
	ani	07Fh		;zero out lower case bit
	mov	m,a		;to buffer
	dcr	c		;end of buffer?
	jnz	dbfill		;loop if not
	mov	m,c		;00 at end of buffer
;
;	now fill the file control blocks at fcb and fcb2
	mvi	e,2	;fill fcb/fcb2
	lxi	h,fcb	;start of default fcb
	call	GETFILE
;
;
;	now check for both fcb's complete
	dcr	e
	cnz	GETFILE		;to scan the second half
	mvi	m,0	;fill current record field
	ret
;
;
;
getbuff:	;fill command buffer and set pointers
	mvi	c,getf	;get buffer function
	lxi	d,comlen;start of command buffer
	call	trapad	;fill buffer
	lxi	h,combuf;next to get
	shld	nextcom
	ret
;
;
scan3:	;scan three expn's for fill and move
	call	scanexp
	cpi	3
	jnz	cerror
	call	getval
	push	h
	call	getval
	push	h
	call	getval
	pop	d
	pop	b	;bc,de,hl
	ret
;
;
scanword:
	;perform scan, with possible word mode
	call	gnc	;check for w
	lxi	h,wdisp
	mvi	m,0	;clear it now, check for w
	cpi	'W'
	jnz	scanex	;skip if not w and continue
;	w encountered, set word mode
	mvi	m,0ffh
;	and drop through for remainder of scan
;
scanexp:	;scan expressions - carry set if ,b
;	zero set if no expressions, a set to number of expressions
;	hi order bit set if ,b also
	call	gnc
;
scanex:	;enter here if character already scanned
	lxi	h,explist
	mvi	m,0	;zero expressions
	inx	h	;ready to fill expression list
	cpi	cr	;end of line?
	jz	scanret
;
;	not cr, must be digit or comma
	cpi	','
	jnz	scane0
;	mark as comma
	mvi	a,80h
	sta	explist
	lxi	d,0
	jmp	scane1
;
scane0:	;not cr or comma
	call	getexp	;expression to d,e
scane1:	call	scstore	;store the expression and increment h,l
	cpi	cr
	jz	scanret
	call	gnc
	call	getexp
	call	scstore
;	second digit scanned
	cpi	cr
	jz	scanret
	call	gnc
	call	getexp
	call	scstore
	cpi	cr
	jnz	cerror
scanret:
	lxi	d,explist	;look at count
	ldax	d		;load count to acc
	cpi	81h		;, without b?
	jz	cerror
	inx	d		;ready to extract expn's
	ora	a	;zero flag may be set
	rlc
	rrc		;set carry if ho bit set (,b)
	ret			;with flags set
;
;
GETFILE:
; Get filename for FCB routine
fildisk:
	call	gnfcb0	;read and clear lookahead character
	cpi	' '
	jz	fildisk	;deblank input line
;
	push	psw	;save first character
	call	gnfcb	;get second character
	cpi	':'
	jnz	nodisk	;skip if not disk drive
;
;	disk specified, fill with drive name
	pop	psw
	sui	'A'-1	;normalized to 1,2,...
	mov	m,a
	inx	h	;filled to memory
	call	gnfcb0	;scan another character
	jmp	filnam
;
nodisk:	;use default drive (00 in fcb/fcb2)
	mov	b,a	;save second char
	mvi	m,0
	inx	h	;character filled
	pop	psw	;recall original character
;
filnam:	
;fill the file name field, first character in a
	mvi	c,ffnl	;file name length
	call	filfield;filed filled, padded with blanks
	cpi	'.'	;delimiter period filename.filetype
	cz	gnfcb	;clear the period
;
	mvi	c,fftl	;file type length in c
	call	filfield;fill the type field
;
filext:	;now cleared to next blank or cr
	mvi	c,fcbl/2-ffnl-fftl-1	;number of bytes remaining
filex0:
	mvi	m,0
	inx	h	;fill a zero
	dcr	c
	jnz	filex0
	ret
;
;
; set input file control block (at 5ch) to simulate console command
;	useful subroutines for infcb:
gnfcb0:	;zero the lookahead character and read
	mvi	b,0
gnfcb:	;get next fcb character from lookahead or input
	mov	a,b	;lookahead active?
	mvi	b,0	;clear if so
	ora	a	;set flags
	rnz
	jmp	gnc	;otherwise get real character
;
gnc:	;get next console character with translation
	call	gnlc	;get next lower case char
	;drop through to translate
trans:
;	translate to upper case
	cpi	7fh	;rubout?
	rz
	cpi	('A' or 0100000b)	;upper case a
	rc
	ani	1011111b	;clear upper case bit
	ret
;
gnlc:
;	get next buffer character from console w/o translation
	push	h	;save for reuse locally
	lxi	h,curlen
	mov	a,m
	ora	a	;zero?
	mvi	a,cr
	jz	gncret	;return with cr if exhausted
	dcr	m	;curlen=curlen-1
	lhld	nextcom
	mov	a,m	;get next character
	inx	h	;nextcom=nextcom+1
	shld	nextcom	;updated
gncret:	pop	h	;restore environment
	ret;
;
;	*********************************
;	*				*
;	*	Disk I/O routines	*
;	*				*
;	*********************************
;
opn:	
;file open routine.  this subroutine opens the disk input
	push	h
	push	d
	push	b
	xra	a
	sta	dbp	;clear buffer pointer
	mvi	c,opf
	lxi	d,dfcb
	call	trapad	;to bds
	pop	b
	pop	d
	pop	h
	ret
CLOSE:
	push	b
	push	d
	push	h
	mvi	c,16
	call	TRAPAD
	pop	h
	pop	d
	pop	b
	ret
;
DWRITE:
; Disk write routine
	push	b
	push	d
	push	h
	mvi	c,WRITF		;write func
	call	TRAPAD
	pop	h
	pop	d
	pop	b
	ret
;
;
SETDMA:
; DMA address set routine
	push	b
	push	d
	push	h
	mvi	c,DMAF		;DMA func #
	call	TRAPAD
	pop	h
	pop	d
	pop	b
	ret
;
MAKE:
;make a file
	push	b
	push	d
	push	h
	mvi	c,22
	call	TRAPAD
	pop	h
	pop	d
	pop	b
	ret
;
DELETE:
; File delete routine
	push	b
	push	d
	push	h
	mvi	c,DELF
	call	TRAPAD
	pop	h
	pop	d
	pop	b
	ret
;
;	read files (hex or com)
;
;
qtype:	;check for command file type (com, hex, utl)
;	regs a,b,c contain characters to match
	lxi	h,fcb+fft
	cmp	m
	rnz		;return with no match?
	mov	a,b	;matched, check next
	inx	h	;next fcb char
	cmp	m
	rnz		;matched?
	mov	a,c	;yes, get next char
	inx	h
	cmp	m	;compare, and
	ret		;return with nz flag if no match
;
;
comload:	;compare hl > mload
	xchg	;h,l to d,e
	lhld	mload	;mload to h,l
	mov	a,l	;mload lsb
	sub	e
	mov	a,h
	sbb	d	;mload-oldhl gens carry if hl>mload
	xchg
	ret
;
ckmload:	;check for hl > mload and set mload if so
	call	comload	;carry if hl>mload
	rnc
	shld	mload	;change it
	ret
;
;
CKDFLD:
	xchg
	lhld	DEFLOAD
	mov	a,l	;lsb
	sub	e	;
	mov	a,h	;msb
	sbb	d	;is it smaller?
	xchg
	rnc		;no change
	shld	DEFLOAD	;return new value
	ret
;
;
chkdis:	;check for disassm present
	lda	dasm	;=00 if present
	cpi	1	;00-1 generates carry
	rnc		;01-1 generates "no carry"
;	otherwise, check high load address
	push	h
	lxi	h,modbas	;base address
	call	comload
	pop	h
	ret
;
; Print routines for sscreen display
;
blank:
	mvi	a,' '
;
pchar:	;print character to console
	push	h
	push	d
	push	b
	mov	e,a
	mvi	c,cof
	call	trapad
	pop	b
	pop	d
	pop	h
	ret
;
prmsg:	;print message at hl until 00 encountered
	mov	a,m
	ora	a
	rz		;end if 00 found
	call	pchar	;print the current char
	inx	h	;move to next char
	jmp	prmsg	;for another char

;
pnib:	;print nibble in lo accum
	cpi	10
	jnc	pnibh	;jump if a-f
	adi	'0'
	jmp	pchar	;ret thru pchar
pnibh:	adi	'A'-10
	jmp	pchar
;
pbyte:	push	psw	;save a copy for lo nibble
	rar
	rar
	rar
	rar
	ani	0fh	;mask ho nibble to lo nibble
	call	pnib
	pop	psw	;recall byte
	ani	0fh
	jmp	pnib
;
crlf:	;carriage return line feed
	mvi	a,cr
	call	pchar
	mvi	a,lf
	jmp	pchar
;
break:	;check for break key
	push	b
	push	d
	push	h
	mvi	c,chkio
	call	trapad
	ani	1b
	pop	h
	pop	d
	pop	b
	ret
;
paddsh:	;print address reference given by hl
	xchg
;
paddsy:	;print address reference given by de, along
;	with symbol at that address (if it exists)
	push	d	;save the address for symbol lookup
	xchg		;ready for the address dump
	call	paddr	;hex value printed
	pop	d	;recall search address
	lda	negcom	;negated command?
	ora	a	;ff?
	rnz		;return if true
	call	alookup	;address lookup
	rz		;skip symbol if not found
;	symbol found, print it
prdotsy:
	;print symbol preceded by .
	call	blank
	mvi	a,'.'
	call	pchar
;
;	drop through to print symbol
prsym:
	mov	e,m	;get length of symbol
prsy0:	dcx	h	;to first/next character
	mov	a,m	;next to print
	call	pchar	;character out
	dcr	e	;count length down
	jnz	prsy0
	ret		;return to caller
;
;	enter here to print optional label at hl
prlabel:
	push	h	;save address
	lda	negcom	;negated?
	ora	a
	pop	d	;recalled in case return
	rnz		;continue if not negated
	call	alookup	;does the label exist?
	rz		;return if not present
	call	crlf	;go to newline
	call	prsym	;print the symbol
	mvi	a,':'
	call	pchar	;label:
	ret
;
;
paddr:	;print the address value in h,l
	mov	a,h
	call	pbyte
	mov	a,l
	jmp	pbyte
;
pgraph:	;print graphic character in reg-a or '.' if not
	cpi	7fh
	jnc	pperiod
	cpi	' '
	jnc	pchar
pperiod:
	mvi	a,'.'
	jmp	pchar
;
discom:	;compare h,l against dismax.  carry set if hl > dismax and
	xchg
	lhld	dismax
	mov	a,l
	sub	e
	mov	l,a	;replace for zero tests later
	mov	a,h
	sbb	d
	xchg
	ret
;
;
;	sydelim checks for / + - cr , or blank
;	sysep   checks for   + - cr , or blank
;	delim   checks for       cr , or blank
;
;
sydelim:;check for symbol delimiter
	cpi	'/'	;separator
	rz
sysep:	;separator?
	cpi	'+'
	rz
	cpi	'-'
	rz
;
delim:	;check for delimiter character
	cpi	cr
	rz
	cpi	','
	rz
	cpi	' '
	ret
;
hexcon:	;convert accumulator to pure binary from external hex
	sui	'0'
	cpi	10
	rc		;must be 0-9
	adi	('0'-'A'+10) and 0ffh
	cpi	16
	rc		;must be 0-15
	jmp	cerror	;bad hex digit
;
getval:	;get next expression value to h,l (pointer in d,e assumed)
	xchg
	mov	e,m
	inx	h
	mov	d,m
	inx	h
	xchg
	ret
;
getsymv:
	;lookup symbol preceded by =, @, or . operator
	push	d	;save next to fill in address vector
	call	gnc	;read the next character
	lhld	sytop	;hl is beginning of search
getsy0:	push	psw	;save first character
	mov	c,m	;length of current symbol
	mov	a,c	;to a for end of search check
	cpi	16	;length 16 or more ends search
	jnc	cerror	;? error if not there
	pop	psw	;recall first character
	xchg		;symbol address to de
	push	d	;save search address
	push	psw	;save character
	lhld	nextcom	;next buffer position
	push	h	;saved to memory
	lhld	comlen	;comlen and curlen
	push	h	;save to memory
;	stacked: curlen/nextcom/char/symaddr
	xchg		;de is next to match+1
	inr	c	;count+1
sychar:	;check next character
	call	sydelim	;/, comma, cr, or space?
	jz	sydel	;stop scan if so
;	not a delimiter in the input, end of symbol?
	dcr	c	;count=count-1
	jz	synxt	;skip to next symbol if so
;	not end of symbol, check for match
	dcx	h	;next symbol address
	cmp	m	;same?
	jnz	synxt	;skip if not
	call	gnc	;otherwise, get next input character
	jmp	sychar	;for another match attempt
;
sydel:	;delimiter found, count should go to zero
	dcr	c
	jnz	synxt	;skip symbol if not
;
;	symbol matched, return symbol's value
	pop	h	;discard comlen
	pop	h	;discard nextcom
	pop	h	;discard first character
	call	sysep	;+ - cr, comma, or space? (not / test)
	jz	syloc	;return if not a / at end
	call	gnc	;remove the / and continue the scan
	jmp	synxt0	;for another symbol
;
;	end of input, get value to de
syloc:	pop	h	;recall symbol address
	inx	h	;to low address
	mov	e,m	;low address to de
	inx	h	;to high address
	mov	d,m	;to d
	pop	h	;re-instate hl
	ret		;with de=value, hl=next to fill
;
;
synxt:	;move to the next symbol
	pop	h	;comlen
	shld	comlen	;restored
	pop	h	;nextcom
	shld	nextcom	;restored
	pop	psw	;first character to a
synxt0:	pop	h	;symbol address
	push	psw	;save first character
	mov	a,m	;symbol length
	cma		;1's complement of length
	add	l	;hl=hl-length-1
	mov	l,a
	mvi	a,0ffh	;extend sign of length
	adc	h	;high order bits
	mov	h,a	;now move past address field
	dcx	h	;-1
	dcx	h	;total is: hl=hl-length-3
	pop	psw	;recall first character
	jmp	getsy0	;for another search
;
;
;	otherwise, numeric operand expected
getoper:	;get hex value to d,e (possible symbol reference)
	xchg		;next to fill in de
	lxi	h,0	;ready to accumulate value
	cpi	'.'	;address reference?
	jz	getsymv	;return through getsymv
	cpi	'@'	;value reference?
	jnz	getoper0	;skip if not
	call	getsymv	;address to de
	push	h	;save next to fill
	xchg		;address of double prec value to hl
	mov	e,m
	inx	h
	mov	d,m	;double value to de
	pop	h	;restore next to fill
	ret		;with de=value, hl=next to fill
getoper0:
	cpi	'='	;byte reference?
	jnz	getoper1	;skip if not
;	found a byte reference, look up symbol
	call	getsymv	;de = address, hl = next to fill
	push	h	;save hl
	xchg		;operand address to hl
	mov	e,m	;get byte value
	mvi	d,0	;high byte is zero
	pop	h	;restore next to fill
	ret		;with de=value, hl=next to fill
;
getoper1:
;	not ., @, or .
	cpi	''''	;start of string?
	jnz	getoper2
;	start of string, scan until matching quote
	xchg		;return 0000 to de, next to fill to hl
getstr0:
	call	gnlc	;inside quoted string
	cpi	' '	;must be grapic
	jc	cerror	;otherwise report error
;	character is graphic, check for embedded quotes
	cpi	''''
	jnz	getstr1	;skip if not
;	must be embedded quote or end of string
	call	gnlc	;character following quote
	call	sysep	;symbol separator?
	rz		;return with value in de
;	otherwise the symbol is not a separator, must be quote
	cpi	''''
	jnz	cerror	;report error if not
getstr1:
	;store the ascii character into low order de
	mov	d,e	;low character to high character
	mov	e,a	;low character from accumulator
	jmp	getstr0	;for another character scan
;
getoper2:
	;check for decimal input
	cpi	'#'
	jnz	getoper3	;must be hex
;	decimal input, convert
getdec0:
	call	gnc		;get next digit
	call	sysep		;separator?
	jz	getdec1		;skip to end if so
	sui	'0'		;decimal digit?
	cpi	10
	jnc	cerror		;error if above 9
	dad	h		;hl=hl*2
	mov	b,h		;save high order
	mov	c,l		;save low order
	dad	h		;*4
	dad	h		;*8
	dad	b		;*10
	mov	c,a		;ready to add digit
	mvi	b,0
	dad	b		;digit added to hl
	jmp	getdec0		;for another digit
;
getdec1:
	xchg
	ret			;with de=value
;
getoper3:
	cpi	'^'	;stacked value?
	jnz	getoper4;skip if not
;
;	get stacked value
	push	d	;save next to fill
	lhld	sloc	;stack pointer
getstk:	mov	e,m
	inx	h
	mov	d,m	;de is stacked value
	inx	h	;in case another ^
	call	gnc	;get another char
	cpi	'^'	;^ ... ^
	jz	getstk
	pop	h	;de=value, hl=next to fill
	ret		;with value in de
;
getoper4:
;	not ., @, =, or ', must be numeric
	call	hexcon
	dad	h	;*2
	dad	h	;*4
	dad	h	;*8
	dad	h	;*16
	ora	l	;hl=hl+hex
	mov	l,a
	call	gnc
	call	sysep	;delimiter?
	jnz	getoper3
	xchg
	ret
;
scstore:	;store d,e to h,l and increment address
	xchg
	shld	lastexp	;save as "last expression"
	xchg
	mov	m,e
	inx	h
	mov	m,d
	inx	h
	push	h
	lxi	h,explist
	inr	m	;count number of expn's
	pop	h
	ret
;
getexp:
	;scan the next expression with embedded +,- symbols
	cpi	'-'	;leading minus?
	jnz	getexpp	;skip to next if not
	lxi	d,0	;assume a starting 0, with following minus
	jmp	getexp2	;to continue with the scan
;
getexpp:
	;check for leading + operator
	cpi	'+'
	jnz	getexp0	;to continue the scan
;	leading + found, use last expression
	xchg		;de=hl
	lhld	lastexp	;last expression to hl
	xchg		;then to de
	jmp	getplus	;handle the plus operator
getexp0:
	;scan next item
	call	getoper	;value to de
getexpo:
	;get expression operator
	cpi	'+'	;stopped on +?
	jnz	getexp1	;skip to next test if not
;	+ delimiter found, scan following operand
getplus:
	push	d	;save current value
	call	gnc	;scan past the +
	call	getoper	;next value to de
	pop	b	;recall previous value
	xchg		;next value to hl
	dad	b	;sum in hl
	xchg		;back to position
	jmp	getexpo	;to test for following operand
;
getexp1:
	;not a +, check for - operator
	cpi	'-'
	rnz		;return with delimiter in a if not
;	- delimiter found
getexp2:
	call	gnc	;to clear the operator
	push	d	;save current value
	call	getoper	;to get the next value
	pop	b	;recall original value to bc
	push	psw	;save character
	mov	a,c	;low byte to a
	sub	e	;diff in low bytes
	mov	e,a	;back to e
	mov	a,b	;high byte to a
	sbb	d	;diff in high bytes
	mov	d,a	;back to de
	pop	psw	;restore next character
	jmp	getexpo	;for the remainder of the expression

;
;
;	subroutines for cpu state display
flgshf:	;shift computation for flag given by reg-b
;	reg a contains flag upon exit (unshifted)
;	reg c contains number of shifts required+1
;	regs d,e contain address of flags in template
	push	h
	lxi	h,flgtab	;shift table
	mov	e,b
	mvi	d,0
	dad	d
	mov	c,m		;shift count to c
	lxi	h,floc		;address of flags
	mov	a,m		;to reg a
	xchg			;save address
	pop	h
	ret
;
getflg:	;get flag given by reg-b to reg-a and mask
	call	flgshf	;bits to shift in reg-a
getfl0:	dcr	c
	jz	getfl1
	rar
	jmp	getfl0
getfl1:	ani	1b
	ret
;
getdba:	;get double byte address corresponding to reg-a to hl
	sui	bval	;normalize to 0,1,...
	lxi	h,rinx	;index to stacked values
	mov	e,a	;index to e
	mvi	d,0	;double precision
	dad	d	;indexed into vector
	mov	e,m	;offset to e
	mvi	d,0ffh	;-1
	lxi	h,stack
	dad	d	;hl has base address
	ret
;
getdbl:	;get double byte corresponding to reg-a to hl
	call	getdba	;address of elt in hl
	mov	e,m	;lsb
	inx	h
	mov	d,m	;msb
	xchg		;back to hl
	ret
;
delt:	;display cpu element given by count in reg-b, address in h,l
	mov	a,b	;get count
	cpi	aval	;past a?
	jnc	delt0	;jmp if not flag
;
;	display flag
	call	getflg	;flag to reg-a
	ora	a	;flag=0?
	mvi	a,'-'	;for false display
	jz	pchar	;return through pchar
	mov	a,m	;otherwise get the character
	jmp	pchar	;print the flag name if true
;
delt0:	;not flag, display x= and data
	push	psw
	mov	a,m
	call	pchar	;register name
	mvi	a,'='
	call	pchar
	pop	psw
	jnz	delt1	;jump if not reg-a
;
;	register a, display byte value
	lxi	h,aloc
	mov	a,m
	call	pbyte
	ret
;
delt1:	;double byte display
	call	getdbl	;to h,l
	call	paddr	;printed
	ret
;
dstate:	;display cpu state
	call	crlf	;new line
	call	blank	;single blank
	lxi	h,rvect	;register vector
	mvi	b,0	;register count
dsta0:	push	b
	push	h
	call	delt	;element displayed
	pop	h	;rvect address restored
	pop	b	;count restored
	inr	b	;next count
	inx	h	;next register
	mov	a,b	;last count?
	cpi	pval+1
	jnc	dsta1	;jmp if past end
	cpi	aval	;blank after?
	jc	dsta0
;	yes, blank and go again
	call	blank
	jmp	dsta0
;
;	ready to send decoded instruction
dsta1:
	call	blank
	call	nbrk	;compute breakpoints in case of trace
	push	psw	;save expression count - b,c and d,e have bpts
	push	d	;save bp address
	push	b	;save aux breakpoint
	call	chkdis	;check to see if disassember is here
	jnc	dchex	;display hex if not
;	disassemble code
	lhld	ploc	;get current pc
	shld	dispc	;set disassm pc
	lxi	h,dispg;page mode = 0ffh to trace
	mvi	m,0ffh
	call	disen
	jmp	dstret
;
dchex:	;display hex
	dcx	h	;point to last to write
	shld	dismax	;save for compare below
	lhld	ploc	;start address of trace
	mov	a,m	;get opcode
	call	pbyte
	inx	h	;ready for next byte
	call	discom	;zero set if one byte to print, carry if no more
	jc	dstret
	push	psw	;save result of zero test
	call	blank	;separator
	pop	psw	;recall zero test
	ora	e	;zero test
	jz	dsta2
;	display double byte
	mov	e,m
	inx	h
	mov	d,m
	call	paddsy	;print address
	jmp	dstret
;
dsta2:	;print byte value
	mov	a,m
	call	pbyte
dstret:
;	now print symbol for this instruction if implied memory op
	lhld	ploc	;instruction location
	mov	a,m	;instruction to a register
	mov	b,a	;copy to b register
;	check for adc, add, ana, cmp, ora, sbb, sub, xra m
	ani	1100$0000b	;high order bits 11?
	cpi	1000$0000b	;check
	jnz	notacc
;	found acc-reg operation, involving memory?
	mov	a,b	;restore op code
	ani	0000$0111b
	cpi	6	;memory = 6
	jnz	disrest	;skip to restore registers if not
	jmp	dismem	;to display symbol
;
notacc:	;not an accumulator operation, check for mov x,m or m,x
	cpi	0100$0000b	;mov operation?
	jnz	notmov
	mov	a,b	;mov operation or halt
	cpi	hlt	;skip halt test
	jz	disrest	;to skip tests
	ani	111b	;move from memory?
	cpi	6
	jz	dishl	;skip to print hl if so
;	not move from memory, move to memory?
	mov	a,b	;restore operation code
	ani	111000b	;select high order register
	cpi	6 shl 3	;check for memory op
	jnz	disrest	;skip to restore if not
	jmp	dishl	;to display hl register
;
notmov:	;not a move operation, check for mvi m
	mov	a,b	;restore operation code
	cpi	0011$0110b	;mvi m,xx?
	jz	dishl		;display hl address if so
;	now look for inr m, dcr m
	cpi	0011$0100b	;inr m?
	jz	dismem	;skip to print hl if so
	cpi	0011$0101b	;dcr m?
	jnz	notidcr	;skip if not inr / dcr m
dismem:	;display memory value first
	mvi	a,'='
	call	pchar
	lhld	hloc
	mov	a,m
	call	pbyte
;
dishl:	;display the hl symbol, if it exists
	lhld	hloc
	jmp	dissym	;to retrieve the symbol
;
notidcr:
	;check for ldax/stax b/d
	ani	1110$0111b	;ldax = 000 x1 010
	cpi	0000$0010b	;stax = 000 x0 010
	jnz	disrest		;skip if not
	mov	a,b		;ldax/stax, get register
	ani	0001$0000b	;get the b register bit
	lhld	dloc
	jnz	dissym		;skip to display
	lhld	bloc		;display b instead
dissym:	;enter here with the hl register set to symbol location
	lda	negcom	;negated?
	ora	a
	jnz	disrest	;forget it.
	xchg		;search address to de
	call	alookup	;zero set if not found
	jz	disrest	;restore if not found
	call	prdotsy	;.symbol printed
;	drop through to restore the registers
disrest:
	pop	b	;aux breakpoint
	pop	d	;restore breakpoint
	pop	psw	;restore count
	ret
;
;	data vectors for cpu display
rvect:	db	'CZMEIABDHSP'
rinx:	db	(bloc-stack) and 0ffh	;location of bc
	db	(dloc-stack) and 0ffh	;location of de
	db	(hloc-stack) and 0ffh	;location of hl
	db	(sloc-stack) and 0ffh	;location of sp
	db	(ploc-stack) and 0ffh	;location of pc
;	flgtab elements determine shift count to set/extract flags
flgtab:	db	1,7,8,3,5	;cy, zer, sign, par, idcy
;
clrtrace:	;clear the trace flag
	lxi	h,0
	shld	tracer
	xra	a	;clear accumulator
	sta	tmode	;clear trace mode
	ret
;
breakp:	;arrive here when programmed break occurs
	di
	shld	hloc	;hl saved
	pop	h	;recall return address
	dcx	h	;decrement for restart
	shld	ploc
;	dad sp below destroys cy, so save and recall
	push	psw	;into user's stack
	lxi	h,2	;bias sp by 2 because of push
	dad	sp	;sp in hl
	pop	psw	;restore cy and flags
	lxi	sp,stack-4;local stack
	push	h	;sp saved
	push	psw
	push	b
	push	d
;	machine state saved, clear break points
	ei		;in case interrupt driven io
	lhld	ploc	;check for rst instruction
	mov	a,m	;opcode to a
	cpi	rstin
;	save condition codes for later test
	push	psw
;	save ploc for later increment or decrement
	push	h
;
;	clear any permanent break points
;
;	check for auto "u" command from perm break pass
	lda	pbcnt	;=00 if no auto u in effect
	sta	autou	;hold this condition in auto u
;
;	permanent breaks may be active, clear them
;
	lxi	h,pbtable+(pbsize-1)*pbelt	;set to last elt
	mvi	c,pbsize	;number of elements
resper0:
	push	h		;save element address
	mov	a,m		;(count)
	ora	a		;set flags
	jz	resper1		;skip if not in use
	inx	h		;to next address
	mov	e,m		;low(addr)
	inx	h
	mov	d,m		;high(addr)
	inx	h
	mov	a,m		;data to set at addr
	stax	d		;data back to memory
resper1:
	pop	h		;base of element
	lxi	d,-pbelt	;element size
	dad	d		;addressing previous element
	dcr	c		;count table douwn
	jnz	resper0		;for another element
;
;	drop through when we have replaced all elements,
;	now check for an "auto u" command from the last
;	permanent break point bypass
	call	respbc	;restore pbcnt
;
clergo:
;	clear "go" breakpoints which are pending
	lxi	h,breaks
	mov	a,m
	mvi	m,0	;set to zero breaks
cler0:	ora	a	;any more?
	jz	cler1
	dcr	a
	mov	b,a	;save count
	inx	h	;address of break
	mov	e,m	;low addr
	inx	h
	mov	d,m	;high addr
	inx	h
	mov	a,m	;instruction
	stax	d	;back to program
	mov	a,b	;restore count
	jmp	cler0
;
cler1:
;	all breakpoints have been cleared, check type of interrupt
	pop	h	;restore ploc
	pop	psw	;restore condition rstin=instruction
	jz	softbrk	;skip to softbreak if rst instruction
	inx	h	;front panel interrupt, don't dec ploc
	shld	ploc	;incremented
	xchg		;ploc to de
	if	isis2	;check for below bdtop
	lxi	b,bdtop
	call	bcde
	jnc	softbrk
	else
	lxi	h,trapjmp+1	;address ifeld of jmp bdos
	mov	c,m		;low address
	inx	h		;.high address
	mov	b,m		;bc is bdos address
	call	bcde		;to compare
	jc	softbrk
	endif
;
;	in the bdos, don't break until the return occurs
	call	clrtrace
	lhld	retloc	;trapped upon entry to bdos
	xchg
	mvi	a,82h	;looks like g,bbbb
	ora	a	;sets flags
	stc		;"," after g
	jmp	gopr	;to set break points
;
softbrk:
	;now check for a matching address for a permanent break
;	a matching address for a permanent break
	lda	pbtrace		;ff if trace from last perm break
	ora	a		;ff if traced
	jnz	stopcrx		;stop if so
;
;	may be active permanent breaks, are we at one now?
	lxi	h,pbtable
	mvi	c,pbsize
chkpb0:	;check next element for permanent break address
	push	h	;save current pbtable address
	mov	a,m	;(count)
	ora	a	;set flags
	jz	chkpb3	;skip if zero
	inx	h	;.low(addr)
	mov	a,m	;low(addr) in a
	inx	h
	mov	d,m	;high(addr) in d
	lhld	ploc	;program location
	cmp	l	;low(addr) = low(ploc)?
	jnz	chkpb3	;skip if not
	mov	a,d	;check high bytes
	cmp	h
	jnz	chkpb3	;skip if addr <> ploc
;
;	addresses match, print trace or stop
	pop	h	;recall element address
	mov	a,m	;pass count
	dcr	a	;1 becomes 0
	jnz	chkpb1	;skip if not last count
;
;	stop execution at this point
	push	psw	;for "pass" report below
	dcr	a	;00 becomes ff
	sta	pbtrace	;perm break trace on
;	trace is cleared on next iteration through code
;	zero in accumulator printed in trace heading
	jmp	chktra0	;to trace and stop
;
chkpb1:	;not the last count, decrement and set autou mode
	mov	m,a	;count=count-1
	push	psw	;save count
	call	dectra	;decrement trace counters
	cpi	2	;trace mode = 2?
	jz	chktra0	;skip to print trace if so
;
;	must be u/-u or g/-g, check negative command
	lda	negcom
	ora	a	;set to ff if -u or -g
	jz	chktra0	;00 if u or g, so trace it
;
;	must be -u or -g, so suppress the trace through
;	ploc will match perm break address in gopr, so compute breaks
	call	nbrk	;setup break addresses
	jmp	gopr	;to move past break address
;
chktra0:
	;print the header and go around again (may be one more time)
;	(decremeted count is currently stacked)
	call	crlf
	pop	psw
	inr	a	;restore count
	call	pbyte	;print the byte value
	lxi	h,passmsg	;hh pass
	call	prmsg	;pass message printed
	lhld	ploc	;location counter
	xchg		;readied for paddsy
	call	paddsy	;print address and symbol
	call	dstate	;display the current cpu state
	jmp	gopr	;to iterate one last time
;
chkpb3:	;move to next element
	pop	h	;recall element address
	lxi	d,pbelt	;element size
	dad	d	;to next element
	dcr	c	;count table down
	jnz	chkpb0
;
cler2:	;end of permanent breakpoint scan
;	arrive here following simple break from a g command, or
;	following an autou past a permanent break point
;	may also be trace/untrace mode
;
	call	break		;break at the console?
	jnz	stopcrx		;stop execution if so
	call	dectra		;decrement trace flags
	jz	stopcr		;end if auto u not set (tmode=0)
	dcr	a		;1=untrace becomes 0
	jnz	break1		;skip to print trace if not
;
;	untrace mode, with or without autou set
;	current ploc is not a permanent break address
	call	nbrk		;next break computed
	jmp	gopr		;go to the program untraced
;
break1:	;must be trace mode, not a permanent break address
;	with or without the autou flag set
	lhld	ploc		;label trace
	call	prlabel
	call	dstate		;display cpu state
	jmp	gopr		;to next machine instruction
;
stopcr:	;not untrace/trace mode, if autou set then continue
;	since this must be a step through a break point
	lda	autou
	ora	a	;zero set?
	jz	stopcrx	;skip if autou not set
;	auto u set, must be step through a break point, next address
;	is not a permanent break point, so go to user breaks
	lhld	gobrk2	;auxiliary break point
	mov	c,l	;to bc
	mov	b,h	;in case set
	lhld	gobrk1	;primary break point
	xchg		;to de
	lda	gobrks	;number of breaks set by user
	ora	a	;may set the zero flag
	stc		;carry indicates use current ploc
	jmp	gopr	;to continue
;
stopcrx:
	call	crlf
;
stopex:
	call	respbc	;restore pbcnt/pbloc, if necessary
	lxi	h,0
	shld	userbrk		;clear user break address
	call	clrtrace	;trace flags go to zero
	sta	pbtrace		;clear perm trace flag
	mvi	a,'*'
	call	pchar
	lhld	ploc
;	check to ensure disassembler is present
	call	chkdis
	jnc	stop0
	shld	dispc
stop0:	call	paddsh	;print address with symbol location
	lhld	hloc
	shld	disloc
	jmp	start
;
passmsg:
	db	' PASS ',0	;printed in pass trace
;
dectra:	;decrement trace flags if trace mode
	lxi	h,tmode		;trace mode 0 if off, 1 un, 2 tr
	mov	a,m		;to accum
	ora	a		;set condition flags
	rz			;no action if off
	push	h		;save tmode address
	lhld	tracer		;get count
	dcx	h		;count=count-1
	shld	tracer		;back to memory
	mov	a,h		;now zero?
	ora	l		;hl=0000?
	pop	h		;restore tmode address
	jnz	dectr0		;skip if not
	mov	m,a		;tmode = 0
	dcr	a		;accum = ff
	sta	pbtrace		;to stop on next iteration
dectr0:	mov	a,m		;recall tmode
	ora	a		;set flags
	ret
;
cat:	;determine opcode category - code in register b
;	d,e contain double precision category number on return
	lxi	d,opmax	;d=0,e=opmax
	lxi	h,oplist
cat0:	mov	a,m		;mask to a
	ana	b	;mask opcode from b
	inx	h	;ready for compare
	cmp	m	;same after mask?
	inx	h	;ready for next compare
	jz	cat1	;exit if compared ok
	inr	d	;up count if not matched
	dcr	e	;finished?
	jnz	cat0
cat1:	mov	e,d	;e is category number
	mvi	d,0	;double precision
	ret
;
respbc:	;restore pbcnt to pbloc, if req'd
	lda	pbcnt	;00 if no auto u
	ora	a	;set flags
	rz		;no further actions if so
	lhld	pbloc	;pbtable element to restore
	mov	m,a	;(count)
	xra	a	;clear accumulator
	sta	pbcnt	;clear auto u mode
	ret
;
nbrk:	;find next break point address
;	upon return, register a is setup as if user typed g,b1,b2 or
;	g,b1 depending upon operator category.  b,c contains second bp,
;	d,e contains primary bp.  hl address next opcode byte
	lhld	ploc
	mov	b,m	;get operator
	inx	h	;hl address byte following opcode
	push	h	;save it for later
	call	cat	;determine operator category
	lxi	h,catno	;save category number
	mov	m,e
	lxi	h,cattab;category table base
	dad	d	;inxed
	dad	d	;inxed*2
	mov	e,m	;low byte to e 
	inx	h
	mov	d,m	;high byte to d
	xchg
	pchl		;jump into table
;
;	opcode category table
callop	equ	2	;position of call operator
callcon	equ	3	;position of call conditional
cattab:	dw	jmpop	;jump operator
	dw	ccop	;jump conditional
	dw	jmpop	;call operator (treated as jmp)
	dw	ccop	;call conditional
	dw	retop	;return from subroutine
	dw	rstop	;restart
	dw	pcop	;pchl
	dw	imop	;single precision immediate (2 byte)
	dw	imop	;adi ... cpi
	dw	dimop	;double precision immediate (3 bytes)
	dw	dimop	;lhld ... sta
	dw	rcond	;return conditional
	dw	imop	;in/out
;	next dw must be the last in the sequence
	dw	simop	;simple operator (1 byte)
;
jmpop:	;get operand field, check for bdos
	call	getopa	;get operand address to d,e and compare with bdos
	jnz	endop	;treat as simple operator if not bdos
;	otherwise, treat as a return instruction
retop:	call	getsp	;address at stacktop to d,e
	jmp	endop	;treat as simple operator
;
cbdos:	;de addresses a possible break point - check to ensure
;	it is not a jump to the bdos
;
	lda	trapjmp+1	;low bdos address
	cmp	e
	rnz
	lda	trapjmp+2	;high bdos address
	cmp	d
	ret
;
getopa:	;get operand address and compare with bdos
	pop	b	;get return address
	pop	h	;get operand address
	mov	e,m
	inx	h
	mov	d,m
	inx	h
	push	h	;updated pc into stack
	push	b	;return address to stack
	jmp	cbdos	;return through cbdos with zero flag set
;
getsp:	;get return address from user's stack to d,e
	lhld	sloc
	mov	e,m
	inx	h
	mov	d,m
	ret
;
ccop:	;call conditional operator
	call	getopa	;get operand address to d,e / compare with bdos
	jz	ccop1
;	not the bdos, break at operand address and next address
	pop	b	;next address to b,c
	push	b	;back to stack
	mvi	a,2	;two breakpoints
	jmp	retcat	;return from nbrk
;
ccop1:	;break address at next location only, wait for return from bdos
	pop	d
	push	d	;back to stack
	jmp	endop	;one breakpoint address
;
rstop:	;restart instruction - check for rst 7
	mov	a,b
	cpi	rstin	;restart instruction used for soft int
	jnz	rst0
;
;	soft rst, no break point since it will occur immediately
	xra	a
	jmp	retcat1	;zero accumulator
rst0:	ani	111000b	;get restart number
	mov	e,a
	mvi	d,0	;double precision breakpoint to d,e
	jmp	endop
;
pcop:	;pchl
	lhld	hloc
	xchg	;hl value to d,e for breakpoint
	call	cbdos	;bdos value?
	jnz	endop
;	pchl to bdos, use return address
	jmp	retop
;
chkcall:
	;check for call or call conditional operator,
	;if found, use the return address (pc+3) as break
	;return "no carry" if call or call conditional
	lda	catno	;category number
	cpi	callop	;category number for call operator
	rc		;carry if below callop
;	must be call operator or above
	cpi	callcon+1
;	carry set if below callcon+1, so complement
	cmc		;carry if callcon+1 or above
	rc		;carry implies not between callop and callcon
;	must be between callop and callcon (inclusive)
;	use pc+3 as the break for tw/uw or rom entry
	lhld	ploc
	inx	h
	inx	h
	inx	h	;ploc+3
	xchg		;to de
	ret		;with the no-carry bit set
;
;
simop:	;simple operator, use stacked pc
	pop	d
	push	d
	jmp	endop
;
rcond:	;return conditional
	call	getsp	;get return address from stack
	pop	b	;b,c alternate location
	push	b	;replace it
	mvi	a,2
	jmp	retcat	;to set flags and return
;
dimop:	;double precision immediate operator
	pop	d
	inx	d	;incremented once, drop thru for another
	push	d	;copy back
;
imop:	;single precision immediate
	pop	d
	inx	d
	push	d
;
endop:	;end operator scan
	mvi	a,1	;single breakpoint
retcat:	;return from nbrk
	inr	a	;count up for g,...
	stc
retcat1:
	push	psw	;save register state in case userbrk
	lhld	userbrk
	mov	a,h
	ora	l
	jz	retcat2	;no userbrk if zero
;
	push	d	;save break point
	push	b	;save aux break point
	push	h	;save userbrk address for pchl below
;	user break occurs here, call user routine and check return
	lxi	h,catno
	mov	c,m	;opcode category is in c
	lhld	ploc
	xchg		;location of instruction in d,e
	lxi	h,retuser
	xthl		;return address to stack, userbrk to h,l
	pchl
retuser:	;return from user break, check register a
	ora	a
	pop	b	;restore breakpoints
	pop	d
	jz	retcat2
;	abort the operation with a condition
	push	psw
	mvi	a,'#'
	call	pchar
	pop	psw
	call	pbyte
	mvi	a,' '
	call	pchar
	jmp	stopex	;stop execution
retcat2:
	;check for call operator with tw or uw mode set
	lda	tmode
	lxi	h,wdisp	;wdisp=ff if w encountered
	ana	m	;non zero if tmode>0, wmode set
	jz	notcall	;skip if not a call
;
;	this may be a call or call condition in tw/uw mode
	call	chkcall	;check for call, nc set if found
	jc	notcall	;skip if not a call
;
;	this is a call in tw/uw mode, de is pc+3, use it for break
	pop	psw	;previous break count in a
	mvi	a,2	;use only one break
	jmp	retcat4	;to return from nbrk
;
notcall:
	pop	psw	;recall g, state
	push	psw	;save for final return below
;
;	now check to ensure that break is not in rom
	ora	a	;zero break points set?
	jz	retcat3	;skip to end if so
;
;	must be 2/3 in accumulator
	dcr	a	;resulting in 1/2 breakpoints
;	bc = aux breakpoint, de = primary breakpoint
romram:	xchg		;first/aux breakpoint to hl
	mov	e,a	;breakpoint count to e (1/2)
	mov	a,m	;get code byte
	cma		;complement for rom test
	mov	m,a	;store to rom/ram
	cmp	m	;did it change?
	cma		;complement back to orginal
	mov	m,a	;restore in case ram
	mov	a,e	;restore breakpoint count
;	arrive here with zero flag set if ram break
	xchg		;break address back to de
	push	psw	;save count
	jz	ramloc	;skip if ram location
;
;	break address is in rom.  if conditional call, let
;	it go, the return break is already set.  if a simple
;	call, set break at the ploc+3.  otherwise, assume that
;	the stack contains the return address
	call	chkcall	;check for call or call conditional
	jnc	ramloc	;nc if found, de is return address
	;not a call operation, must be pchl or jmp
	call	getsp	;get the return address from stack
;
ramloc:	pop	psw	;restore break count
	dcr	a	;1/2 breaks becomes 0/1
	jz	retcat3	;stop analysis if breaks exhausted
;	otherwise, exchange bc/de and retry
	push	d	;de saved for exchange
	mov	e,c	;low bc to low de
	mov	d,b	;high bc to high de
	pop	b	;old de to bc
	jmp	romram	;to analze next break
;
retcat3:
	;analysis of rom/ram complete, restore counts
	pop	psw	;break count and carry
retcat4:
	pop	h	;next address recalled
	ret
;
;
;
;	opcode category tables
oplist:	db	1111$1111b,	1100$0011b	;0 jmp
	db	1100$0111b,	1100$0010b	;1 jcond
	db	1111$1111b,	1100$1101b	;2 call
	db	1100$0111b,	1100$0100b	;3 ccond
	db	1111$1111b,	1100$1001b	;4 ret
	db	1100$0111b,	1100$0111b	;5 rst 0..7
	db	1111$1111b,	1110$1001b	;6 pchl
	db	1100$0111b,	0000$0110b	;7 mvi
	db	1100$0111b,	1100$0110b	;8 adi...cpi
	db	1100$1111b,	0000$0001b	;9 lxi
	db	1110$0111b,	0010$0010b	;10 lhld shld lda sta
	db	1100$0111b,	1100$0000b	;11 rcond
	db	1111$0111b,	1101$0011b	;in out
opmax	equ	($-oplist)/2
;
;	symbol access algorithms
alookup:
;look for the symbol with address given by de
;return with non zero flag if found, zero if not found
;when found, base address is returned in hl:
;		: high addr :
;		:  low addr:
;	hl:	: length   :
;		:  char 1  :
;		   . . .
;		:  char len:
;	(list terminated by length > 15)
	lhld	sytop	;top symbol in table
	inx	h	;to low address
	inx	h	;to high address field
alook0:	mov	b,m	;high address
	dcx	h
	mov	c,m	;low address
	dcx	h	;.length
	mov	a,m	;get length
	cpi	16	;max length is 15
	jnc	alook2	;to stop the search
	push	h	;save current location in case matched
	cma		;1's complement of low(length)
	add	l	;add to hl
	mov	l,a
	mvi	a,0ffh	;1's complement of high(length)
	adc	h	;propagate carry for subtract
	mov	h,a	;hl is hl-length-1
;	now compare symbol address
	mov	a,e	;low of search address
	cmp	c	;-low of symbol address
	jnz	alook1	;skip if unequal
	mov	a,d
	sub	b	;skip if unequal
	jnz	alook1
;	symbol matched, return hl as symbol address
	pop	h
	inr	a	;difference was zero
	ret		;with non zero flag set
;
alook1:	;symbol not matched, look for next
	inx	sp
	inx	sp	;remove stacked address
	jmp	alook0	;for another search
;
;	symbol address not found
alook2:	xra	a
	ret		;with zero flag set
;
;
;	*********************************
;	*				*
;	*	Data Structures		*
;	*				*
;	*********************************
;
; D - structures
disloc:	ds	2	;display location
DISEND:	db	FALSE	;storage for end of display
dismax:	ds	2	;max value for current display
tdisp:	ds	2	;temp 16 bit location
DISTMP:	ds	2	;temp storage for 16bit add	
;
; G - structures
autou:	ds	1	;ff if auto "u" command in effect
gobrks:	ds	1	;number of breaks in go command
gobrk1:	ds	2	;primary break in go command
gobrk2:	ds	2	;secondary break in go command
pbloc:	ds	2	;pbtable location for auto u
pbcnt:	db	00	;permanent break temp counter
;
; H - structures
dtable:	;decimal division table
	dw	10000
	dw	1000
	dw	100
	dw	10
	dw	1
;
; R - structures
bias:	ds	2	;holds r bias value for load
sytop:	ds	2	;high symbol table address
mload:	ds	2	;max load address
dasm:	ds	1	;00 if dis/assem present, 01 if not
symsg:	db	cr,lf,'SYMBOLS',0
lmsg:	db	cr,lf,'NEXT MSZE  PC  END',cr,lf,0
DEFLOAD: ds	2	;holds the default read address
;
; T - structures
tmode:	ds	1	;trace mode
userbrk:ds	2	;user break address if non-zero
tracer:	ds	2	;trace count
;
; W - structures
WRTREC:	ds	2	;# of written records
WBEGIN:	ds	2		;Beginning address of write
WEND:	ds	2		;ending address of write
WRTMSG:	db	CR,LF,0
WRTMSG1: db	'h record(s) written.',0
;
; Common to all routines
;
lastexp:dw	0000	;last expression encountered
;
pbtrace:
	ds	1	;trace on for perm break
pbtable:
	rept	pbsize	;one for each element
	db	0	;counter
	ds	2	;address
	ds	1	;data
	endm
;	each perm table element takes the form:
;	low(count) high(count) low(addr) high(addr) data
;
;
negcom:	ds	1	;00 if normal command, ff if "-x"
wdisp:	ds	1	;00 if byte display, ff if word display
catno:	ds	1	;category number saved in nbrk
retloc:	ds	2	;return address to user from bdos
breaks:	ds	7	;#breaks/bkpt1/dat1/bkpt2/dat2
explist:ds	7	;count+(exp1)(exp2)(exp3)
nextcom:ds	2	;next location from command buffer
comlen:	db	csize	;max command length
curlen:	ds	1	;current command length
combuf:	ds	csize	;command buffer
;	temporary values used in "r" command share end of buffer
tfcb	equ	$-fcbl/2;holds name of symbol file during code load
;
	ds	ssize	;stack area
stack:
ploc	equ	stack-2	;pc in template
hloc	equ	stack-4	;hl
sloc	equ	stack-6	;sp
aloc	equ	stack-7	;a
floc	equ	stack-8	;flags
bloc	equ	stack-10	;bc
dloc	equ	stack-12;d,e
;
	nop		;for relocation boundary
	end
