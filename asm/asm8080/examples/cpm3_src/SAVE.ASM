	title	'SAVE.RSX - CP/M 3.0 save routine.  July 1982'
;	*************************************************
;	*
;	*	Title:	SAVE.RSX	Resident System eXtension       
;	*	Date:	7/28/82
;	*	Author:	Thomas J. Mason
;	*
;	*	Modified:
;	*	11/30/82 - Thomas J. Mason
;	*	Added trap for function 60 to fix PUT and SAVE
;	*	bios vector mods.
;	*
;	*********************************************************
;
;	Copyright (c) 1982
;	Digital Research
;	PO Box	579
;	Pacific Grove, Ca.  93950
;
TRUE	equ	0FFFFh
FALSE	equ	not TRUE
;
; BIOS and BDOS Jump vectors
;
WBOOT	equ	0
WBTADR	equ	1	;address of boot in BIOS
BDOS	equ	5	;BDOS jump vector
BDOSAD	equ	6	;location of instructions
DFCB	equ	05Ch	;default FCB
;
; BDOS Function calls 
;
BDOSAD	equ	6		;BDOS jump address
PSTRING	equ	9		;print string
BUFIN	equ	10		;console buffer input
CFILE	equ	16		;file close
DFILE	equ	19		;file delete
WFILE	equ	21		;file write
MFILE	equ	22		;make file
SETDMA	equ	26		;set DMA function
BDOSER	equ	45		;Set BDOS error mode
GETSCB	equ	49		;get/set scb func #
LDRSX	equ	59		;function for RSX load
CALRSX	equ	60		;call rsx func #
CONMOD	equ	109		;GET/SET Console Mode
;
; Non Printable ASCII characters
;
CTL$C	equ	03	;CONTROL-C
CR	equ	13	;ASCII Carrige Return
LF	equ	10	;ASCII Line Feed
;
VERSION	equ	30
;
; Buffer size
;
CONMAX	equ	13	;console buffer maximum
STKSZE	equ	010h	;size fo stack
SCBOST	equ	068h	;page boundary + to jmp instr
RETDSP	equ	0FEh	;RETurn and DiSPlay mode
JUMP	equ	0C3h	;opcode for jump
LXIH	equ	21h	;lxi instr to poke
BSNLY	equ	07Fh	;restore bios jump table only
CMMON	equ	0F9h	;offset of common memory base from pg. bound
;
;	*********************************
;	*				*
;	*	The Save Program	*
;	*				*
;	*********************************
;
	db	0,0,0,0,0,0
	jmp	PREFIX
NEXTJ:
	db	JUMP		;jump
NEXT:
	db	0,0		;next module in line
PREV:
	dw	5		;previous, initialized to 5
STKYBT:	db	00h		;for warm start
	db	0
	db	'SAVE    '
	ds	3
;
;
; This is the check performed every time the BDOS is
; called to see if the RSX is to be invoked
;
PREFIX:
	mov	a,c	;set up for compare
	cpi	CALRSX
	jnz	GETGOING

	push	b
	push	d
	push	h
	lxi	h,0000h		;zero out HL
	dad	d		; <HL> -> RSXPB
	mov	a,m		;get the byte
	cpi	160		; sub function defined

	pop	h
	pop	d
	pop	b
	jz	GOODBYE		;remove this RSX

GETGOING:
;
	cpi	LDRSX	;do the compare
	jz	START
	lhld	NEXT		;get address for continue
	pchl			;get going.....
;
;
;
START:
;
; They are equal so get the BIOS address to point here
; in case of a Func 0 call
;
	push	b		;save state
	push	d		; of registers
;
; check for jump byte before the SCB
	call	GETSET$SCB
	shld	SCBADR		;save address for later
;
	mvi	l,CMMON+1	;offset into scb to check BIOS
	mov	a,m		;get byte
	ora	a		;check for zero
	mvi	a,FALSE		;store for insurance
	sta	CHGJMP		;non-banked = FALSE
	jz	NBNKED		;high byte zero if non-banked
;
	lhld	SCBADR		;restor SCB
	mvi	l,SCBOST	;offset from page for instr
	mov	a,m		;get byte
	cpi	JUMP		;is it a jump?
	jnz	MORRSX		;we are not alone
	mvi	a,TRUE
	sta	CHGJMP		;set flag
	mvi	m,LXIH		;put in lxi h,xxxx mnemonic
;
MORRSX:
;	continue with processing
NBNKED:
;
;
	lhld	WBTADR		;get address at 01h
	inx	h		;now points to address of jmp xxxx
	mov	a,m		;get low order byte
	sta	BIOSAD
	inx	h		;next byte
	mov	a,m
	sta	BIOSAD+1	;high order byte
;
; Now poke the BIOS address to point to
; the save routine.
;
	lxi	d,BEGIN		;begining of routine
	mov	m,d
	dcx	h		;point back to first byte
	mov	m,e		;low order
;
	mvi	c,BDOSER	;now set BDOS errormode
	mvi	e,RETDSP	;to trap any hard
	call	BDOS		;errors
;
;
	pop	d
	pop	b
	lhld	NEXT
	pchl			;continue on
;
BEGIN:
; Start of the save routine
; Notify the user which program is running
;
	lxi	sp,STACK	;initialize stack
	lxi	d,SIGNON	;prompt
	call	PSTR
;
; Get the file from the user
;
FLEGET:
	lxi	d,FLEPRMPT	;ask for file name
	call	PSTR
	call	GETBUF
; zero at end of string for parser
	lxi	h,CONBUF-1	;address of #
	mov	a,m		;get it
	cpi	0
	jz	REPLCE
	inx	h		;HL->CONBUF
	mvi	d,0		;zero out high order
	mov	e,a		;fill low
	dad	d		;add to h
	mvi	m,00		;zero out byte for parse
	push	h
;
;
	call	PARSE
	mov	a,h
	cpi	0FFh
	jz	FLEGET
;
	pop	h		;get end of string address back
	inx	h
	mvi	m,'?'		;put in question mark
	inx	h		;bump
	mvi	m,' '		;blank in string
	inx	h		;bump
	mvi	m,'$'		;end of string
;
	mvi	c,17		;Search for first
	lxi	d,DFCB
	call	BDOS		;find it
	inr	a		;bump Acc
	jz	FLECLR		;file no present skip prompt
;
	lxi	d,DELFLE
	call	PSTR		;print out delete prompt
	lxi	d,CONBUF	;buffer address
	call	PSTR		;print out filename
	call	GETBUF		;get answer
	call	GNC		;get the next char
	cpi	'Y'		;is it yes
	jnz	FLEGET		;another name if not
;
; Delete any existing file, then make a new one
FLECLR:
	mvi	c,DFILE		;file delete func
	lxi	d,DFCB		;default FCB
	call	BDOS		;real BDOS call
;
	mvi	a,0
	lxi	h,07ch		;M -> record count in FCB
	mov	m,a		;zero out record count
;
	mvi	c,MFILE		;make file function
	lxi	d,DFCB		;default FCB
	call	BDOS
; Get the address of start of write
;
STRADD:
	lxi	d,SPRMPT	;first address
	call	PSTR
	call	GETBUF
;
	lda	BUFFER+1	;get # of chars read
	cpi	0
	jz	STRADD
;
	call	SCANAD		;get address
	jc	STRADD
;
	shld	SADDR		;store in SADDR
;
; Get the finish address
ENDADD:
	lxi	d,FPRMPT	;load prompt
	call	PSTR		;print
	call	GETBUF		;read in
;
	lda	BUFFER+1
	cpi	0
	jz	ENDADD
;
	call	SCANAD		;get finish address
	jc	ENDADD
;
	shld	FADDR		;store it
	xchg
	lhld	SADDR
	xchg
;
	call	CHECK
	jc	STRADD
;
;
	lhld	SADDR		;beginning DMA address
	xchg			;DE=DMA address
;
; Write the first record then check the beginning address
; if DMA address ends up larger exit
;
WLOOP:
	call	WFLAG
	push	d		;save DMA address
 	mvi	c,SETDMA
	call	BDOS		;set DMA address
;
	mvi	c,WFILE	
	lxi	d,DFCB
	call	BDOS		;write
;
; Check for directory space on disk for extents
	lxi	d,NODIR
	cpi	01h		;no more directory
	jz	FINIS
;
; CHECK data block error
	lxi	d,NOBLK
	cpi	02h
	jz	FINIS		;out of disk space!
; final check
	ora	a		;if bad write occured...
	jnz	REPLCE		;restore BIOS address
;
; Write OK now check write address
	pop	d		;get DMA address
	lxi	h,080h
	dad	d
	xchg
	lhld	FADDR		;HL=end of write
;
	call	CHECK
;
	lda	ONEFLG
	cpi	TRUE
	jnz	WLOOP		;WLOOP if not done
;
; Else, Close file and print out ending prompt
CLOSE:
	mvi	c,CFILE		;close function
	lxi	d,DFCB		;get filename
	call	BDOS
;
	inr	a		;check for close error
	lxi	d,CERROR
	jz	FINIS		;maybe write protected
;
;good copy
	lxi	d,ENDMSG
FINIS:
	call	PSTR
;
; Replace the BIOS Address to correct one
REPLCE:
	lhld	BIOSAD	;HL=BIOS warm jump
	xchg		;DE="     "    "
	lhld	WBTADR
	inx	h
	mov	m,e
	inx	h
	mov	m,d
;
GOODBYE:
	mvi	a,0FFh
	sta	STKYBT		;change sticky byte for 
;				; removal of RSX
;
; check to see if JMP changed for BANKED system
	lda	CHGJMP
	cpi	TRUE		;has it been done?
	jnz	CHGBIOS
	lhld	SCBADR		;retreive SCB address
	mvi	l,SCBOST	;points to page + offset
	mvi	m,JUMP		;restore original code
;
CHGBIOS:
	mvi	c,13		;reset the disk system
	call	BDOS
;
	mvi	c,0		;set up for wboot
	call	BDOS
;****************************************
;*					*
;*	 Logical end of the program	*
;*					*
;****************************************
;
GETSET$SCB:
	mvi	c,GETSCB
	lxi	d,SCBPB
	call	BDOS
	ret
;
WFLAG:
	mvi	a,FALSE
	sta	ONEFLG
	lda	RSLT+1
	cpi	00h
	rnz	
	lda	RSLT
	cpi	080h
	jc	WFLAG1
	jz	WFLAG1
	ret
;
WFLAG1:
	mvi	a,TRUE
	sta	ONEFLG
	ret
;
;
;
CHECK:
; Subtract the two to find out if finished
	mov	a,l		;low order
	sub	e		;subtraction
	sta	RSLT
	mov	a,h		;now ...
	sbb	d		;high order subtraction	
	sta	RSLT+1		;saved
	ret
;
GETBUF:
;buffer input routine
;
	lxi	h,CONBUF	;address of buffer
	shld	NEXTCOM		;store it
	mvi	c,BUFIN
	lxi	d,BUFFER
	call	BDOS
	ret
;
PSTR:
; String output routine for messages
;
	mvi	c,PSTRING
	call	BDOS
	ret
;
PARSE:
; General purpose parser
;
; Filename = [d:]file[.type][;password]
;
; FCB assignments
;
;	0	=> drive, 0=default, 1=A, 2=B
;	1-8	=> file, converted to upper case,
;		   padded with blanks
;	9-11	=> type, converted to upper case,
;		   padded with blanks
;	12-15	=> set to zero
;	16-23	=> passwords, converted to upper case,
;		   padded with blanks
;	24-25	=> address of password field in "filename",
;		   set to zero if password length=0.
;	26	=> length of password (0-8)
;
; Upon return, HL is set to FFFFh if BC locates
;		   an invalid file name;
; otherwise, HL is set to 0000h if the delimiter
;		   following the file name is a 00h (null)
;		   or a 0Dh (CR);
; otherwise, HL is set to the address of the delimiter
;		   following the file name.
;
;
	lxi	h,0
	push	h
	push	h
	lxi	d,CONBUF	;set up source address
	lxi	h,DFCB		;set up dest address
	call	DEBLNK		;scan the blanks
	call	DELIM		;check for delimeter
	jnz	PARSE1
	mov	a,c
	ora	a
	jnz	PARSE9
	mov	m,a
	jmp	PARSE3
;
PARSE1:
	mov	b,a
	inx	d
	ldax	d
	cpi	':'
	jnz	PARSE2
;
	mov	a,b
	sui	'A'
	jc	PARSE9
	cpi	16
	jnc	PARSE9
	inr	a
	mov	m,a
	inx	d
	call	DELIM
	jnz	PARSE3
	cpi	'.'
	jz	PARSE9
	cpi	':'
	jz	PARSE9
	cpi	';'
	jz	PARSE9
	jmp	PARSE3
;
PARSE2:
	dcx	d
	mvi	m,0
PARSE3:
	mvi	b,8
	call	SETFLD
	mvi	b,3
	cpi	'.'
	jz	PARSE4
	call	PADFLD
	jmp	PARSE5
;
PARSE4:
	inx	d
	call	SETFLD
PARSE5:
	mvi	b,4
PARSE6:
	inx	h
	mvi	m,0
	dcr	b
	jnz	PARSE6
	mvi	b,8
	cpi	';'
	jz	PARSE7
	call	PADFLD
	jmp	PARSE8
PARSE7:
	inx	d
	call	PWFLD
PARSE8:
	push	d
	call	DEBLNK
	call	DELIM
	jnz	PARSE81
	inx	sp
	inx	sp
	jmp	PARSE82
PARSE81:
	pop	d
PARSE82:
	mov	a,c
	ora	a
	pop	b
	mov	a,c
	pop	b	
	inx	h
	mov	m,c
	inx	h
	mov	m,b
	inx	h
	mov	m,a
	xchg
	rnz
	lxi	h,0
	ret
PARSE9:
	pop	h
	pop	h
	lxi	h,0FFFFh
	ret
;
SETFLD:
	call	DELIM
	jz	PADFLD
	inx	h
	cpi	'*'
	jnz	SETFD1
	mvi	m,'?'
	dcr	b
	jnz	SETFLD
	jmp	SETFD2
SETFD1:
	mov	m,a
	dcr	b
SETFD2:
	inx	d
	jnz	SETFLD
SETFD3:
	call	DELIM
	rz
	pop	h
	jmp	PARSE9
;
PWFLD:
	call	DELIM
	jz	PADFLD
	inx	sp
	inx	sp
	inx	sp
	inx	sp
	inx	sp
	inx	sp
	push	d
	push	h
	mvi	l,0
	xthl
	dcx	sp
	dcx	sp
PWFLD1:
	inx	sp
	inx	sp
	xthl
	inr	l
	xthl
	dcx	sp
	dcx	sp
	inx	h
	mov	m,a
	inx	d
	dcr	b
	jz	SETFD3
	call	DELIM
	jnz	PWFLD1
;
PADFLD:
	inx	h
	mvi	m,' '
	dcr	b
	jnz	PADFLD
	ret
;
DELIM:
	ldax	d
	mov	c,a
	ora	a
	rz
	mvi	c,0
	cpi	0Dh
	rz
	mov	c,a
	cpi	09h
	rz
	cpi	' '
	jc	DELIM2
	rz
	cpi	'.'
	rz
	cpi	':'
	rz
	cpi	';'
	rz
	cpi	'='
	rz
	cpi	','
	rz
	cpi	'/'
	rz
	cpi	'['
	rz
	cpi	']'
	rz
	cpi	'<'
	rz
	cpi	'>'
	rz
	cpi	'a'
	rc
	cpi	'z'+1
	jnc	DELIM1
	ani	05Fh
DELIM1:
	ani	07Fh
	ret
DELIM2:
	pop	h
	jmp	PARSE9
;
DEBLNK:
	ldax	d
	cpi	' '
	jz	DBLNK1
	cpi	09h
	jz	DBLNK1
	ret
DBLNK1:
	inx	d
	jmp	DEBLNK
; End of the Parser
;
; GET a character from the console buffer
GNC:
	push	h
	lxi	h,CONBUF-1	;get length
	mov	a,m
	ora	a		;zero?
	mvi	a,CR		;return with CR if so
	jz	GNCRET
	dcr	m		;lenght = length-1
	lhld	NEXTCOM		;next char address
	mov	a,m
	inx	h		;bump to next
	shld	NEXTCOM		;update
GNCRET:
	pop	h
TRANS:
	cpi	7Fh		;Rubout?
	rz
	cpi	('A' or 0100000b)
	rc
	ani	1011111b	; clear upper case bit
	ret
;
;
; Scan the buffer for the address read in ASCII from the terminal
;
SCANAD:
	lxi	d,00h		;zero out address
	push	d		;and save
;
	lda	CONBUF-1	;get character count
	cpi	05		;5 is too many
	jc	SCAN0
	stc			;set carry for routine
	jmp	SCNRET
SCAN0:
	call	GNC		;get a char
	cpi	CR		;end?
	jz	SCNRET		;to scnret if so
	cpi	'0'		;is it >0?
	jnc	SCAN01		;bad character
	jmp	SCNRET
SCAN01:
	cpi	'@'
	jnz	SCAN02		;bad character
	stc
	jmp	SCNRET		;return on bad file
SCAN02:
	jnc	SCAN1		;must be A-F
	sui	030h		;normalize 0-9
	jmp	SCAN2
SCAN1:
	cpi	'G'		;is it out of range?
	jc	SCAN11
	stc
	jmp	SCNRET
SCAN11:	
	sui	037h		;normalize
SCAN2:
	mov	l,a		;character in low of DE
	lda	CONBUF-1	;get # left
	adi	1		;readjust
	mov	c,a
	mvi	h,00		;zero out high order
SCAN3:
	dcr	c		;dec to set flag
	jz	SCAN4		;were done
	dad	h		;shift 1bit left
	dad	h		;same
	dad	h		;same
	dad	h		;finally
	jmp	SCAN3		;back for more
;
SCAN4:
	pop	d		;ready for or
	mov	a,d		;high order
	ora	h		;
	mov	d,a
	mov	a,e		;low order
	ora	l		;ORed
	mov	e,a		;back
	push	d		;save
	jmp	SCAN0		;get more characters
SCNRET:
	pop	d		;hl = address
	xchg			;DE->HL
	ret
;
;
;	*********************************
;	*				*
;	*	Data Structures		*
;	*				*
;	*********************************
;
SCBPB:
	db	03Ah	;SCB address
	db	0
;
SADDR:	dw	0		;write start address
FADDR:	dw	0		;write finish address
BIOSAD:	dw	0		;WarmBOOT bios address
NEXTCOM: dw	0		;address of next character to read
ONEFLG:	db	0
RSLT:	dw	0
CHGJMP	db	FALSE
;
SCBADR:	dw	0		;Scb address
;
BIOSMD:	db	0		;if non-zero change LXI @jmpadr to
				;JUMP when removed.
;
BUFFER:	db	CONMAX
	db	0		;# of console characters read
CONBUF:	ds	CONMAX
;
SIGNON:	db	CR,LF,'CP/M 3 SAVE - Version ',VERSION/10+'0','.',VERSION mod 10+'0','$'
FLEPRMPT: db	CR,LF,'Enter file '
	db	'(type RETURN to exit): $'
DELFLE:	db	CR,LF,'Delete $'
SPRMPT:	db	CR,LF,'Beginning hex address $'
FPRMPT:	db	CR,LF,'Ending hex address    $'
ENDMSG:	db	CR,LF,'$'
;
; Error messages......
CERROR:	db	CR,LF,'ERROR: Bad close.$'
NODIR:	db	CR,LF,'ERROR: No directory space.$'
NOBLK:	db	CR,LF,'ERROR: No disk space.$'
;
; Stack for program
	ds	STKSZE
STACK:
	end		;Physical end of program
