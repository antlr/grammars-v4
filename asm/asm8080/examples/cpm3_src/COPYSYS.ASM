	title	'Copysys - updated sysgen program 6/82'
; System generation program
VERS	equ	30		;version x.x for CP/M x.x
;
;**********************************************************
;*							  *
;*							  *
;*		Copysys source code			  *
;*							  *
;*							  *
;**********************************************************
;
FALSE	equ	0
TRUE	equ	not FALSE
;
;
NSECTS	equ	26		;no. of sectors
NTRKS	equ	2		;no. of systems tracks
NDISKS	equ	4		;no. of disks drives
SECSIZ	equ	128		;size of sector
LOG2SEC	equ	7		;LOG2 128
SKEW	equ	2		;skew sector factor
;
FCB	equ	005Ch		;location of FCB
FCBCR	equ	FCB+32		;current record location
TPA	equ	0100h		;Transient Program Area
LOADP	equ	1000h		;LOAD Point for system
BDOS	equ	05h		;DOS entry point
BOOT	equ	00h		;reboot for system
CONI	equ	1h		;console input function
CONO	equ	2h		;console output function
SELD	equ	14		;select a disk
OPENF	equ	15 		;disk open function
CLOSEF	equ	16		;open a file
DWRITF	equ	21		;Write func
MAKEF	equ	22		;mae a file
DELTEF 	equ	19		;delete a file
DREADF	equ	20		;disk read function
DRBIOS	equ	50		;Direct BIOS call function
EIGHTY	equ	080h		;value of 80
CTLC	equ	'C'-'@'		;ConTroL C
Y	equ	89		;ASCII value of Y
;
MAXTRY	equ	01		;maximum number of tries
CR	equ	0Dh		;Carriage Return
LF	equ	0Ah		;Line Feed
STACKSIZE equ	016h		;size of local stack
;
WBOOT	equ	01		;address of warm boot
;
SELDSK	equ	9		;Bios func #9 SELect DiSK
SETTRK	equ	10		;BIOS func #10 SET TRacK
SETSEC	equ	11		;BIOS func #11 SET SECtor
SETDMA	equ	12		;BIOS func #12 SET DMA address
READF	equ	13		;BIOS func #13 READ selected sector
WRITF	equ	14		;BIOS func #14 WRITe selected sector

;
	org	TPA		;Transient Program Area
	jmp	START
	dw	0,0,0,0,0,0,0,0
	dw	0,0,0,0,0,0,0,0
	dw	0,0,0,0,0,0,0,0
	dw	0,0,0,0,0,0,0,0
	dw	0,0,0,0,0
	db	0,0,0
	db	'COPYRIGHT 1982, '
	db	'DIGITAL RESEARCH'
	db	'151282'
	db	0,0,0,0
	db	'654321'
;
; Translate table-sector numbers are translated here to decrease
; the systen tie for missed sectors when slow controllers are
; involved.  Translate takes place according to the "SKEW" factor
; set above.
;
OST:	db	NTRKS		;operating system tracks
SPT:	db	NSECTS		;sectors per track
TRAN:
TRELT	set	1
TRBASE	set	1
	rept	NSECTS
	db	TRELT		;generate first/next sector
TRELT	set	TRELT+SKEW
	if	TRELT gt NSECTS
TRBASE	set	TRBASE+1
TRELT	set	TRBASE
	endif
	endm
;
; Now leave space for extensions to translate table
;
	if	NSECTS lt 64
	rept	64-NSECTS
	db	0
	endm
	endif
;
; Utility subroutines
;
MLTBY3:
;multiply the contents of regE to get jmp address
	mov	a,e		;Acc = E
	sui	1
	mov	e,a		;get ready for multiply
	add	e
	add	e
	mov	e,a
	ret			;back at it
;
SEL:
	sta	TEMP
	lda	V3FLG
	cpi	TRUE
	lda	TEMP
	jnz	SEL2
;
	sta	CREG		;CREG = selected register
	lxi	h,0000h
	shld	EREG		;for first time

	mvi	a,SELDSK
	sta	BIOSFC		;store it in func space
	mvi	c,DRBIOS
	lxi	d,BIOSPB
	jmp	BDOS
SEL2:
	mov	c,a
	lhld	WBOOT
	lxi	d,SELDSK
	call	MLTBY3
	dad	d
	pchl
;
TRK:
; Set up track
	sta	TEMP
	lda	V3FLG
	cpi	TRUE
	lda	TEMP
	jnz	TRK2

;
	mvi	a,00h
	sta	BREG		;zero out B register
	mov	a,c		;Acc = track #
	sta	CREG		;set up PB
	mvi	a,SETTRK	;settrk func #
	sta	BIOSFC
	mvi	c,DRBIOS
	lxi	d,BIOSPB
	jmp	BDOS
TRK2:
	lhld	WBOOT
	lxi	d,SETTRK
	call	MLTBY3
	dad	d
	pchl			;gone to set track
;
SEC:
; Set up sector number
	sta	TEMP
	lda	V3FLG
	cpi	TRUE
	lda	TEMP
	jnz	SEC2
;
	mvi	a,00h
	sta	BREG		;zero out BREG
	mov	a,c		; Acc = C
	sta	CREG		;CREG = sector #
	mvi	a,SETSEC
	sta	BIOSFC		;set up bios call
	mvi	c,DRBIOS
	lxi	d,BIOSPB
	jmp	BDOS
SEC2:
	lhld	WBOOT
	lxi	d,SETSEC
	call	MLTBY3
	dad	d
	pchl
;
DMA:
; Set DMA address to value of BC
	sta	TEMP
	lda	V3FLG
	cpi	TRUE
	lda	TEMP
	jnz	DMA2
;
	mov	a,b		;
	sta	BREG		;
	mov	a,c		;Set up the BC
	sta	CREG		;register pair
	mvi	a,SETDMA	;
	sta	BIOSFC		;set up bios #
	mvi	c,DRBIOS
	lxi	d,BIOSPB
	jmp	BDOS
DMA2:
	lhld	WBOOT
	lxi	d,SETDMA
	call	MLTBY3
	dad	d
	pchl
;
READ:
; Perform read operation
	sta	TEMP
	lda	V3FLG
	cpi	TRUE
	lda	TEMP
	jnz	READ2
;
	mvi	a,READF
	sta	BIOSFC
	mvi	c,DRBIOS
	lxi	d,BIOSPB
	jmp	BDOS
READ2:
	lhld	WBOOT
	lxi	d,READF
	call	MLTBY3
	dad	d
	pchl
;
WRITE:
; Perform write operation
	sta	TEMP
	lda	V3FLG
	cpi	TRUE
	lda	TEMP
	jnz	WRITE2
;
	mvi	a,WRITF
	sta	BIOSFC		;set up bios #
	mvi	c,DRBIOS
	lxi	d,BIOSPB
	jmp	BDOS
WRITE2:
	lhld	WBOOT
	lxi	d,WRITF
	call	MLTBY3
	dad	d
	pchl
;
MULTSEC:
; Multiply the sector # in rA by the sector size
	mov	l,a
	mvi	h,0		;sector in hl
	rept	LOG2SEC
	dad	h
	endm
	ret			;with HL - sector*sectorsize
;
GETCHAR:
; Read console character to rA
	mvi	c,CONI
	call	BDOS
; Convert to upper case
	cpi	'A' or 20h
	rc
	cpi	('Z' or 20h)+1
	rnc
	ani	05Fh
	ret
;
PUTCHAR:
; Write character from rA to console
	mov	e,a
	mvi	c,CONO
	call	BDOS
	ret
;
CRLF:
; Send Carriage Return, Line Feed
	mvi	a,CR
	call	PUTCHAR
	mvi	a,LF
	call	PUTCHAR
	ret
;

CRMSG:
; Print message addressed by the HL until zero with leading CRLF
	push	d
	call	CRLF
	pop	d		;drop through to OUTMSG
OUTMSG:
	mvi	c,9
	jmp	BDOS
;
SELCT:
; Select disk given by rA
	mvi	c,0Eh
	jmp	BDOS
;
DWRITE:
; Write for file copy
	mvi	c,DWRITF
	jmp	BDOS
;
DREAD:
; Disk read function
	mvi	c,DREADF
	jmp	BDOS
;
OPEN:
; File open function
	mvi	c,OPENF
	jmp	BDOS
;
CLOSE:
	mvi	c,CLOSEF
	jmp	BDOS
;
MAKE:
	mvi	c,MAKEF
	jmp	BDOS
;
DELETE:	
	mvi	c,DELTEF
	jmp	BDOS
;
;
;
DSTDMA:
	mvi	c,26
	jmp	BDOS
;
SOURCE:
	lxi	d,GETPRM	;ask user for source drive
	call	CRMSG
	call	GETCHAR		;obtain response
	cpi	CR		;is it CR?
	jz	DFLTDR		;skip if CR only
	cpi	CTLC		;isit ^C?
	jz	REBOOT
;
	sui	'A'		;normalize drive #
	cpi	NDISKS		;valid drive?
	jc	GETC		;skip to GETC if so
;
; Invalid drive
	call	BADDISK		;tell user bad drive
	jmp	SOURCE		;try again
;
GETC:
; Select disk given by Acc.
	adi	'A'
	sta	GDISK		;store source disk
	sui	'A'
	mov	e,a		;move disk into E for select func
	call	SEL		;select the disk
	jmp	GETVER
;
DFLTDR:
	mvi	c,25		;func 25 for current disk
	call	BDOS		;get curdsk
	adi	'A'
	sta	GDISK
	call	CRLF
	lxi	d,VERGET
	call	OUTMSG
	jmp	VERCR
;
GETVER:	
; Getsys set r/w to read and get the system
	call	CRLF
	lxi	d,VERGET	;verify source disk
	call	OUTMSG
VERCR:	call	GETCHAR
	cpi	CR
	jnz	REBOOT		;jmp only if not verified
	call	CRLF
	ret
;
DESTIN:
	lxi	d,PUTPRM	;address of message
	call	CRMSG		;print it
	call	GETCHAR		;get answer
	cpi	CR
	jz	REBOOT		;all done
	sui	'A'
	cpi	NDISKS			;valid disk
	jc	PUTC
;
; Invalid drive
	call	BADDISK		;tell user bad drive
	jmp	PUTSYS		;to try again
;
PUTC:
; Set disk fron rA
	adi	'A'
	sta	PDISK		;message sent
	sui	'A'
	mov	e,a		;disk # in E
	call	SEL		;select destination drive
; Put system, set r/w to write
	lxi	d,VERPUT	;verify dest prmpt
	call	CRMSG		;print it out
	call	GETCHAR		;retrieve answer
	cpi	CR	
	jnz	REBOOT		;exit to system if error
	call	CRLF
	ret
;
;
GETPUT:
; Get or put CP/M (rw = 0 for read, 1 for write)
; disk is already selected
	lxi	h,LOADP		;load point in RAM for DMA address
	shld	DMADDR
;
;
;

;
; Clear track 00
	mvi	a,-1		;
	sta	TRACK
;
RWTRK: 
; Read or write next track
	lxi	h,TRACK
	inr	m		;track = track+1
	lda	OST		;# of OS tracks
	cmp	m		;=track # ?
	jz	ENDRW		;end of read/write
;
; Otherwise not done
	mov	c,m		;track number
	call	TRK		;set to track
	mvi	a,-1		;counts 0,1,2,...,25
	sta	SECTOR
;
RWSEC:
; Read or write a sector
	lda	SPT		;sectors per track
	lxi	h,SECTOR	
	inr	m		;set to next sector
	cmp	m		;A=26 and M=0,1,..,25
	jz	ENDTRK
;
; Read or write sector to or from current DMA address
	lxi	h,SECTOR
	mov	e,m		;sector number
	mvi	d,0		;to DE
	lxi	h,TRAN	
	mov	b,m		;tran(0) in B
	dad	d		;sector translated
	mov	c,m		;value to C ready for select
	push	b		;save tran(0)
	call 	SEC
	pop	b		;recall tran(0),tran(sector)
	mov	a,c		;tran(sector)
	sub	b		;--tran(sector)
	call	MULTSEC		;*sector size
	xchg			;to DE
	lhld	DMADDR		;base DMA
	dad	d
	mov	b,h
	mov	c,l		;to set BC for SEC call
	call	DMA		;dma address set from BC
	xra	a
	sta	RETRY		;to set zero retries
;
TRYSEC:
; Try to read or write current sector
	lda	RETRY
	cpi	MAXTRY
	jc	TRYOK
;
; Past MAXTRY, message and ignore
	lxi	d,ERRMSG
	call	OUTMSG
	call	GETCHAR
	cpi	CR
	jnz	REBOOT
;
; Typed a CR, ok to ignore
	call	CRLF
	jmp	RWSEC
;
TRYOK:
; Ok to tyr read write
	inr	a
	sta	RETRY	
	lda	RW
	ora	a
	jz	TRYREAD
;
; Must be write
	call	WRITE
	jmp	CHKRW
TRYREAD:
	call	READ
CHKRW:
	ora	a
	jz	RWSEC		;zero flag if read/write ok
;
;Error, retry operation
	jmp	TRYSEC
;
; End of track
ENDTRK:
	lda	SPT		;sectors per track
	call	MULTSEC		;*secsize
	xchg			; to DE
	lhld	DMADDR		;base dma for this track
	dad	d		;+spt*secsize
	shld	DMADDR		;ready for next track
	jmp	RWTRK		;for another track
;
ENDRW:
; End of read or write
	ret
;
;*******************
;*
;*	MAIN ROUTINE
;*
;*
;*******************
;
START:

	lxi	sp,STACK
	lxi	d,SIGNON
	call	OUTMSG
;
;get version number to check compatability
	mvi	c,12		;version check
	call	BDOS
	mov	a,l		;version in Acc
	cpi	30h		;version 3 or newer?
	jc	OLDRVR		;
	mvi	a,TRUE
	sta	V3FLG		;
	jmp	FCBCHK
OLDRVR:	
	mvi	a,FALSE
	sta	V3FLG
;

; Check for default file liad instead of get
FCBCHK:	lda	FCB+1		;blank if no file
	cpi	' '
	jz	GETSYS		;skip to system message
	lxi	d,FCB		;try to open it
	call	OPEN
	inr	a		;255 becomes 00
	jnz	RDOK
;
; File not present
	lxi	d,NOFILE
	call	CRMSG
	jmp	REBOOT
;
;file present
RDOK:
	xra	a
	sta	FCBCR		;current record = 0
	lxi	h,LOADP
RDINP:
	push	h
	mov	b,h
	mov	c,l
	call	DMA		;DMA address set
	lxi	d,FCB		;ready fr read
	call	DREAD
	pop	h		;recall
	ora	a		;00 if read ok
	jnz	PUTSYS		;assume eof if not
; More to read continue
	lxi	d,SECSIZ 
	dad	d		;HL is new load address
	jmp	RDINP
;
GETSYS:
	call	SOURCE		;find out source drive
;
	xra	a		;zero out a
	sta	RW		;RW = 0 to signify read
	call	GETPUT		;get or read system
	lxi	d,DONE		;end message of get or read func
	call	OUTMSG		;print it out
;
; Put the system
PUTSYS:
	call	DESTIN		;get dest drive
;
	lxi	h,RW		;load address
	mvi	m,1
	call	GETPUT		;to put system back on disk
	lxi	d,DONE
	call	OUTMSG		;print out end prompt
;
;	FILE COPY FOR CPM.SYS
;
CPYCPM:
; Prompt the user for the source of CP/M3.SYS
;
	lxi	d,CPYMSG	;print copys prompt
	call	CRMSG		;print it
	call	GETCHAR		;obtain reply
	cpi	Y		;is it yes?
	jnz	REBOOT		;if not exit
				;else
;
;
	mvi	c,13		;func # for reset
	call	BDOS		;
	inr	a

	lxi	d,ERRMSG
	cz	FINIS
;
	call	SOURCE		;get source disk for CPM3.SYS
CNTNUE:
	lda	GDISK		;Acc = source disk
	sui	'A'
	mvi	d,00h
	mov	e,a		;DE = selected disk
	call	SELCT
; now copy the FCBs
	mvi	c,36		;for copy
	lxi	d,SFCB		;source file
	lxi	h,DFCB		;destination file
MFCB:

	ldax	d
	inx	d		;ready next
	mov	m,a
	inx	h		;ready next dest
	dcr	c		;decrement coun
	jnz	MFCB
;
	lda	GDISK		;Acc = source disk
	sui	40h		;correct disk
	lxi	h,SFCB
	mov	m,a		;SFCB has source disk #
	lda 	PDISK		;get the dest. disk
	lxi	h,DFCB		;
	sui	040h		;normalize disk
	mov	m,a
;
	xra	a		;zero out a
	sta	DFCBCR		;current rec = 0
;
; Source and destination fcb's ready
;
	lxi	d,SFCB		;
	call	OPEN		;open the file
	lxi	d,NOFILE	;error messg
	inr	a		;255 becomes 0
	cz	FINIS		;done if no file
;
; Source file is present and open
	lxi	d,LOADP		;get DMA address
	xchg			;move address to HL regs
	shld	BEGIN		;save for begin of write
;
	lda	BEGIN		;get low byte of
	mov	l,a		;DMA address into L
	lda	BEGIN+1		;
	mov	h,a		;into H also
COPY1:
	xchg			;DE = address of DMA
	call	DSTDMA		;
;
	lxi	d,SFCB		;
	call	DREAD		;read next record
	ora	a		;end of file?
	jnz	EOF		;skip write if so
;
	lda	CRNREC
	inr	a		;bump it
	sta	CRNREC
;
	lda	BEGIN
	mov	l,a
	lda	BEGIN+1
	mov	h,a
	lxi	d,EIGHTY
	dad	d		;add eighty to begin address
	shld	BEGIN
	jmp	COPY1		;loop until EOF
;
EOF:
	lxi	d,DONE
	call	OUTMSG
;
COPY2:
	call	DESTIN		;get destination drive for CPM3.SYS
	lxi	d,DFCB		;set up dest FCB
	xchg	
	lda	PDISK
	sui	040h		;normalize disk
	mov	m,a		;correct disk for dest
	xchg			;DE = DFCB
	call	DELETE		;delete file if there
;
	lxi	d,DFCB		;
	call	MAKE		;make a new one
	lxi	d,NODIR
	inr	a		;check directory space
	cz	FINIS		;end if none
;
	lxi	d,LOADP
	xchg
	shld	BEGIN
;
	lda	BEGIN
	mov	l,a
	lda	BEGIN+1
	mov	h,a
LOOP2:
	xchg
	call	DSTDMA
	lxi	d,DFCB
	call	DWRITE
	lxi	d,FSPACE
	ora	a
	cnz	FINIS
	lda	CRNREC
	dcr	a
	sta	CRNREC
	cpi	0
	jz	FNLMSG
	lda	BEGIN
	mov	l,a
	lda	BEGIN+1
	mov	h,a
	lxi	d,EIGHTY
	dad	d
	shld	BEGIN
	jmp	LOOP2
; Copy operation complete
FNLMSG:
	lxi	d,DFCB
	mvi	c,CLOSEF
	call	BDOS
;
	lxi	d,DONE
;
FINIS:
; Write message given by DE, reboot
	call	OUTMSG
;
REBOOT:
	mvi	c,13
	call	BDOS
	call	CRLF
	jmp	BOOT
;
BADDISK:
	lxi	d,QDISK
	call	CRMSG
	ret
;****************************
;*
;*
;*	DATA STRUCTURES     
;*
;*
;****************************
;
BIOSPB:
; BIOS Parameter Block
BIOSFC:	db	0		;BIOS function number
AREG:	db	0		;A register contents
CREG:	db	0		;C register contents
BREG:	db	0		;B register contents
EREG:	db	0		;E register contents
DREG:	db	0		;D register contents
HLREG:	dw	0		;HL register contents
;
SFCB:
DR:	ds	1
F1F8:	db	'CPM3    '
T1T3:	db	'SYS'
EXT:	db	0
CS:	db	0
RS:	db	0
RCC:	db	0
D0D15:	ds	16
CCR:	db	0
R0R2:	ds	3
;
DFCB:	ds	36
DFCBCR	equ	DFCB+32
;
;
V3FLG:	db	0		;flag for version #
TEMP:	db	0
SDISK:	ds	1		;selected disk
BEGIN:	dw	0
DFLAG:	db	0
TRACK:	ds	1		;current track
CRNREC:	db	0		;current rec count
SECTOR:	ds	1		;current sector
RW:	ds	1		;read if 0 write if 1
DMADDR:	ds	2		;current DMA address
RETRY:	ds	1		;number of tries on this sector
SIGNON:	db	'CP/M 3 COPYSYS - Version '
	db	VERS/10+'0','.',VERS mod 10 +'0'
	db	'$'
GETPRM:	db	'Source drive name (or return for default) $'
VERGET:	db	'Source on '
GDISK:	ds	1
	db	' then type return $'
PUTPRM:	db	'Destination drive name (or return to reboot) $'
VERPUT:	db	'Destination on '
PDISK:	ds	1
	db	' then type return $'
CPYMSG:	db	'Do you wish to copy CPM3.SYS? $'
DONE:	db	'Function complete$'
;
; Error messages......
;
QDISK:	db	'ERROR: Invalid drive name (Use A, B, C, or D)$'
NOFILE:	db	'ERROR: No source file on disk.$'
NODIR:	db	'ERROR: No directory space.$'
FSPACE:	db	'ERROR: Out of data space.$'
WRPROT:	db	'ERROR: Write protected?$'
ERRMSG: db	'ERROR: Possible incompatible disk format.'
	db	CR,LF,' Type return to ignore.$' 
CLSERR:	db	'ERROR: Close operation failed.$'
;
	ds	STACKSIZE * 3
STACK:
	end
