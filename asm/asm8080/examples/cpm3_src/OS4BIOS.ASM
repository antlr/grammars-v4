;	MDS-800 I/O Drivers for CP/M 2.2
;	(four drive single density version)
;
;	Version 2.2 February, 1980
;
vers	equ	22	;version 2.2
;
;	Copyright (c) 1980
;	Digital Research
;	Box 579, Pacific Grove
;	California, 93950
;
;
true	equ	0ffffh	;value of "true"
false	equ	not true	;"false"
test	equ	false	;true if test bios
;
	if	test
bias	equ	03400h	;base of CCP in test system
	endif
	if	not test
bias	equ	0000h	;generate relocatable cp/m system
	endif
;
patch	equ	1600h
;
	org	patch
cpmb	equ	$-patch	;base of cpm console processor
bdos	equ	806h+cpmb	;basic dos (resident portion)
cpml	equ	$-cpmb	;length (in bytes) of cpm system
nsects	equ	cpml/128	;number of sectors to load
offset	equ	2	;number of disk tracks used by cp/m
cdisk	equ	0004h	;address of last logged disk on warm start
buff	equ	0080h	;default buffer address
retry	equ	10	;max retries on disk i/o before error
;
;	perform following functions
;	boot	cold start
;	wboot	warm start (save i/o byte)
;	(boot and wboot are the same for mds)
;	const	console status
;		reg-a = 00 if no character ready
;		reg-a = ff if character ready
;	conin	console character in (result in reg-a)
;	conout	console character out (char in reg-c)
;	list	list out (char in reg-c)
;	punch	punch out (char in reg-c)
;	reader	paper tape reader in (result to reg-a)
;	home	move to track 00
;
;	(the following calls set-up the io parameter block for the
;	mds, which is used to perform subsequent reads and writes)
;	seldsk	select disk given by reg-c (0,1,2...)
;	settrk	set track address (0,...76) for subsequent read/write
;	setsec	set sector address (1,...,26) for subsequent read/write
;	setdma	set subsequent dma address (initially 80h)
;
;	(read and write assume previous calls to set up the io parameters)
;	read	read track/sector to preset dma address
;	write	write track/sector from preset dma address
;
;	jump vector for indiviual routines
	jmp	boot
wboote:	jmp	wboot
	jmp	const
	jmp	conin
	jmp	conout
	jmp	list
	jmp	punch
	jmp	reader
	jmp	home
	jmp	seldsk
	jmp	settrk
	jmp	setsec
	jmp	setdma
	jmp	read
	jmp	write
	jmp	listst	;list status
	jmp	sectran
;
	maclib	diskdef	;load the disk definition library
	disks	4	;four disks
	diskdef	0,1,26,6,1024,243,64,64,offset
	diskdef	1,0
	diskdef	2,0
	diskdef	3,0
;	endef occurs at end of assembly
;
;	end of controller - independent code, the remaining subroutines
;	are tailored to the particular operating environment, and must
;	be altered for any system which differs from the intel mds.
;
;	the following code assumes the mds monitor exists at 0f800h
;	and uses the i/o subroutines within the monitor
;
;	we also assume the mds system has four disk drives
revrt	equ	0fdh	;interrupt revert port
intc	equ	0fch	;interrupt mask port
icon	equ	0f3h	;interrupt control port
inte	equ	0111$1110b	;enable rst 0(warm boot), rst 7 (monitor)
;
;	mds monitor equates
mon80	equ	0f800h	;mds monitor
rmon80	equ	0ff0fh	;restart mon80 (boot error)
ci	equ	0f803h	;console character to reg-a
ri	equ	0f806h	;reader in to reg-a
co	equ	0f809h	;console char from c to console out
po	equ	0f80ch	;punch char from c to punch device
lo	equ	0f80fh	;list from c to list device
csts	equ	0f812h	;console status 00/ff to register a
;
;	disk ports and commands
base	equ	78h	;base of disk command io ports
dstat	equ	base	;disk status (input)
rtype	equ	base+1	;result type (input)
rbyte	equ	base+3	;result byte (input)
;
ilow	equ	base+1	;iopb low address (output)
ihigh	equ	base+2	;iopb high address (output)
;
readf	equ	4h	;read function
writf	equ	6h	;write function
recal	equ	3h	;recalibrate drive
iordy	equ	4h	;i/o finished mask
cr	equ	0dh	;carriage return
lf	equ	0ah	;line feed
;
signon:	;signon message: xxk cp/m vers y.y
	db	cr,lf,lf
	if	test
	db	'32'	;32k example bios
	endif
	if	not test
	db	'00'	;memory size filled by relocator
	endif
	db	'k CP/M vers '
	db	vers/10+'0','.',vers mod 10+'0'
	db	cr,lf,0
;
boot:	;print signon message and go to ccp
;	(note: mds boot initialized iobyte at 0003h)
	lxi	sp,buff+80h
	lxi	h,signon
	call	prmsg	;print message
	xra	a	;clear accumulator
	sta	cdisk	;set initially to disk a
	jmp	gocpm	;go to cp/m
;
;
wboot:;	loader on track 0, sector 1, which will be skipped for warm 
;	read cp/m from disk - assuming there is a 128 byte cold start
;	start.
;
	lxi	sp,buff	;using dma - thus 80 thru ff available for stack
;
	mvi	c,retry	;max retries
	push	b
wboot0:	;enter here on error retries
	lxi	b,cpmb	;set dma address to start of disk system
	call	setdma
	mvi	c,0	;boot from drive 0
	call	seldsk
	mvi	c,0
	call	settrk	;start with track 0
	mvi	c,2	;start reading sector 2
	call	setsec
;
;	read sectors, count nsects to zero
	pop	b	;10-error count
	mvi	b,nsects
rdsec:	;read next sector
	push	b	;save sector count
	call	read
	jnz	booterr	;retry if errors occur
	lhld	iod	;increment dma address
	lxi	d,128	;sector size
	dad	d	;incremented dma address in hl
	mov	b,h
	mov	c,l	;ready for call to set dma
	call	setdma
	lda	ios	;sector number just read
	cpi	26	;read last sector?
	jc	rd1
;	must be sector 26, zero and go to next track
	lda	iot	;get track to register a
	inr	a
	mov	c,a	;ready for call
	call	settrk
	xra	a	;clear sector number
rd1:	inr	a	;to next sector
	mov	c,a	;ready for call
	call	setsec
	pop	b	;recall sector count
	dcr	b	;done?
	jnz	rdsec
;
;	done with the load, reset default buffer address
gocpm:	;(enter here from cold start boot)
;	enable rst0 and rst7
	di
	mvi	a,12h	;initialize command
	out	revrt
	xra	a
	out	intc	;cleared
	mvi	a,inte	;rst0 and rst7 bits on
	out	intc
	xra	a
	out	icon	;interrupt control
;
;	set default buffer address to 80h
	lxi	b,buff
	call	setdma
;
;	reset monitor entry points
	mvi	a,jmp
	sta	0
	lxi	h,wboote
	shld	1	;jmp wboot at location 00
	sta	5
	lxi	h,bdos
	shld	6	;jmp bdos at location 5
	if	not test
	sta	7*8	;jmp to mon80 (may have been changed by ddt)
	lxi	h,mon80
	shld	7*8+1
	endif
;	leave iobyte set
;	previously selected disk was b, send parameter to cpm
	lda	cdisk	;last logged disk number
	mov	c,a	;send to ccp to log it in
	ei
	jmp	cpmb
;
;	error condition occurred, print message and retry
booterr:
	pop	b	;recall counts
	dcr	c
	jz	booter0
;	try again
	push	b
	jmp	wboot0
;
booter0:
;	otherwise too many retries
	lxi	h,bootmsg
	call	prmsg
	jmp	rmon80	;mds hardware monitor
;
bootmsg:
	db	'?boot',0
;
;
const:	;console status to reg-a
;	(exactly the same as mds call)
	jmp	csts
;
conin:	;console character to reg-a
	call	ci
	ani	7fh	;remove parity bit
	ret
;
conout:	;console character from c to console out
	jmp	co
;
list:	;list device out
;	(exactly the same as mds call)
	jmp	lo
;
listst:
	;return list status
	xra	a
	ret		;always not ready
;
punch:	;punch device out
;	(exactly the same as mds call)
	jmp	po
;
reader:	;reader character in to reg-a
;	(exactly the same as mds call)
	jmp	ri
;
home:	;move to home position
;	treat as track 00 seek
	mvi	c,0
	jmp	settrk
;
seldsk:	;select disk given by register c
	lxi	h,0000h	;return 0000 if error
	mov	a,c
	cpi	ndisks	;too large?
	rnc		;leave HL = 0000
;
	ani	10b	;00 00 for drive 0,1 and 10 10 for drive 2,3
	sta	dbank	;to select drive bank
	mov	a,c	;00, 01, 10, 11
	ani	1b	;mds has 0,1 at 78, 2,3 at 88
	ora	a	;result 00?
	jz	setdrive
	mvi	a,00110000b	;selects drive 1 in bank
setdrive:
	mov	b,a	;save the function
	lxi	h,iof	;io function
	mov	a,m
	ani	11001111b	;mask out disk number
	ora	b	;mask in new disk number
	mov	m,a	;save it in iopb
	mov	l,c
	mvi	h,0	;HL=disk number
	dad	h	;*2
	dad	h	;*4
	dad	h	;*8
	dad	h	;*16
	lxi	d,dpbase
	dad	d	;HL=disk header table address
	ret
;
;
settrk:	;set track address given by c
	lxi	h,iot
	mov	m,c
	ret
;
setsec:	;set sector number given by c
	lxi	h,ios
	mov	m,c
	ret
sectran:
		;translate sector bc using table at de
	mvi	b,0	;double precision sector number in BC
	xchg		;translate table address to HL
	dad	b	;translate(sector) address
	mov	a,m	;translated sector number to A
	sta	ios
	mov	l,a	;return sector number in L
	ret
;
setdma:	;set dma address given by regs b,c
	mov	l,c
	mov	h,b
	shld	iod
	ret
;
read:	;read next disk record (assuming disk/trk/sec/dma set)
	mvi	c,readf	;set to read function
	call	setfunc
	call	waitio	;perform read function
	ret		;may have error set in reg-a
;
;
write:	;disk write function
	mvi	c,writf
	call	setfunc	;set to write function
	call	waitio
	ret		;may have error set
;
;
;	utility subroutines
prmsg:	;print message at h,l to 0
	mov	a,m
	ora	a	;zero?
	rz
;	more to print
	push	h
	mov	c,a
	call	conout
	pop	h
	inx	h
	jmp	prmsg
;
setfunc:
;	set function for next i/o (command in reg-c)
	lxi	h,iof	;io function address
	mov	a,m	;get it to accumulator for masking
	ani	11111000b	;remove previous command
	ora	c	;set to new command
	mov	m,a	;replaced in iopb
;	the mds-800 controller requires disk bank bit in sector byte
;	mask the bit from the current i/o function
	ani	00100000b	;mask the disk select bit
	lxi	h,ios		;address the sector select byte
	ora	m		;select proper disk bank
	mov	m,a		;set disk select bit on/off
	ret
;
waitio:
	mvi	c,retry	;max retries before perm error
rewait:
;	start the i/o function and wait for completion
	call	intype	;in rtype
	call	inbyte	;clears the controller
;
	lda	dbank		;set bank flags
	ora	a		;zero if drive 0,1 and nz if 2,3
	mvi	a,iopb and 0ffh	;low address for iopb
	mvi	b,iopb shr 8	;high address for iopb
	jnz	iodr1	;drive bank 1?
	out	ilow		;low address to controller
	mov	a,b
	out	ihigh	;high address
	jmp	wait0		;to wait for complete
;
iodr1:	;drive bank 1
	out	ilow+10h	;88 for drive bank 10
	mov	a,b
	out	ihigh+10h
;
wait0:	call	instat		;wait for completion
	ani	iordy		;ready?
	jz	wait0
;
;	check io completion ok
	call	intype		;must be io complete (00) unlinked
;	00 unlinked i/o complete,    01 linked i/o complete (not used)
;	10 disk status changed       11 (not used)
	cpi	10b		;ready status change?
	jz	wready
;
;	must be 00 in the accumulator
	ora	a
	jnz	werror		;some other condition, retry
;
;	check i/o error bits
	call	inbyte
	ral
	jc	wready		;unit not ready
	rar
	ani	11111110b	;any other errors?  (deleted data ok)
	jnz	werror
;
;	read or write is ok, accumulator contains zero
	ret
;
wready:	;not ready, treat as error for now
	call	inbyte		;clear result byte
	jmp	trycount
;
werror:	;return hardware malfunction (crc, track, seek, etc.)
;	the mds controller has returned a bit in each position
;	of the accumulator, corresponding to the conditions:
;	0	- deleted data (accepted as ok above)
;	1	- crc error
;	2	- seek error
;	3	- address error (hardware malfunction)
;	4	- data over/under flow (hardware malfunction)
;	5	- write protect (treated as not ready)
;	6	- write error (hardware malfunction)
;	7	- not ready
;	(accumulator bits are numbered 7 6 5 4 3 2 1 0)
;
;	it may be useful to filter out the various conditions,
;	but we will get a permanent error message if it is not
;	recoverable.  in any case, the not ready condition is
;	treated as a separate condition for later improvement
trycount:
;	register c contains retry count, decrement 'til zero
	dcr	c
	jnz	rewait	;for another try
;
;	cannot recover from error
	mvi	a,1	;error code
	ret
;
;	intype, inbyte, instat read drive bank 00 or 10
intype:	lda	dbank
	ora	a
	jnz	intyp1	;skip to bank 10
	in	rtype
	ret
intyp1:	in	rtype+10h	;78 for 0,1  88 for 2,3
	ret
;
inbyte:	lda	dbank
	ora	a
	jnz	inbyt1
	in	rbyte
	ret
inbyt1:	in	rbyte+10h
	ret
;
instat:	lda	dbank
	ora	a
	jnz	insta1
	in	dstat
	ret
insta1:	in	dstat+10h
	ret
;
;
;
;	data areas (must be in ram)
dbank:	db	0	;disk bank 00 if drive 0,1
			;	   10 if drive 2,3
iopb:	;io parameter block
	db	80h	;normal i/o operation
iof:	db	readf	;io function, initial read
ion:	db	1	;number of sectors to read
iot:	db	offset	;track number
ios:	db	1	;sector number
iod:	dw	buff	;io address
;
;
;	define ram areas for bdos operation
	endef
	end
