$title	('COM Externals')
	name	mcd80a
	CSEG
;	September 14, 1982

offset	equ	0000h


	EXTRN	PLM

;	EXTERNAL ENTRY POINTS

mon1	equ	0005h+offset
mon2	equ	0005h+offset
mon2a	equ	0005h+offset
mon3 	equ	0005h+offset
	public	mon1,mon2,mon2a,mon3

;	EXTERNAL BASE PAGE DATA LOCATIONS

iobyte	equ	0003h+offset
bdisk	equ	0004h+offset
maxb	equ	0006h+offset
memsiz	equ	maxb
cmdrv	equ	0050h+offset
pass0	equ	0051h+offset
len0	equ	0053h+offset
pass1	equ	0054h+offset
len1	equ	0056h+offset
fcb	equ	005ch+offset
fcba	equ	fcb
sfcb	equ	fcb
ifcb	equ	fcb
ifcba	equ	fcb
fcb16	equ	006ch+offset
dolla	equ	006dh+offset
parma	equ	006eh+offset
cr	equ	007ch+offset
rr	equ	007dh+offset
rreca	equ	rr
ro	equ	007fh+offset
rreco	equ	ro
tbuff	equ	0080h+offset
buff	equ	tbuff
buffa	equ	tbuff
cpu	equ	0	; 0 = 8080, 1 = 8086/88, 2 = 68000

	public	iobyte,bdisk,maxb,memsiz
	public	cmdrv,pass0,len0,pass1,len1
	public	fcb,fcba,sfcb,ifcb,ifcba,fcb16
	public	cr,rr,rreca,ro,rreco,dolla,parma
	public	buff,tbuff,buffa, cpu


	;*******************************************************
	; The interface should proceed the program
	; so that TRINT becomes the entry point for the 
	; COM file.  The stack is set and memsiz is set
	; to the top of memory.  Program termination is done
	; with a return to preserve R/O diskettes.
	;*******************************************************

;	EXECUTION BEGINS HERE

	lxi	sp, stack
	JMP 	PLM

;	PATCH AREA, DATE, VERSION & SERIAL NOS.

	dw	0,0,0,0,0,0,0,0
	dw	0,0,0,0,0,0,0,0
	dw	0,0,0,0,0,0,0,0
	dw	0,0,0,0,0

	db	'CP/M Version 3.0'
	db	'COPYRIGHT 1982, '
	db	'DIGITAL RESEARCH'
	db	'151282'	; version date day-month-year
	db	0,0,0,0		; patch bit map
	db	'654321'	; Serial no.

	END
	EOF
