$title	('GENCPM Data module')
	name	datmod

;  Copyright (C) 1982
;  Digital Research
;  P.O. Box 579
;  Pacific Grove, CA 93950
;
;  Revised:
;    15 Nov 82 by Bruce Skidmore
;

	cseg

        public	symtbl

;declare symtbl(16) structure(
;  token(8) byte, /* question variable name */
;  len      byte, /* length of structure in array of structures */
;  flags    byte, /* type of variable */
;  qptr     byte, /* index into query array */
;  ptr      address); /* pointer to the associated data structure */

;  flags definition:
;      bit(3) = 1 then array of structures
;      bit(4) = 1 then index is A-P else index is 0-F
;      bit(2) = 1 then numeric variable
;      bit(1) = 1 boolean variable legal values are Y or N
;      bit(0) = 1 drive variable legal values are A-P

symtbl:
	db	'PRTMSG  ',1, 00000010B,0
	dw	prtmsg
	db	'PAGWID  ',1, 00000100B,1
	dw	conwid
	db	'PAGLEN  ',1, 00000100B,2
	dw	conpag
	db	'BACKSPC ',1, 00000010B,3
	dw	bckspc
	db	'RUBOUT  ',1, 00000010B,4
	dw	rubout
	db	'BOOTDRV ',1, 00000001B,5
	dw	bdrive
	db	'MEMTOP  ',1, 00000100B,6
	dw	memtop
	db	'BNKSWT  ',1, 00000010B,7
	dw	bnkswt
	db	'COMBAS  ',1, 00000100B,8
	dw	bnktop
	db	'LERROR  ',1, 00000010B,9
	dw	lerror
	db	'NUMSEGS ',1, 00000100B,10
	dw	numseg
	db	'MEMSEG00',5, 00001100B,11
	dw	memtbl+5
	db	'HASHDRVA',1, 00011010B,27
	dw	hash
	db	'ALTBNKSA',10,00011010B,43
	dw	record+3
	db	'NDIRRECA',10,00011100B,59
	dw	record+4
	db	'NDTARECA',10,00011100B,75
	dw	record+5
	db	'ODIRDRVA',10,00011001B,91
	dw	record+6
	db	'ODTADRVA',10,00011001B,107
	dw	record+7
	db	'OVLYDIRA',10,00011010B,123
	dw	record+8
	db	'OVLYDTAA',10,00011010B,139
	dw	record+9
	db	'CRDATAF ',1,00000010B,155
	dw	crdatf
	db	'DBLALV  ',1,00000010B,156
	dw	dblalv

	public	lerror,prtmsg,bnkswt,memtop,bnktop
	public	bdrive,conpag,conwid,bckspc
	public	rubout,numseg,hash,memtbl,record
	public	crdatf,dblalv

lerror:
	db	0ffh
prtmsg:
	db	0ffh
bnkswt:
	db	0ffh
memtop:
	db	0ffh
bnktop:
	db	0c0h
bdrive:
	db	00h
conpag:
	db	23
conwid:
	db	79
bckspc:
	db	0
rubout:
	db	0ffh
numseg:
	db	3
hash:
	db	0ffh,0ffh,0ffh,0ffh
	db	0ffh,0ffh,0ffh,0ffh
	db	0ffh,0ffh,0ffh,0ffh
	db	0ffh,0ffh,0ffh,0ffh
memtbl:
	db	0,0,0,0,0
	db	0,080h,00h,0,0
	db	0,0c0h,02h,0,0
	db	0,0c0h,03h,0,0
	db	0,0c0h,04h,0,0
	db	0,0c0h,05h,0,0
	db	0,0c0h,06h,0,0
	db	0,0c0h,07h,0,0
	db	0,0c0h,08h,0,0
	db	0,0c0h,09h,0,0
	db	0,0c0h,0ah,0,0
	db	0,0c0h,0bh,0,0
	db	0,0c0h,0ch,0,0
	db	0,0c0h,0dh,0,0
	db	0,0c0h,0eh,0,0
	db	0,0c0h,0fh,0,0
	db	0,0c0h,10h,0,0
record:
	dw	0
	db	0,0,1,1,0,0,0ffh,0ffh
	dw	0
	db	0,0,1,1,0,0,0ffh,0ffh
	dw	0
	db	0,0,1,1,0,0,0ffh,0ffh
	dw	0
	db	0,0,1,1,0,0,0ffh,0ffh
	dw	0
	db	0,0,1,1,0,0,0ffh,0ffh
	dw	0
	db	0,0,1,1,0,0,0ffh,0ffh
	dw	0
	db	0,0,1,1,0,0,0ffh,0ffh
	dw	0
	db	0,0,1,1,0,0,0ffh,0ffh
	dw	0
	db	0,0,1,1,0,0,0ffh,0ffh
	dw	0
	db	0,0,1,1,0,0,0ffh,0ffh
	dw	0
	db	0,0,1,1,0,0,0ffh,0ffh
	dw	0
	db	0,0,1,1,0,0,0ffh,0ffh
	dw	0
	db	0,0,1,1,0,0,0ffh,0ffh
	dw	0
	db	0,0,1,1,0,0,0ffh,0ffh
	dw	0
	db	0,0,1,1,0,0,0ffh,0ffh
	dw	0
	db	0,0,1,1,0,0,0ffh,0ffh
crdatf:
	db	0
dblalv:
	db	0ffh

	public	quest
quest:
	ds	157
	end
