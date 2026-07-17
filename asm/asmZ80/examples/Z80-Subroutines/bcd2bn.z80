;
;
;
;
;       Title           BCD to binary coversion
;       Name:           BCD2BN
;
;
;       Purpose:        Convert one byte of BCD data to one
;			byte of binary data
;
;	Entry:		Register A = BCD data
;
;	Exit:		Register A = Binary data
;
;	Registers used:	A, B, C, F
;
;	Time:		60 cycles
;
;	Size:		Program 14 bytes
;
;
;

BCD2BN:
	;MULYIPLY UPPER NIBBLE BY 10 AND SAVE IT
	; UPPER NIBBLE * 10 = UPPER NIBBLE * (8 +2)
	LD	B,A		;SAVE ORIGINAL BCD VALUE IN B
	AND	0F0H		;MASK OFFUPPER NIBBLE
	RRCA			;SHIFT RIGHT 1 BIT
	LD	C,A		;C = UPPER NIBBLE * 8
	RRCA			;SHIFT RIGHT 2 MORE TIMES
	RRCA			;A = UPPER NIBBLE * 2
	ADD	A,C
	LD	C,A 		;C = UPPER NIBBLE * (8+2)

	;GET LOWER NIBBLE AND ADD IT TO THE
	; BINARY EQUIVALENT OF THE UPPER NIBBLE
	LD	A,B		;GET ORIGINAL VALUE BACK
	AND 	0FH		;MASK OFF UPPER NIBBLE
	ADD	A,C		;ADD TO BINARY UPPER NIBBLE
	RET	

;
;
;	SAMPLE EXECUTION:
;
;

SC4B:
	;CONVERT 0 BCD TO 0 HEXADECIMAL
	LD	A,0
	CALL	BCD2BN		;A = 0H

	;CONVERT 99 BCD TO 63 HEXADECIMAL
	LD	A,099H
	CALL	BCD2BN		;A = 63H

	;CONVERT 23 BCD TO 17 HEXADECIMAL
	LD	A,23H
	CALL	BCD2BN		;A = 17H

	JR	SC4B

	END
