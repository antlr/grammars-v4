;
;
;
;
;	Title		Hex ASCII to binary
;	Name:		HEX2BN
;
;
;
;	Purpose:	Convert two ASCII characters to one
;			byte of binary data
;
;	Entry:		Register H = ASCII more significant digit
;			Register L = ASCII less significant digit
;
;	Exit:		Register A = Binary data
;
;	Registers used: AF, B
;
;	Time:		Approximately 148 cycles
;
;	Size:		Program 24 bytes
;
;

HEX2BN:
	LD	A,L		;GET LOW CHARACTER
	CALL	A2HEX		;CONVERT IT TO HEXADECIMAL
	LD	B,A		;SAVE HEX VALUE IN B
	LD	A,H		;GET HIGH CHARACTER
	CALL	A2HEX		;CONVERT IT TO HEXADECIMAL
	RRCA			;SHIFT HEX VALUE TO UPPER 4 BITS
	RRCA
	RRCA
	RRCA
	OR	B		;OR IN LOW HEX VALUE
	RET

	;---------------------------------------
	;SUBROUTINE: A2HEX
	;PURPOSE: CONVERT ASCII DIGIT TO A HEX DIGIT
	;ENTRY: A = ASCII HEXADECIMAL DIGIT
	;EXIT:  A = BINARY VALUE OF ASCII DIGIT
	;REGISTERS USED: A,F
	;---------------------------------------
A2HEX:
	SUB	'0'		;SUBTRACT ASCII OFFSET
	CP	10
	JR	C,A2HEX1	;BRANCH IF A IS DECIMAL DIGIT
	SUB	7		;ELSE SUBTRACT OFFSET FOR LETTERS
A2HEX1:
	RET

;
;
;	SAMPLE EXECUTION:
;
;

SC4D:
	;CONVERT 'C7' TO C7 HEXADECIMAL
	LD	H,'C'
	LD	L,'7'
	CALL	HEX2BN		;A=C7H

	;CONVERT '2F' TO 2F HEXADECIMAL
	LD	H,'2'
	LD	L,'F'
	CALL	HEX2BN		;A=2FH

	;CONVERT '2A' TO 2A HEXADECIMAL
	LD	H,'2'
	LD	L,'A'
	CALL	HEX2BN		;A=2AH

	JR	SC4D

	END
