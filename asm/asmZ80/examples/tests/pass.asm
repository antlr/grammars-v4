; pass.asm - test program which should pass compilation (with warnings)
; Copyright 2002-2007  Bas Wijnen
;
; This file is part of z80asm.
;
; Z80asm is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 3 of the License, or
; (at your option) any later version.
;
; Z80asm is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program.  If not, see <http://www.gnu.org/licenses/>.

	;;  macro handling
testm1:	macro
	nop
	endm

testm2:	macro arg
	xor arg
	endm

testm3:	macro arg1,arg2
	ex arg1,arg2
	endm

testm4:	macro
	ld b, 0x04
.lbl:	ld a,b
	djnz .lbl
	endm

	; Some constructs which should compile properly

	ld a, (0xc0)
	ld a, (0xc0) + 3	; this is the same as ld a, 0xc3
	ld b, (0xc0)		; warning: expression in parentheses
	ld a, -3
	ld c, 0xc003		; warning: 8-bit value truncated
	ld bc, 0xc003
	ld de, 0x12345		; warning: 16-bit value truncated
	ld bc, -0x8000
	ld de, -0x8001		; warning: 16-bit value truncated
	ld b, -0x81		; warning: 8-bit value truncated
	ld a, -0x80
	halt
	add hl, de
	ex af, af'
	jr z, $ + 4
	defb 0, 4, 5, label - $
label:
	jr label + 2
	db ';("', "'"

	testm1			; should have no problem
	testm2 a		; one argument, simple
	testm3 (sp),hl		; two arguments, should not give empty parameter error
	testm4			; .lbl should be resolved correctly
	testm4			; scope of .lbl..
	
data:
	db 0x01, 0x02, ' T\'is a test...'
	db 0xff, "What does \"quote\" mean?"

test_rd_r_rr:
	inc iy      ; fd 23
	inc ix      ; dd 23
	inc sp      ; 33
	inc hl      ; 23
	inc de      ; 13
	inc bc      ; 03

	inc b       ; 04
	inc c       ; 0c
	inc d       ; 14
	inc e       ; 1c
	inc h       ; 24
	inc l       ; 2c

	inc (hl)    ; 34
	inc a       ; 3c
	inc (ix+3)  ; dd 34 03
	inc (iy+3)  ; fd 34 03

	inc ixl ; dd 2c
	inc ixh ; dd 24
	inc iyl ; fd 2c
	inc iyh ; fd 24

	dec iy      ; fd 2b
	dec ix      ; dd 2b
	dec sp      ; 3b
	dec hl      ; 2b
	dec de      ; 1b
	dec bc      ; 0b

	dec b       ; 05
	dec c       ; 0d
	dec d       ; 15
	dec e       ; 1d
	dec h       ; 25
	dec l       ; 2d

	dec (hl)    ; 35
	dec a       ; 3d

	dec (ix+3)  ; dd 35 03
	dec (iy+3)  ; fd 35 03

	dec ixl     ; dd 2d
	dec ixh     ; dd 25
	dec iyl     ; fd 2d
	dec iyh     ; fd 25
