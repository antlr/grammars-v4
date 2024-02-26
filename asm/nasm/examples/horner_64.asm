; Source: https://redirect.cs.umbc.edu/portal/help/nasm/horner_64.asm
; horner_64.asm  Horners method of evaluating polynomials
;
; given a polynomial  Y = a_n X^n + a_n-1 X^n-1 + ... a_1 X + a_0
; a_n is the coefficient 'a' with subscript n. X^n is X to nth power
; compute y_1 = a_n * X + a_n-1
; compute y_2 = y_1 * X + a_n-2
; compute y_i = y_i-1 * X + a_n-i   i=3..n
; thus    y_n = Y = value of polynomial 
;
; in assembly language:
;   load some register with a_n, multiply by X
;   add a_n-1, multiply by X, add a_n-2, multiply by X, ...
;   finishing with the add  a_0
;
; output from execution:
; a  6319
; aa 6319
; af 6.319000e+03

	extern	printf
	section	.data
	global	main

	section	.data
fmta:	db	"a  %ld",10,0
fmtaa:	db	"aa %ld",10,0
fmtflt:	db	"af %e",10,0

	section	.text
main:	push	rbp		; set up stack

; evaluate an integer polynomial, X=7, using a count

	section	.data
a:	dq	2,5,-7,22,-9	; coefficients of polynomial, a_n first
X:	dq	7		; X = 7
				; n=4, 8 bytes per coefficient
	section	.text
	mov	rax,[a]		; accumulate value here, get coefficient a_n
	mov	rdi,1		; subscript initialization
	mov	rcx,4		; loop iteration count initialization, n
h3loop:	imul	rax,[X]		; * X     (ignore edx)
	add	rax,[a+8*rdi]	; + a_n-i
	inc	rdi		; increment subscript
	loop	h3loop		; decrement rcx, jump on non zero

	mov	rsi, rax	; print rax
	mov	rdi, fmta	; format
	mov	rax, 0		; no float
	call	printf


; evaluate an integer polynomial, X=7, using a count as index
; optimal organization of data allows a three instruction loop
	
	section	.data
aa:	dq	-9,22,-7,5,2	; coefficients of polynomial, a_0 first
n:	dq	4		; n=4, 8 bytes per coefficient
	section	.text
	mov	rax,[aa+4*8]	; accumulate value here, get coefficient a_n
	mov	rcx,[n]		; loop iteration count initialization, n
h4loop:	imul	rax,[X]		; * X     (ignore edx)
	add	rax,[aa+8*rcx-8]; + aa_n-i
	loop	h4loop		; decrement rcx, jump on non zero

	mov	rsi, rax	; print rax
	mov	rdi, fmtaa	; format
	mov	rax, 0		; no float
	call	printf

; evaluate a double floating polynomial, X=7.0, using a count as index
; optimal organization of data allows a three instruction loop
	
	section	.data
af:	dq	-9.0,22.0,-7.0,5.0,2.0	; coefficients of polynomial, a_0 first
XF:	dq	7.0
Y:	dq	0.0
N:	dd	4

	section	.text
	mov	rcx,[N]		; loop iteration count initialization, n
	fld	qword [af+8*rcx]; accumulate value here, get coefficient a_n
h5loop:	fmul	qword [XF]	; * XF
	fadd	qword [af+8*rcx-8] ; + aa_n-i
	loop	h5loop		; decrement rcx, jump on non zero

	fstp	qword [Y]	; store Y in order to print Y
	movq	xmm0, qword [Y]	; well, may just mov reg
	mov	rdi, fmtflt	; format
	mov	rax, 1		; one float
	call	printf

	pop	rbp		; restore stack
	mov	rax,0		; normal return
	ret			; return
