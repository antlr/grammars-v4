; Source: https://redirect.cs.umbc.edu/portal/help/nasm/testreg_64.asm
; testreg_64.asm   test what register names can be used
;
; compile:	nasm -f elf64 -l testasm_64.lst  testasm_64.asm
; link:		gcc -o testasm_64  testasm_64.o
; run:		testasm  # may get segfault or other error
;
	section .data  		; preset constants, writeable
aa8:	db	8		; 8-bit
aa16:	dw	16		; 16-bit
aa32:	dd	32		; 32-bit
aa64:	dq	64		; 64-bit
	
	section .bss
bb16:	resw	16
	
	section .rodata
cc16:	db	8
		
	section .text		; instructions, code segment
	global	 main		; for gcc standard linking
main:				; label
	push	rbp		; set up stack
	
	mov	rax,[aa64]	; five registers in RAX
	mov	eax,[aa32]	; four registers in EAX
	mov	ax,[aa16]
	mov	ah,[aa8]
	mov	al,[aa8]

	mov	RAX,[aa64]	; upper case register names
	mov	EAX,[aa32]
	mov	AX,[aa16]
	mov	AH,[aa8]
	mov	AL,[aa8]
	
	mov	rbx,[aa64]	; five registers in RBX
	mov	ebx,[aa32]	; four registers in EBX
	mov	bx,[aa16]
	mov	bh,[aa8]
	mov	bl,[aa8]
	
	mov	rcx,[aa64]	; five registers in RCX
	mov	ecx,[aa32]	; four registers in ECX
	mov	cx,[aa16]
	mov	ch,[aa8]
	mov	cl,[aa8]
	
	mov	rdx,[aa64]	; five registers in RDX
	mov	edx,[aa32]	; four registers in EDX
	mov	dx,[aa16]
	mov	dh,[aa8]
	mov	dl,[aa8]
	
	mov	rsi,[aa64]	; three registers in RSI
	mov	esi,[aa32]	; two registers in ESI
	mov	si,[aa16]
	
	mov	rdi,[aa64]	; three registers in RDI
	mov	edi,[aa32]	; two registers in EDI
	mov	di,[aa16]

	mov	rbp,[aa64]	; three registers in RBP
	mov	ebp,[aa32]	; two registers in EBP
	mov	bp,[aa16]

	mov	r8,[aa64]	; just 64-bit r8 .. r15
	
	movq	xmm0, qword [aa64] ; xmm registers special

	fld	qword [aa64]	; floating point special
	
;	POPF			; no "mov" on EFLAGS register
;	PUSHF			; 32 bits on 386 and above

;	mov	rsp,[aa64]	; three registers in RSP
;	mov	esp,[aa32]	; two registers in ESP
;	mov	sp,[aa16]	; don't mess with stack
	
	pop	rbp
	mov     rax,0           ; exit code, 0=normal
	ret			; main returns to operating system

; end testreg_64.asm
