; Source: https://redirect.cs.umbc.edu/portal/help/nasm/fib_64l.asm
; fib_64l.asm  using 64 bit registers to implement fib.c
	global main
	extern printf

	section .data
format:	db '%15ld', 10, 0
title:	db 'fibinachi numbers', 10, 0
	
	section .text
main:
	push rbp 		; set up stack
	mov rdi, title 		; arg 1 is a pointer
	mov rax, 0 		; no vector registers in use
	call printf

	mov rcx, 95 		; rcx will countdown from 52 to 0
	mov rax, 1 		; rax will hold the current number
	mov rbx, 2 		; rbx will hold the next number
print:
	;  We need to call printf, but we are using rax, rbx, and rcx.
	;  printf may destroy rax and rcx so we will save these before
	;  the call and restore them afterwards.
	push rax 		; 32-bit stack operands are not encodable
	push rcx 		; in 64-bit mode, so we use the "r" names
	mov rdi, format 	; arg 1 is a pointer
	mov rsi, rax 		; arg 2 is the current number
	mov eax, 0 		; no vector registers in use
	call printf
	pop rcx
	pop rax
	mov rdx, rax 		; save the current number
	mov rax, rbx 		; next number is now current
	add rbx, rdx 		; get the new next number
	dec rcx 		; count down
	jnz print 		; if not done counting, do some more

	pop rbp 		; restore stack
	mov rax, 0		; normal exit
	ret
