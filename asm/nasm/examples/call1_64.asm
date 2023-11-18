; Source: https://redirect.cs.umbc.edu/portal/help/nasm/call1_64.asm
; call1_64.asm  a basic structure for a subroutine to be called from "C"
;
; Parameter:   long int *L
; Result: L[0]=L[0]+3  L[1]=L[1]+4

        global call1_64		; linker must know name of subroutine

        extern	printf		; the C function, to be called for demo

        SECTION .data		; Data section, initialized variables
fmt1:    db "rdi=%ld, L[0]=%ld", 10, 0	; The printf format, "\n",'0'
fmt2:    db "rdi=%ld, L[1]=%ld", 10, 0	; The printf format, "\n",'0'

	SECTION .bss
a:	resq	1		; temp for printing

	SECTION .text           ; Code section.

call1_64:			; name must appear as a nasm label
        push	rbp		; save rbp
        mov	rbp, rsp	; rbp is callers stack
        push	rdx		; save registers
        push	rdi
	push	rsi

	mov	rax,rdi		; first, only, in parameter
	mov	[a],rdi		; save for later use

	mov	rdi,fmt1	; format for printf debug, demo
	mov	rsi,rax         ; first parameter for printf
	mov	rdx,[rax]	; second parameter for printf
	mov	rax,0		; no xmm registers
        call    printf		; Call C function	

	mov	rax,[a]		; first, only, in parameter, demo
	mov	rdi,fmt2	; format for printf
	mov	rsi,rax         ; first parameter for printf
	mov	rdx,[rax+8]	; second parameter for printf
	mov	rax,0		; no xmm registers
        call    printf		; Call C function	

	mov	rax,[a]		; add 3 to L[0]
	mov	rdx,[rax]	; get L[0]
	add	rdx,3		; add
	mov	[rax],rdx	; store sum for caller

	mov	rdx,[rax+8]	; get L[1]
	add	rdx,4		; add
	mov	[rax+8],rdx	; store sum for caller

        pop	rsi		; restore registers
	pop	rdi		; in reverse order
        pop	rdx
        mov	rsp,rbp		; restore callers stack frame
        pop	rbp
        ret			; return

