; Source: https://redirect.cs.umbc.edu/portal/help/nasm/intarith_64.asm
; intarith_64.asm    show some simple C code and corresponding nasm code
;                    the nasm code is one sample, not unique
;
; compile:	nasm -f elf64 -l intarith_64.lst  intarith_64.asm
; link:		gcc -m64 -o intarith_64  intarith_64.o
; run:		./intarith_64
;
; the output from running intarith_64.asm and intarith.c is:	
; c=5  , a=3, b=4, c=5
; c=a+b, a=3, b=4, c=7
; c=a-b, a=3, b=4, c=-1
; c=a*b, a=3, b=4, c=12
; c=c/a, a=3, b=4, c=4
;
;The file  intarith.c  is:
;  /* intarith.c */
;  #include <stdio.h>
;  int main()
;  { 
;    long int a=3, b=4, c;
;    c=5;
;    printf("%s, a=%ld, b=%ld, c=%ld\n","c=5  ", a, b, c);
;    c=a+b;
;    printf("%s, a=%ld, b=%ld, c=%ld\n","c=a+b", a, b, c);
;    c=a-b;
;    printf("%s, a=%ld, b=%ld, c=%ld\n","c=a-b", a, b, c);
;    c=a*b;
;    printf("%s, a=%ld, b=%ld, c=%ld\n","c=a*b", a, b, c);
;    c=c/a;
;    printf("%s, a=%ld, b=%ld, c=%ld\n","c=c/a", a, b, c);
;    return 0;
; }
        extern printf		; the C function to be called

%macro	pabc 1			; a "simple" print macro
	section .data
.str	db	%1,0		; %1 is first actual in macro call
	section .text
        mov     rdi, fmt4	; first arg, format
	mov	rsi, .str	; second arg
	mov     rdx, [a]        ; third arg
	mov     rcx, [b]        ; fourth arg
	mov     r8, [c]         ; fifth arg
	mov     rax, 0	        ; no xmm used
	call    printf		; Call C function
%endmacro
	
	section .data  		; preset constants, writeable
a:	dq	3		; 64-bit variable a initialized to 3
b:	dq	4		; 64-bit variable b initializes to 4
fmt4:	db "%s, a=%ld, b=%ld, c=%ld",10,0	; format string for printf
	
	section .bss 		; unitialized space
c:	resq	1		; reserve a 64-bit word

	section .text		; instructions, code segment
	global	 main		; for gcc standard linking
main:				; label
	push 	rbp		; set up stack
lit5:				; c=5;
	mov	rax,5	 	; 5 is a literal constant
	mov	[c],rax		; store into c
	pabc	"c=5  "		; invoke the print macro
	
addb:				; c=a+b;
	mov	rax,[a]	 	; load a
	add	rax,[b]		; add b
	mov	[c],rax		; store into c
	pabc	"c=a+b"		; invoke the print macro
	
subb:				; c=a-b;
	mov	rax,[a]	 	; load a
	sub	rax,[b]		; subtract b
	mov	[c],rax		; store into c
	pabc	"c=a-b"		; invoke the print macro
	
mulb:				; c=a*b;
	mov	rax,[a]	 	; load a (must be rax for multiply)
	imul	qword [b]	; signed integer multiply by b
	mov	[c],rax		; store bottom half of product into c
	pabc	"c=a*b"		; invoke the print macro
	
diva:				; c=c/a;
	mov	rax,[c]	 	; load c
	mov	rdx,0		; load upper half of dividend with zero
	idiv	qword [a]	; divide double register edx rax by a
	mov	[c],rax		; store quotient into c
	pabc	"c=c/a"		; invoke the print macro

	pop	rbp		; pop stack
        mov     rax,0           ; exit code, 0=normal
	ret			; main returns to operating system

