; Source: https://redirect.cs.umbc.edu/portal/help/nasm/intlogic_64.asm
; intlogic_64.asm    show some simple C code and corresponding nasm code
;                    the nasm code is one sample, not unique
;
; compile:	nasm -f elf64 -l intlogic_64.lst  intlogic_64.asm
; link:		gcc -m64 -o intlogic_64  intlogic_64.o
; run:		./intlogic_64 > intlogic_64.out
;
; the output from running intlogic_64.asm and intlogic.c is
; c=5  , a=3, b=5, c=15
; c=a&b, a=3, b=5, c=1
; c=a|b, a=3, b=5, c=7
; c=a^b, a=3, b=5, c=6
; c=~a , a=3, b=5, c=-4
;
;The file  intlogic.c  is:
;  /* intlogic.c */
;  #include <stdio.h>
;  int main()
;  { 
;    long int a=3, b=5, c;
;
;    c=15;
;    printf("%s, a=%ld, b=%ld, c=%ld\n","c=5  ", a, b, c);
;    c=a&b; /* and */
;    printf("%s, a=%ld, b=%ld, c=%ld\n","c=a&b", a, b, c);
;    c=a|b; /* or */
;    printf("%s, a=%ld, b=%ld, c=%ld\n","c=a|b", a, b, c);
;    c=a^b; /* xor */
;    printf("%s, a=%ld, b=%ld, c=%ld\n","c=a^b", a, b, c);
;    c=~a;  /* not */
;    printf("%s, a=%ld, b=%ld, c=%ld\n","c=~a", a, b, c);
;    return 0;
; }

        extern printf		; the C function to be called

%macro	pabc 1			; a "simple" print macro
	section .data
.str	db	%1,0		; %1 is first actual in macro call
	section .text
        mov	rdi, fmt        ; address of format string
	mov	rsi, .str 	; users string
	mov	rdx, [a]	; long int a
	mov	rcx, [b]	; long int b 
	mov	r8, [c]		; long int c
	mov     rax, 0	        ; no xmm used
        call    printf          ; Call C function
%endmacro
	
	section .data  		; preset constants, writeable
a:	dq	3		; 64-bit variable a initialized to 3
b:	dq	5		; 64-bit variable b initializes to 4
fmt:    db "%s, a=%ld, b=%ld, c=%ld",10,0 ; format string for printf
	
	section .bss 		; unitialized space
c:	resq	1		; reserve a 64-bit word

	section .text		; instructions, code segment
	global	 main		; for gcc standard linking
main:				; label
	push	rbp		; set up stack
	
lit5:				; c=5;
	mov	rax,15	 	; 5 is a literal constant
	mov	[c],rax		; store into c
	pabc	"c=5  "		; invoke the print macro
	
andb:				; c=a&b;
	mov	rax,[a]	 	; load a
	and	rax,[b]		; and with b
	mov	[c],rax		; store into c
	pabc	"c=a&b"		; invoke the print macro
	
orw:				; c=a-b;
	mov	rax,[a]	 	; load a
	or	rax,[b]		; logical or with b
	mov	[c],rax		; store into c
	pabc	"c=a|b"		; invoke the print macro
	
xorw:				; c=a^b;
	mov	rax,[a]	 	; load a
	xor	rax,[b] 	; exclusive or with b
	mov	[c],rax		; store result in c
	pabc	"c=a^b"		; invoke the print macro
	
notw:				; c=~a;
	mov	rax,[a]	 	; load c
	not	rax	 	; not, complement
	mov	[c],rax		; store result into c
	pabc	"c=~a "		; invoke the print macro

	pop	rbp		; restore stack
	mov     rax,0           ; exit code, 0=normal
	ret			; main returns to operating system
