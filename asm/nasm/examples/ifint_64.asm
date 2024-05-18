; Source: https://redirect.cs.umbc.edu/portal/help/nasm/ifint_64.asm
; ifint_64.asm  code ifint_64.c for nasm 
; /* ifint_64.c an 'if' statement that will be coded for nasm */
; #include <stdio.h>
; int main()
; {
;   long int a=1;
;   long int b=2;
;   long int c=3;
;   if(a<b)
;     printf("true a < b \n");
;   else
;     printf("wrong on a < b \n");
;   if(b>c)
;     printf("wrong on b > c \n");
;   else
;     printf("false b > c \n");
;   return 0;
;}
; result of executing both "C" and assembly is:
; true a < b
; false b > c 
	
	global	main		; define for linker
        extern	printf		; tell linker we need this C function
        section .data		; Data section, initialized variables
a:	dq 1
b:	dq 2
c:	dq 3
fmt1:   db "true a < b ",10,0
fmt2:   db "wrong on a < b ",10,0
fmt3:   db "wrong on b > c ",10,0
fmt4:   db "false b > c ",10,0

	section .text
main:	push	rbp		; set up stack
	mov	rax,[a]		; a
	cmp	rax,[b]		; compare a to b
	jge	false1		; choose jump to false part
	; a < b sign is set
        mov	rdi, fmt1	; printf("true a < b \n"); 
        call    printf	
        jmp	exit1		; jump over false part
false1:	;  a < b is false 
        mov	rdi, fmt2	; printf("wrong on a < b \n");
        call    printf
exit1:				; finished 'if' statement

	mov	rax,[b]		; b
	cmp	rax,[c]		; compare b to c
	jle	false2		; choose jump to false part
	; b > c sign is not set
        mov	rdi, fmt3	; printf("wrong on b > c \n");
        call    printf	
        jmp	exit2		; jump over false part
false2:	;  a < b is false 
        mov	rdi, fmt4	; printf("false b > c \n");
        call    printf
exit2:				; finished 'if' statement

	pop	rbp		; restore stack
	mov	rax,0		; normal, no error, return value
	ret			; return 0;
