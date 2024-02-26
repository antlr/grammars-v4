; Source: https://redirect.cs.umbc.edu/portal/help/nasm/hello_64.asm
; hello_64.asm    print a string using printf
; Assemble:	  nasm -f elf64 -l hello_64.lst  hello_64.asm
; Link:		  gcc -m64 -o hello_64  hello_64.o
; Run:		  ./hello_64 > hello_64.out
; Output:	  cat hello_64.out

; Equivalent C code
; // hello.c
; #include <stdio.h>
; int main()
; {
;   char msg[] = "Hello world\n";
;   printf("%s\n",msg);
;   return 0;
; }
	
; Declare needed C  functions
        extern	printf		; the C function, to be called

        section .data		; Data section, initialized variables
msg:	db "Hello world", 0	; C string needs 0
fmt:    db "%s", 10, 0          ; The printf format, "\n",'0'

        section .text           ; Code section.

        global main		; the standard gcc entry point
main:				; the program label for the entry point
        push    rbp		; set up stack frame, must be aligned
	
	mov	rdi,fmt
	mov	rsi,msg
	mov	rax,0		; or can be  xor  rax,rax
        call    printf		; Call C function

	pop	rbp		; restore stack

	mov	rax,0		; normal, no error, return value
	ret			; return
