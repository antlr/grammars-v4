; Source: https://redirect.cs.umbc.edu/portal/help/nasm/fltarith_64.asm
; fltarith_64.asm   show some simple C code and corresponding nasm code
;                   the nasm code is one sample, not unique
;
; compile  nasm -f elf64 -l fltarith_64.lst  fltarith_64.asm
; link     gcc -m64 -o fltarith_64  fltarith_64.o
; run      ./fltarith_64
;
; the output from running fltarith and fltarithc is:	
; c=5.0, a=3.000000e+00, b=4.000000e+00, c=5.000000e+00
; c=a+b, a=3.000000e+00, b=4.000000e+00, c=7.000000e+00
; c=a-b, a=3.000000e+00, b=4.000000e+00, c=-1.000000e+00
; c=a*b, a=3.000000e+00, b=4.000000e+00, c=1.200000e+01
; c=c/a, a=3.000000e+00, b=4.000000e+00, c=4.000000e+00
; a=i  , a=8.000000e+00, b=1.600000e+01, c=1.600000e+01
; a<=b , a=8.000000e+00, b=1.600000e+01, c=1.600000e+01
; b==c , a=8.000000e+00, b=1.600000e+01, c=1.600000e+01
;The file  fltarith.c  is:
;  #include <stdio.h>
;  int main()
;  { 
;    double a=3.0, b=4.0, c;
;    long int i=8;
;
;    c=5.0;
;    printf("%s, a=%e, b=%e, c=%e\n","c=5.0", a, b, c);
;    c=a+b;
;    printf("%s, a=%e, b=%e, c=%e\n","c=a+b", a, b, c);
;    c=a-b;
;    printf("%s, a=%e, b=%e, c=%e\n","c=a-b", a, b, c);
;    c=a*b;
;    printf("%s, a=%e, b=%e, c=%e\n","c=a*b", a, b, c);
;    c=c/a;
;    printf("%s, a=%e, b=%e, c=%e\n","c=c/a", a, b, c);
;    a=i;
;    b=a+i;
;    i=b;
;    c=i;
;    printf("%s, a=%e, b=%e, c=%e\n","c=c/a", a, b, c);
;    if(a<b) printf("%s, a=%e, b=%e, c=%e\n","a<=b ", a, b, c);
;    else    printf("%s, a=%e, b=%e, c=%e\n","a>b  ", a, b, c);
;    if(b==c)printf("%s, a=%e, b=%e, c=%e\n","b==c ", a, b, c);
;    else    printf("%s, a=%e, b=%e, c=%e\n","b!=c ", a, b, c);
;    return 0;
; }

        extern printf		; the C function to be called

%macro	pabc 1			; a "simple" print macro
	section	.data
.str	db	%1,0		; %1 is macro call first actual parameter
	section .text
				; push onto stack backwards 
        mov	rdi, fmt	; address of format string
	mov	rsi, .str	; string passed to macro
	movq	xmm0, qword [a]	; first floating point in fmt
	movq	xmm1, qword [b]	; second floating point
	movq	xmm2, qword [c]	; third floating point
	mov	rax, 3		; 3 floating point arguments to printf
        call    printf          ; Call C function
%endmacro
	
	section	.data  		; preset constants, writeable
a:	dq	3.0		; 64-bit variable a initialized to 3.0
b:	dq	4.0		; 64-bit variable b initializes to 4.0
i:	dq	8		; a 64 bit integer
five:	dq	5.0		; constant 5.0
fmt:    db "%s, a=%e, b=%e, c=%e",10,0	; format string for printf
	
	section .bss 		; unitialized space
c:	resq	1		; reserve a 64-bit word

	section .text		; instructions, code segment
	global	main		; for gcc standard linking
main:				; label

	push	rbp		; set up stack
lit5:				; c=5.0;
	fld	qword [five]	; 5.0 constant
	fstp	qword [c]	; store into c
	pabc	"c=5.0"		; invoke the print macro
	
addb:				; c=a+b;
	fld	qword [a] 	; load a (pushed on flt pt stack, st0)
	fadd	qword [b]	; floating add b (to st0)
	fstp	qword [c]	; store into c (pop flt pt stack)
	pabc	"c=a+b"		; invoke the print macro
	
subb:				; c=a-b;
	fld	qword [a] 	; load a (pushed on flt pt stack, st0)
	fsub	qword [b]	; floating subtract b (to st0)
	fstp	qword [c]	; store into c (pop flt pt stack)
	pabc	"c=a-b"		; invoke the print macro
	
mulb:				; c=a*b;
	fld	qword [a]	; load a (pushed on flt pt stack, st0)
	fmul	qword [b]	; floating multiply by b (to st0)
	fstp	qword [c]	; store product into c (pop flt pt stack)
	pabc	"c=a*b"		; invoke the print macro
	
diva:				; c=c/a;
	fld	qword [c] 	; load c (pushed on flt pt stack, st0)
	fdiv	qword [a]	; floating divide by a (to st0)
	fstp	qword [c]	; store quotient into c (pop flt pt stack)
	pabc	"c=c/a"		; invoke the print macro

intflt:				; a=i;
	fild	dword [i]	; load integer as floating point
	fst	qword [a]	; store the floating point (no pop)
	fadd	st0		; b=a+i; 'a' as 'i'  already on flt stack
	fst	qword [b]	; store sum (no pop) 'b' still on stack
	fistp	dword [i]	; i=b; store floating point as integer
	fild	dword [i]	; c=i; load again from ram (redundant)
	fstp	qword [c]
	pabc	"a=i  "		; invoke the print macro

cmpflt:	fld	dword [b]	; into st0, then pushed to st1
	fld	dword [a]	; in st0
	fcomip	st0,st1		; a compare b, pop a
	jg	cmpfl2
	pabc	"a<=b "
	jmp	cmpfl3
cmpfl2:	
	pabc	"a>b  "
cmpfl3:
	fld	dword [c]	; should equal [b]
	fcomip  st0,st1
	jne	cmpfl4
	pabc	"b==c "
	jmp	cmpfl5
cmpfl4:
	pabc	"b!=c "
cmpfl5:

	pop	rbp		; pop stack
        mov     rax,0           ; exit code, 0=normal
	ret			; main returns to operating system
