; Source: https://redirect.cs.umbc.edu/portal/help/nasm/shift_64.asm
; shift_64.asm    the nasm code is one sample, not unique
;
; compile:	nasm -f elf64 -l shift_64.lst  shift_64.asm
; link:		gcc -o shift_64  shift_64.o
; run:		./shift_64 > shift_64.out
;
; the output from running shift.asm (zero filled) is:	
; shl rax,4, old rax=ABCDEF0987654321, new rax=BCDEF09876543210, 
; shl rax,8, old rax=ABCDEF0987654321, new rax=CDEF098765432100, 
; shr rax,4, old rax=ABCDEF0987654321, new rax= ABCDEF098765432, 
; sal rax,8, old rax=ABCDEF0987654321, new rax=CDEF098765432100, 
; sar rax,4, old rax=ABCDEF0987654321, new rax=FABCDEF098765432, 
; rol rax,4, old rax=ABCDEF0987654321, new rax=BCDEF0987654321A, 
; ror rax,4, old rax=ABCDEF0987654321, new rax=1ABCDEF098765432, 
; shld rdx,rax,8, old rdx:rax=0,ABCDEF0987654321,
;                 new rax=ABCDEF0987654321 rdx=              AB, 
; shl rax,8     , old rdx:rax=0,ABCDEF0987654321,
;                 new rax=CDEF098765432100 rdx=              AB, 
; shrd rdx,rax,8, old rdx:rax=0,ABCDEF0987654321,
;                 new rax=ABCDEF0987654321 rdx=2100000000000000, 
; shr rax,8     , old rdx:rax=0,ABCDEF0987654321,
;                 new rax=  ABCDEF09876543 rdx=2100000000000000, 

        extern printf		; the C function to be called

%macro	prt	1		; old and new rax
	section .data
.str	db	%1,0		; %1 is which shift string
	section .text
        mov	rdi, fmt	; address of format string
	mov	rsi, .str 	; callers string
	mov	rdx,rax		; new value
	mov	rax, 0		; no floating point
        call    printf          ; Call C function
%endmacro

%macro	prt2	1		; old and new rax,rdx
	section .data
.str	db	%1,0		; %1 is which shift
	section .text
        mov	rdi, fmt2	; address of format string
	mov	rsi, .str 	; callers string
	mov	rcx, rdx	; new rdx befor next because used
	mov	rdx, rax	; new rax
	mov	rax, 0		; no floating point
        call    printf          ; Call C function
%endmacro

	 section .bss
raxsave: resq	1		; save rax while calling a function 
rdxsave: resq	1		; save rdx while calling a function 
	
	section .data  		; preset constants, writeable
b64:	dq	0xABCDEF0987654321	; data to shift
fmt:    db "%s, old rax=ABCDEF0987654321, new rax=%16lX, ",10,0	; format string
fmt2:   db "%s, old rdx:rax=0,ABCDEF0987654321,",10,"                new rax=%16lX rdx=%16lX, ",10,0
	
	section .text		; instructions, code segment
	global	 main		; for gcc standard linking
main:	push	rbp		; set up stack
	
shl1:	mov	rax, [b64]	; data to shift
	shl	rax, 4		; shift rax 4 bits, one hex position left
	prt	"shl rax,4 "	; invoke the print macro

shl4:	mov	rax, [b64]	; data to shift
	shl	rax,8		; shift rax 8 bits. two hex positions left
	prt	"shl rax,8 "	; invoke the print macro

shr4:	mov	rax, [b64]	; data to shift
	shr	rax,4		; shift
	prt	"shr rax,4 "	; invoke the print macro

sal4:	mov	rax, [b64]	; data to shift
	sal	rax,8		; shift
	prt	"sal rax,8 "	; invoke the print macro

sar4:	mov	rax, [b64]	; data to shift
	sar	rax,4		; shift
	prt	"sar rax,4 "	; invoke the print macro

rol4:	mov	rax, [b64]	; data to shift
	rol	rax,4		; shift
	prt	"rol rax,4 "	; invoke the print macro

ror4:	mov	rax, [b64]	; data to shift
	ror	rax,4		; shift
	prt	"ror rax,4 "	; invoke the print macro

shld4:	mov	rax, [b64]	; data to shift
	mov	rdx,0		; register receiving bits
	shld	rdx,rax,8	; shift
	mov	[raxsave],rax	; save, destroyed by function
	mov	[rdxsave],rdx	; save, destroyed by function
	prt2	"shld rdx,rax,8"; invoke the print macro

shla:	mov	rax,[raxsave]	; restore, destroyed by function
	mov	rdx,[rdxsave]	; restore, destroyed by function
	shl	rax,8		; finish double shift, both registers
	prt2	"shl rax,8     "; invoke the print macro

shrd4:	mov	rax, [b64]	; data to shift
	mov	rdx,0		; register receiving bits
	shrd	rdx,rax,8	; shift
	mov	[raxsave],rax	; save, destroyed by function
	mov	[rdxsave],rdx	; save, destroyed by function
	prt2	"shrd rdx,rax,8"; invoke the print macro

shra:	mov	rax,[raxsave]	; restore, destroyed by function
	mov	rdx,[rdxsave]	; restore, destroyed by function
	shr	rax,8		; finish double shift, both registers
	prt2	"shr rax,8     "; invoke the print macro

	pop	rbp		; restore stack
	mov     rax,0           ; exit code, 0=normal
	ret			; main returns to operating system

