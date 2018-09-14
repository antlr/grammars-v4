; ----------------------------------------------------------------------------
; powers.asm
;
; Displays powers of 2 from 2^0 to 2^31, one per line, to standard output.
;
; Processor: 386 or later
; Assembler: MASM
; OS: Any Win32-based OS
; Other libraries: Use a Microsoft-compatible C library (e.g. libc.lib).
; Assemble with "ml powers.asm /c"
; Link with "link powers libc.lib"
;
; By default, the linker uses "/subsystem:console /entry:mainCRTStartup".
; The function "mainCRTStartup" is inside libc.lib.  It does some
; initialization, calls a function "_main" (which will end up in powers.obj)
; then does more work and finally calls ExitProcess.
; ----------------------------------------------------------------------------

        .386P
        .model  flat
        extern  _printf:near
        public  _main

        .code
_main:
        push    esi                     ; callee-save registers
        push    edi
        
        mov     esi, 1                  ; current value
        mov     edi, 31                 ; counter                
L1:
        push    esi                     ; push value to print
        push    offset format           ; push address of format string
        call    _printf
        add     esp, 8                  ; pop off parameters passed to printf
        add     esi, esi                ; double value
        dec     edi                     ; keep counting
        jnz     L1

        pop     edi
        pop     esi
        ret
        
format: byte    '%d', 10, 0

        end
        