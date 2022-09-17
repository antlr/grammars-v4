; ----------------------------------------------------------------------------
; hello.asm
;
; This is a Win32 console program that writes "Hello, World" on one line and
; then exits.  It uses only plain Win32 system calls from kernel32.dll, so it
; is very instructive to study since it does not make use of a C library.
; Because system calls from kernel32.dll are used, you need to link with
; an import library.
;
; Processor: 386 or later
; Assembler: MASM
; OS: Any Win32-based OS
; Other libraries: Use Microsoft's import library kernel32.lib
; Assemble with "ml hello.asm /c"
; Link with "link hello kernel32.lib /subsystem:console /entry:go"
; ----------------------------------------------------------------------------

        .386P
        .model  flat
        extern  _ExitProcess@4:near
        extern  _GetStdHandle@4:near
        extern  _WriteConsoleA@20:near
        public  _go

        .data
msg     byte    'Hello, World', 10
handle  dword   ?
written dword   ?

        .stack

        .code
_go:    
        ; handle = GetStdHandle(-11)
        push    -11
        call    _GetStdHandle@4
        mov     handle, eax

        ; WriteConsole(handle, &msg[0], 13, &written, 0)
        push    0
        push    offset written
        push    13
        push    offset msg
        push    handle
        call    _WriteConsoleA@20

        ; ExitProcess(0)
        push    0
        call    _ExitProcess@4

        end

