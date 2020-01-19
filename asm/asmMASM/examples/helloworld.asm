.386
.model flat, stdcall
option casemap: none

include windows.inc
include user32.inc
include kernel32.inc
includelib user32.lib
includelib kernel32.lib

.data
    szCaption   db  'Hello', 0
    szText      db  'Hello, World!', 0

.code
    start:
            invoke MessageBox, NULL, offset szText, offset szCaption, MB_OK
            invoke ExitProcess, NULL        
    end start
