.model small,pascal,nearstack
.386

include win.inc
includelib libw.lib
extern __astart:proc

.data
text sbyte "Hello f*** World!",0
title sbyte "Win",0

.code
WinMain    PROC, hInstance:HANDLE, hPrevInstance:HANDLE, lpszCmdLine:LPSTR, nCmdShow,WORD
  LOCAL msg:MSG

 invoke MessageBox, NULL, addr text, addr title, 0
 invoke PostQuitMessage,0

 .while TRUE
     invoke GetMessage,addr msg,NULL,0,0
     .break .if (ax == 0)
     invoke TranslateMessage,addr msg
     invoke DispatchMessage,addr msg
 .endw
WinMain    ENDP
END        __astart
