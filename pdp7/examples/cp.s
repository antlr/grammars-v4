" cp

   lac 017777
   tad d1
   dac name2
loop:
   lac 017777 i
   sad d4
   sys exit
   sad d8
   jmp unbal
   tad dm8
   dac 017777 i
   lac name2
   tad d4
   dac name1
   tad d4
   dac name2
   sys open; name1: 0; 0
   spa
   jmp error
   lac o17
   sys creat; name2: 0
   spa
   jmp error
   dzm nin

1:
   lac bufp
   tad nin
   dac 0f
   -1
   tad nin
   cma
   tad d1024
   dac 0f+1
   lac d2
   sys read; 0:..;..
   sna
   jmp 2f
   tad nin
   dac nin
   sad d1024
   jmp 2f
   jmp 1b
2:
   lac nin
   dac 2f
   lac d3
   sys write; buf; 2: 0
   dzm nin
   lac 2b
   sad d1024
   jmp 1b
   lac d2
   sys close
   lac d3
   sys close
   jmp loop
error:
   lac name1
   dac 1f
   lac d1
   sys write; 1: 0; 4
   lac d1
   sys write; mes; 1
   lac name2
   dac 1f
   lac d1
   sys write; 1: 0; 4
   lac d1
   sys write; mes; 2
   jmp loop
mes:
   040000;077012
unbal:
   lac name2
   tad d4
   dac 1f
   lac d1
   sys write; 1: 0; 4
   lac d1
   sys write; mes; 2
   sys exit

d1: 1
d4: 4
d8: 8
o17: 017
dm8: -8
d3: 3
d1024: 1024
nin: 0
bufp: buf
d2: 2

buf: