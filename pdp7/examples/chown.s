" chown
   lac 017777 i
   sad d4
   jmp error

   lac 017777
   tad d4
   dac 8
   tad d1
   dac name
   dzm octal
   dzm nochar
   -8
   dac c1
1:
   lac nchar
   dzm nchar
   sza
   jmp 2f
   lac 8 i
   lmq
   and o177
   dac nchar
   lacq
   lrss 9
2:
   sad o40
   jmp 3f
   tad om60
   lmq
   lac octal
   cll; als 3
   omq
   dac octal
3:
   isz c1
   jmp 1b

loop:
   lac 017777 i
   sad d8
   sys exit
   tad dm4
   dac 017777 i
   lac name
   tad d4
   dac name
   lac octal
   sys chowner; name:0
   sma
   jmp loop
   lac name
   dac 1f
   lac d1
   sys write; 1:0; 4
   lac d1
   sys write; 1f; 2
   jmp loop
1:
   040;077012
error:
   lac d1
   sys write;  1b+1; 1
   sys exit

om60: -060
o40: 040
d1: 1
d8: 8
dm4: -4
d4: 4
o177: 0177

nchar: .=.+1
c1: .=.+1
octal: .=.+1
