"** 01-s1.pdf page 65
" trysys

   sys open; a.out; 0
   spa
   jmp error
   sys read; buf; 3072
   sad .-1
   jmp error
   iof
   caf
   cdf
   clof
   law buf
   dac t1
   dzm t2
   -3072
   dac c1
1:
   lac t1 i
   dac t2 i
   isz t1
   isz t2
   isz c1
   jmp 1b
   jmp 0100

error:
   lac d1
   sys write; 1f; 1
   sys exit
1: 077012

a.out:
   <a.>;<ou>;<t 040; 040040
t1: 0
t2: 0
c1: 0
d1: 1
buf:
