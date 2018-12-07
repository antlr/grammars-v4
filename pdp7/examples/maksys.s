"** 01-s1.pdf page 64 -- handwritten maksys
" copy a.out to disk track 18x
" where x is the argument

   lac 017777 i; sad d8; skp; jmp error
   lac 017777; tad d5; dac track
   lac i track; lrss 9; tad om60
   spa; jmp error; dac track
   tad dm10; sma; jmp error

   sys open; a.out; 0
   spa; jmp error
   sys read; bufp: buf; 3072
   sad .-1
   jmp error

   dscs
   -3072; dslw
   lac bufp; dslm
   lac track; alss 8; xor o300000; dsld
   lac o3000; dsls
   dssf; jmp .-1
   dsrs; spa; jmp error
   -1024; dslw
   lac d3072; dslm
   lac track; alss 8; xor o300110; dsld
   lac o3000; dsls
   dssf; jmp .-1
   dsrs; spa; jmp error
   sys exit

error:
   lac d1; sys write; 1f; 2
   sys exit
1: 077077;012

dm10: -10
d5: 5
om60: -060
o300000: 0300000
o300110: 0300110
d8: 8
d3072: 3072
o3000: 03000
d1: 1
a.out:
   <a.>;<ou>;<t 040;040040

track: .=.+1

buf:

