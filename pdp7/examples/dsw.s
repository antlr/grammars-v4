" dsw

   lac djmp
   dac .-1
   oas cla
   cma
   tad d1
   dac t1
   sys open; dd; 0
1:
   lac d2
   sys read; dir; 8
   sna
   sys exit
   lac dir
   sna
   jmp 1b
   isz t1
   jmp 1b

wr:
   lac d1
   sys write; dir+1; 4
   lac d1
   sys write; o12; 1
   sys save
io:
   sys unlink; dir+1
   sys exit

d1: 1
d2: 2
o12: 012
t1: 0
djmp: jmp do
dd: 056056;040040;040040;040040
dir: .=.+8