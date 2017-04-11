" init

   -1
   sys intrp
   jms init1
   jms init2
1:
   sys rmes
   sad pid1
   jmp 1f
   sad pid2
   jms init2
   jmp 1
1:
   jms init1
   jmp 1

init1: 0
   sys fork
   jmp 1f
   sys open; ttyin; 0
   sys open; ttyout; 1
   jmp login
1:
   dac pid1
   jmp init1 i

init2: 0
   sys fork
   jmp 1f
   sys open; keybd; 0
   sys open; displ; 1
   jmp login
1:
   dac pid2
   jmp init2 i

login:
   -1
   sys intrp
   sys open; password; 0
   lac d1
   sys write; m1; m1s
   jms rline
   lac ebufp
   dac tal
1:
   jms gline
   law ibuf-1
   dac 8
   law obuf-1
   dac 9
2:
   lac 8 i
   sac o12
   lac o72
   sad 9 i
   skp
   jmp 1b
   sad o72
   skp
   jmp 2b
   lac 9 i
   sad o72
   jmp 1f
   -1
   tad 9
   dac 9
   lac d1
   sys write; m3; m3s
   jms rline
   law ibuf-1
   dac 8
2:
   lac 8 i
   sad o12
   lac o72
   sad 9 i
   skp
   jmp error
   sad o72
   skp
   jmp 2b
1:
   dzm nchar
   law dir-1
   dac 8
1:
   lac 9 i
   sad o72
   jmp 1f
   dac char
   lac nchar
   sza
   jmp 2f
   lac char
   alss 9
   xor o40
   dac 8 i
   dac nchar
   jmp 1b
2:
   lac 8
   dac nchar
   lac nchar i
   and o777000
   xor char
   dac nchar i
   dzm nchar
   jmp 1b
1:
   dzm nchar
1:
   lac 9 i
   sad o12
   jmp 2f
   tad om60
   lmq
   lac nchar
   cll; als 3
   omq
   dac nchar
   jmp 1b
2:
   lac nchar
   sys setuid
   sys chdir; dd
   sys chdir; dir

   lac d2
   sys close
   sys open; sh; 0
   sma
   jmp 1f
   sys link; system; sh; sh
   spa
   jmp error
   sys open; sh; 0
   spa
   jmp error
   sys unlink; sh
1:
   law 017700
   dac 9
   law boot-1
   dac 8
1:
   lac 8 i
   dac 9 i
   sza
   jmp 1b
   jmp 017701

boot:
   lac d2
   lmq
   sys read; 4096; 07700
   lacq
   sys close
   jmp 4096
   0

rline: 0
   law ibuf-1
   dac 8
1:
   cla
   sys read; char; 1
   lac char
   lrss 9
   sad o100
   jmp rline+1
   sad o43
   jmp 2f
   dac 8 i
   sad o12
   jmp rline i
   jmp 1b
2:
   law ibuf-1
   sad 8
   jmp 1b
   -1
   tad 8
   dac 8
   jmp 1b

gline: 0
   law obuf-1
   dac 8
1:
   jms gchar
   dac 8 i
   sad o12
   jmp gline i
   jmp 1b

gchar: 0
   lac tal
   sad ebufp
   jmp 1f
   ral
   lac tal i
   snl
   lrss 9
   and o777
   lmq
   lac tal
   add o400000
   dac tal
   lacq
   sna
   jmp gchar+1
   jmp gchar i
1:
   lac bufp
   dac tal
1:
   dzm tal i
   isz tal
   lac tal
   sad ebufp
   skp
   jmp 1b
   lac bufp
   dac tal
   lac d2
   sys tead; buf; 64
   sna
   jmp error
   jmp gchar+1

error:
   lac d1
   sys write; m2; m2s
   lac d1
   sys smes
   sys exit

m1:
   012; <lo>;<gi>;<n;<:;<
m1s = .-m1
m2:
   <?; 012
m2s = .-m2
m3:
   <pa>;<ss>;<wo>;<rd>;<: 040
m3s = .-m3
dd:
   <dd>;040040;040040;040040
dir:
   040040;040040;040040;040040

ttyin:
   <tt>;<yi>;<n 040;040040
ttyout:
   <tt>;<yo>;<ut>; 040040
keybd:
   <ke>;<yb>;<oa>;<rd>
displ:
   <di>;<sp>;<la>;<y 040
sh: 
   <sh>; 040040;040040;040040
system:
   <sy>;<st>;<em>; 040040
password:
   <pa>;<ss>;<wo>;<rd>

d1: 1
o43: 043
o100: 0100
o400000; 0400000
d2: 2
o12: 012
om60: -060
d3: 3
ebufp: buf+64
bufp: buf
o777: 0777
o777000: 0777000
o40: 040
o72: 072

ibuf: .=.+100
obuf: .=.+100
tal: .=.+1
buf: .=.+64
char: .=.+1
nchar: .=.+1
pid1: .=.+1
pid2: .=.+1
