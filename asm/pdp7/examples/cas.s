"** 04-cas.pdf page 1
" cas

	" Usage: cas [ input [ output ] ]
	" graphics display character assembler???

   narg = i 017777

   lac 017777
   tad d5
   dac name1
   tad d4
   dac name2
   lac narg			" get arg count
   sad d4			" 4?
   jmp 1f			"  yes: default both to tty
   sad d8			" 8?
   jmp 2f			"  no: default input to tty
   jmp 3f			" yes: use files from command line
1:
   law ttyout
   dac name1
2:
   law ttyin
   dac name2
3:
   sys open; name2: 0; 0
   sma
   jmp 1f
   lac name2
   dac 2f
   lac d1
   sys write; 2: 0; 4
   lac d1
   sys write; mes; 2
   sys exit
1:
   sys open; name1: 0; 1
   sma
   jmp 1f
   lac name1
   dac 2f
   lac o17
   sys creat; 2: 0
   sma
   jmp 1f
   lac name1
   dac 2f
   lac d1
   sys write; 2: 0; 4
   lac d1
   sys write; mes; 2
   sys exit
mes:
   040077;012
1:
   dzm nchar
   dzm x
   dzm y
   dzm parflg
   dzm nins
   dzm nwds
   lac bufp
   dac 10

advanc:
"** 04-cas.pdf page 2
   lac nchar
   dzm nchar
   sza
   jmp adv1
   lac d2
   sys read; char; 1
   sna
   jmp done
   lac char
   and o777
   dac nchar
   lac char
   lrss 9

adv1:
   sna
   jmp advanc
   dac char
   lac labflg
   sna
   jmp 2f
   lac char
   sad o12
   skp
   jmp 1f
   dzm labflg
   dac i 11
   jmp advanc
1:
   dac i 11
   jmp advanc
2:
   lac parflg
   sza
   jmp atoz
   lac char
   sad o12
   jmp advanc
   sad o72			" ':'?
   skp				" yes
   jmp 1f			"  no
   -4
   dac labflg
   dac mod3
   jms wbuf "???
   lac lbufp
   dac 11
   lac o170072			" "x:"
   dac i 10
   lac o12			" \n
   dac i 10
   lac o60			" 0
   dac i 10
   isz nwds
   isz nwds
   isz nwds
   jmp advanc
1:
   sad o170
   skp
   jmp 1f
"** 04-cas.pdf page 3
   dzm vis
   jmp advanc
1:
   sad o166			" 'v'?
   skp
   jmp 1f
   lac visbit
   dac vis
   jmp advanc
1:
   sad o162			" 'r'?
   skp
   jmp letr
   isz mod3
   skp
   jmp 2f
   lac o60060
1:
   dac i 10
   isz nwds
   isz mod3
   jmp 1b
2:
   lac o12
   dac i 10
   isz nwds
   lac lbufp
   dac 11
    1: "???
   lac i 11
   sad o12
   jmp 1f
   dac i 10
   isz nwds
   jmp 1b
1:
   lac o75170			" =x
   dac i 10
   isz nwds
   lac ob1
   dac i 10
   isz nwds
   lac sp
   dac i 10
   isz nwds
   lac nins
   dzm nins
   tad o100
   lmq
   llss 10
   cla
   llss 3
   alss 6
   llss 3
   tad o60060
   dac i 10
   cla
   llss 3
   alss 6
   llss 3
   tad o60060
"** 04-cas.pdf page 4
   dac i 10
   cla
   llss 3
   alss 6
   llss 3
   tad o60060
   dac i 10
   lac nwds
   tad d4
   dac nwds
   lac o12012
   dac i 10
   dzm x
   dzm y
   jmp advanc
letr:
   tad om141 "???
   spa
   jmp error
   tad dm16
   sma "???
   jmp error
   cma "???
   tad dm3
   dac ny
   -1
   dac parflg
   jmp advanc

atoz:
   lac char
   tad om141
   spa
   jmp error
   tad dm14
   sma
   jmp error
   tad d14
   dac nx

loop:
   -1
   tad x
   cma
   tad nx
   dac delx
   -1
   tad y
   cma
   tad ny
   dac dely
   " generate direction

   lac delx
   sna
   jmp c1
   spa
   jmp c2
   lac dely  ;"dx ,gr, 0
   sna
   jmp c3
"** 04-cas.pdf page 5
   spa
   jmp c4
   lac d1
   jmp b
c3:
   lac d2
   jmp a
c4:
   lac d3
   jmp b

c1:
   lac dely
   sna
   jmp out
   spa
   jmp c5
   cla
   jmp a
c5:
   lac d4
   jmp a
c2:
   lac dely
   sna
   jmp c6
   spa
   jmp c7
   lac d7
   jmp b
c6:
   lac d6
   jmp a
c7:
   lac d5
   jmp b
   "
   "
a:
   dac direc
   lac delx
   sma
   jmp 1f
   cma
   tad d1
   dac delx
1:
   lac dely
   sma
   jmp 1f
   cma
   tad d1
1:
   tad delx
   tad dm4
   sma
   cla
   tad d3
   dac dist
   tad incxp
   dac tmp
"** 04-cas.pdf page 6
   lac i tmp
   dac incx
   lac dist
   tad incyp
   dac tmp
   lac i tmp
   dac incy
   jmp com
   "
b:
   dzm dist
   dac direc
   lac incxt
   dac incx
   lac incyt
   dac incy
   "
com:
   isz nins
   lac dist
   alss 4
   xor vis
   xor direc
   isz mod3
   skp
   jmp 1f
2:
   lmq
   llss 12
   cla
   llss 3
   alss 6
   llss 3
   tad o60060
   dac i 10
   isz nwds
   jmp 3f
1:
   dac tmp
   lac o12060
   dac i 10
   isz nwds
   -3
   dac mod3
   lac tmp
   jmp 2b
3:
   lac direc
   tad incx
   dac tmp
   lac i tmp
   tad x
   dac x
   lac direc
   tad incy
   dac tmp
   lac i tmp
   tad y
   dac y
   jmp loop
"** 04-cas.pdf page 7
out:
   lac nx
   dac x
   lac ny
   dac y
   dzm parflg
   jmp advanc

done:
   lac d2
   sys close
   jms wbuf
   lac d3
   sys close
   sys exit

error:
   lac d1
   sys write; char; 1
   lac d1
   sys write; mes; 2
   dzm parflg
   dzm labflg
   jmp advanc

wbuf: 0
   lac nwds
   dac 1f
   lac d3
   sys write; buf; 1: 0;	" PLB: label was 1f:
   dzm nwds
   lac bufp
   dac 10
   jmp i wbuf

d1: 1
d2: 2
d3: 3
d4: 4
d5: 5
d6: 6
d7: 7
d8: 8
o12: 012
o75170: 075170
ob1: 055142

sp: 053060
o60: 060
o60060: 060060
o73: 073
"d6: 6 "seems like a dupe
d14: 14
dm14: -14
dm16: -16
om141: -0141 "???
dm3: -3
dm4: -4
o162: 0162
o166: 0166
"** 04-cas.pdf page 8
o17: 017
o777: 0777
o72: 072
o170: 0170
o10000: 010000
o20000: 020000
o200000: 0200000
o41: 041
ttyin:
   0164164;0171151;0156040;040040
ttyout:
   0164164;0171157;0165164;040040

char: .=.+1
parflg: .=.+1
labflg: .=.+1
obuf: .=.+8
x: .=.+1
y: .=.+1
nx: .=.+1
ny: .=.+1
vis: .=.+1
nchar: .=.+1
   "
incxp:incxt
incyp:incyt
incxt: x1;x2;x3;x4
incyt: y1;y2;y3;y4
   "
x1: 0;1;1;1;0;-1;-1;-1
x2: 0;2;2;2;0;-2;-2;-2
x3: 0;3;3;3;0;-3;-3;-3
x4: 0;4;4;4;0;-4;-4;-4
y1: 1;1;0;-1;-1;-1;0;1
y2: 2;2;0;-2;-2;-2;0;2
y3: 3;3;0;-3;-3;-3;0;3
y4: 4;4;0;-4;-4;-4;0;4
delx: .=.+1
dely: .=.+1
incx: .=.+1
incy: .=.+1
direc: .=.+1
dist: .=.+1
visbit: 010
mod3: .=.+1
tmp: .=.+1
buf: .=.+500
bufp: buf-1
lbuf: .=.+10
lbufp: lbuf-1
nwds: .=.+1
o170072: 0170072
nins: .=.+1
o100: 0100
o12012: 012012
o12060: 012060
