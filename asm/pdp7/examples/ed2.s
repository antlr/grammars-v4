"** 08-rest.pdf page 21
"[handwritten page number top right of scan - 14]
" ed2

cs:
   jms getsc; tal
   sad o40
   jmp cs
   sad o12
   jmp error
   dac delim
   jms compile
   lac tbufp
   dac tal1
1:
   jms getsc; tal
   sad delim
   jmp 1f
   sad o12
   jmp error
   jms putsc; tal1
   jmp 1b
1:
   lac o12
   jms putsc; tal1
   jms newline
   jms setdd
   lac addr1
   sad zerop
   jmp error
1:
   dac addr1
   lac i addr1
   jms execute
   jmp 2f
   lac addr1
   dac dot
   law line-1
   dac 8
   law nlist-1
   dac 9
   -64
   dac c1
3:
   lac i 8
   dac i 9
   isz c1
   jmp 3b
   -1
   tad fchrno
   dac linsiz
   rcr
   szl
   xor o400000
   tad linep
   dac tal1
   lac tbufp
   dac tal
3:
   jms getsc; tal
   sad o12
   jmp 3f
   jms putsc; tal1
   isz linsiz
"** 08-rest.pdf page 22
"[handwritten page number top right of scan - 15]
   jmp 3b
3:
   -1
   tad lchrno
   rcr
   szl
   xor o400000
   tad nlistp
   dac tal
3:
   jms getsc; tal
   jms putsc; tal1
   isz linsiz
   sad o12
   skp
   jmp 3b
   jms addline
2:
   lac addr1
   sad addr2
   jmp advanc
   tad d1
   jmp 1b

fsrch:
   dac delim
   jms compile
   jms srcsav
   lac dot
floop:
   tad d1
   dac addr
   lac i addr
   sza
   jmp 1f
   lac zerop
   dac addr
   jmp 2f
1:
   jms execute
   jmp 2f
   jms srcres
   jmp ad1
2:
   lac addr
   sad dot
   jmp error
   jmp floop

bsrch:
   dac delim
   jms compile
   jms srcsav
   lac dot
   sad zerop
   lac eofp
bloop:
   tad dm1
   dac addr
   lac i addr
"** 08-rest.pdf page 23
"[handwritten page number top right of scan - 16]
   sza
   jmp 1f
   lac eofp
   dac addr
   jmp 2f
1:
   jms execute
   jmp 2f
   jms srcres
   jmp ad1
2:
   lac addr
   sad dot
   jmp error
   jmp bloop

srcsav: 0
   lac minflg
   sza
   jmp error
   lac addr
   sma
   jmp error
   law line-1
   dac 8
   law tbuf-1
   dac 9
   -64
   dac c1
1:
   lac i 8
   dac i 9
   isz c1
   jmp 1b
   jmp i srcsav

srcres: 0
   law tbuf-1
   dac 8
   law line-1
   dac 9
   -64
   dac c1
1:
   lac i 8
   dac i 9
   isz c1
   jmp 1b
   jmp i srcres

compile: 0
   law compbuf-1
   dac 8
   dzm prev
   dzm compflg

cadvanc:
   jms getsc; tal
   sad delim
   jmp cdone
   dac compflg
"** 08-rest.pdf page 24
"[handwritten page number top right of scan - 17]
   dzm lastre
   sad o12			" newline
   jmp error
   "sad o133			" [
   "jmp chrcls
   sad o136			" ^?
   jmp beglin
   sad o44			" $
   jmp endlin
   "sad o52			" * (disabled)
   "jmp clsure
   dac 1f
   jms comp
   1; jms matchar; 1: 0; 0
   jmp cadvanc

cdone:
   lac compflg
   sna
   jmp 1f
   dac lastre
   jms comp
   1; jms found; 0
   jmp i compile
1: "???
   lac lastre
   sna
   jmp error
   jmp i compile

chrcls:
  jmp error

beglin: "???
   jms comp
   1; jms matbol; 0
   dzm prev
   jmp cadvanc

endlin: "???
   jms comp
   1; jms mateol; 0
   dzm prev
   jmp cadvanc

clsure:
   lac prev
   sna
   jmp error
   tad d1
   dac 1f
   jms comp
   1; jms matclo; 1: 0; 0
   dzm prev
   jmp cadvanc

comp: 0 "???
   -1
   tad comp
   dac 9
   lac 8
"** 08-rest.pdf page 25
"[handwritten page number top right of scan - 18]
   dac prev
1: "???
   lac i 9
   sna
   jmp i 9
   dac i 8
   jmp 1b

execute: 0
   jms gline
   lac linep
   dac tal1
   dzm charno
   dzm fchrno
   dzm lchrno
   lac jmpclist
   dac trvect
   lac jmpnlist
   dac trvect+1
   lac jmpxchg
   dac i trvect+1
   jmp 1f

xchg:
   lacq
   sad o12
   jmp i execute
   lac jmpxchg
   dac i 8
1: "???
   lac trvect
   lmq
   lac trvect+1
   dac trvect
   lacq
   dac trvect+1
   tad dm1
   dac 8
   jms getsc; tal1
   lmq
   isz charno
   jms compbuf
charno:
   0
trvect:
   0;0

matchar: 0 "???
   -2
   tad matchar
   dac exret
   lac i exret
   dac exret
   lacq
   sad i matchar
   skp
   jmp 1f
   lac matchar
   and o17777
   tad jms1
   dac i 8
"** 08-rest.pdf page 26
"[handwritten page number top right of scan - 19]
   lac i exret
   dac i 8
1: "???
   isz exret
   jmp i exret

found: 0
   -2
   tad found
   dac exret
   lac i exret
   dac exret
   lac fchrno
   sza
   jmp 1f
   isz execute
   jmp 2f
1: "???
   sad i exret
   jmp 1f
   cma
   tad i exret
   spa
   jmp 2f
   jmp 3f
1: "???
   lac charno
   cma
   tad charno
   spa
   jmp 3f
2: "???
   lac i exret
   dac fchrno
   lac charno
   dac lchrno
3: "???
   isz exret
   jmp i exret

matbol: 0 "???
   lac charno
   sad d1
   jmp 1f
   lac matbol
   jmp 2f
1: "???
   lac matbol
   jmp 3f

mateol: 0 "???
   lacq
   sad o12
   jmp 1f
   lac mateol
2: "???
   tad dm2
   dac exret
   lac i exret
   dac 9
   jmp i 9
"** 08-rest.pdf page 27
"[handwritten page number top right of scan - 20]
1: "???
   lac mateol
3: "???
   tad dm3
   dac 9
   lac i 9
   isz 9
   dac i 9
   jmp i 9

matclo: 0 "???
   -2
   tad matclo
   dac exret
   lac i exret
   dac cloret
   lac i cloret
   dac 1f
   dac 2f
   lac i matclo
   dac exret
   jms i exret; 1: 0
   isz matclo
   jms i matclo; 2: 0
   isz cloret
   jmp i cloret

"??? the remainder of this scan had an unreadable first character
"??? I did the best I could to recreate the characters appropriately
d1: 1
o133: 0133
dm3: -3
o136: 0136
dm2: -2
o52: 052
o57: 057
o77: 077
o40: 040
o12: 012
d47: 47
d58: 58
dm48: -48
d10: 10
d8: 8
d48: o60: 060
d100000: 100000
o44: 044
o53: 053
o56: 056
o55: 055
o11: 011
o400000: 0400000
o17777: 017777
o144: 0144
dm1: -1
o56012: 056012
o777: 0777
o100: 0100
o43: 043
o777000: 0777000
o75: 075
o167: 0167
o161: 0161
"** 08-rest.pdf page 28
"[handwritten page number top right of scan - 21]
o160: 0160
o143: 0143
o141: 0141
o1777: 01777
d1024: 1024
o776000: 0776000
o162: 0162
o163: 0163
o73: 073
o54: 054
o17: 017

tname:
   0145056;0164155;0160040;040040
tbufp: tbuf
linep: line
nlistp: nlist
fbufp: fbuf
dskbfp: dskbuf "[line crossed out - scan markup]
edskbfp: dskbuf+1024 "[line crossed out - scan markup]
lnodp: lnodes
linpm1: line-1
jmpclist: jmp clist
jmpnlist: jmp nlist
jmpxchg: jmp xchg
jms1: jms 1
tal: .=.+1
exret: .=.+1
cloret: .=.+1
delim: .=.+1
prev: .=.+1
compflg: .=.+1
tal1: .=.+1
c1: .=.+1
ital: .=.+1
otal: .=.+1
diskin: .=.+1
glint1: .=.+1
c2: .=.+1
num: .=.+1
zermp: .=.+1
minflg: .=.+1
adrflg: .=.+1
dot: .=.+1
addr: .=.+1
addr1: .=.+1
addr2: .=.+1
eofp: .=.+1
zerop: .=.+1
dskadr: .=.+1
linsiz: .=.+1
tfi: .=.+1
fchrno: .=.+1
lchrno: .=.+1
lastre: .=.+1
bett1: .=.+1
bett2: .=.+1
wrflg: .=.+1
apt1: .=.+1
sfi: .=.+1			" e.tmp input fd
"** 08-rest.pdf page 29
"[handwritten page number top right of scan - 22]
sfo: .=.+1			" e.tmp output fd
sctal: .=.+1
sctalp: .=.+1
char: .=.+1
fbuf: .=.+4  "not sure if this is fbuf, but
tbuf: .=.+64 "there is a write; tbuf; 64 call
line: .=.+64
nlist: .=.+50
clist: .=.+50
compbuf: .=.+100
dskbuf: .=.+1024 "[line crossed out - scan markup]
lnodes: .=.+1000
