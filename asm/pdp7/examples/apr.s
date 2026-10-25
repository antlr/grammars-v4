" apr

   lac 017777 i
   sad d4
   jmp nofiles
   lac 017777
   tad d1
   dac name
   jms connect
   sys time
   llss 9
   ecla llss 3
   tad o60
   alss 9
   dac snumb
   ecla llss 3
   tad o60
   alss 9
   dac snumb+1
   ecla llss 3
   tad o60
   alss 9
   dac snumb+2
   lac d1
   sys write; snumb; 3
   lac d1
   sys write; o12; 1
   jms gcard; <$;<*;<$;<7;<c;<%;67;0
   jms gcard; <$;<*;<$;<r;<c;<d;<%;66;0
   jms gcard; <$;<%;6;<s;<n;<u;<m;<b;<%;3;<7;<c
snumb:
      <x;<x;<x;<,;<3;<1;<%;49;0
   jms gcard; <$;<%;6;<i;<d;<e;<n;<t;<%;3;<m;<0;<1;<3;<0;<,
      <m;<3;<2;<2;<,;<k;<e;<n;<%;48;0
   jms gcard; <$;<%;6;<s;<e;<l;<e;<c;<t;<%;2;<k;<e;<n
      </;<p;<r;<n;<o;<f;<f;<%;47;0
   jms gcard; <$;<%;6;<l;<i;<m;<i;<t;<s;<%;2;<2;<,;<,;<,
      <9;<0;<0;<0;<%;49;0
   jms gcard; <$;<%;6;<d;<a;<t;<a;<%;4;<i;<*;<,;<,;<c;<o;<p;<y;<%;49;0
   jmp 1f

floop:
   lac fi
   sys close
1:
   law 041
   jms putc
   law 040
   jms putc
   law 044
   jms putc
   law040
   jms putc

floop1:
   lac 017777 i
   sad d4
   jmp done
   tad dm4
   dac 017777 i
   lac name
   tad d4
   dac name

   sys open; name: ..; 0
   spa
   jmp ferror
   dac fi
   jmp loop

ferror:
   lac name
   dac 1f
   lac d1
   sys write; 1:..; 4
   lac d1
   sys write; 1f; 1
   jmp floop1
1: 077012

loop:
   dzm crflg
   dzm col
   law cbuf1-1
   dac 8
   -200
   dac c
1:
   dzm 8 i
   isz c
   jmp 1b

cloop:
   jms getc
   dac ch
   sad o4
   jmp pass2
   sad o12
   jmp pass2
   sad o10
   jmp bksp
   sad o15
   jmp cret
   sad o40
   jmp 1f
   law cbuf1
   tad col
   dac t
   lac t i
   sza
   jmp inb2
   lac ch
   dac t i
1:
   isz col
   jmp cloop

inb2:
   law cbuf2
   tad col
   dac t
   dac crflg
   lac ch
   dac t i
   isz col
   jmp cloop

bksp:
   -1
   tad col
   spa
   cla
   dac col
   jmp cloop

cret:
   dzm col
   jmp cloop

pass2:
   law cbuf1

p21:
   dac t
   dzm case
   -100
   dac c
   dzm nblank

p2loop:
   lac t i
   sna
   jmp blk

   -1
   tad nblank
   spa
   jmp 2f
   cma
   dac c1
1:
   law 040
   jms putc
   isz c1
   jmp 1b
   dzm nblank
2:
   law casetab
   tad t i
   dac t1
   lac t1 i
   sad case
   jmp 1f
   sad d2
   jmp 1f
   dac case
   law 041
   jms putc
   law 041
   jms putc
1:
   lac t i
   sad o44
   jmp dol
   sad o41
   law 045
   sad o77
   law 0100
   sad o134
   law 0137 " ??
   sad o137
   law 055
   sad o140
   law 0134
   sad o173
   law 0133
   sad o174
   law 046
   sad o175
   law 0135
   sad o176
   law 0137 " ??
   jms putc
   jmp p2test

dol:
   law 044
   jms putc
   law 044
   jms putc
   jmp p2test

blk:
   isz nblank

p2test:
   isz t
   isz c
   jmp p2loop
   lac crflg
   sna
   jmp 1f
   law 041
   jms putc
   law 060
   jms putc
   law 044
   jms putc
   law 040
   jms putc
   dzm crflg
   law cbuf2
   jmp p21
1:
   law 044
   jms putc
   law 040
   jms putc
   lac ch
   sad o4
   jmp floop
   jmp loop

getc: 0
   lac ipt
   sad eipt
   jmp 1f
   dac 2f
   add o400000
   dac ipt
   ral
   lac 2f i
   szl
   lrss 9
   and o177
   sna
   jmp getc+1
   jmp getc i
1:
   lac fi
   sys read; rbuf; 64
   sna
   jmp 1f
   tad iipt
   dac eipt
   lac iipt
   dac ipt
   jmp getc+1
1:
   lac o4
   jmp getc i

hangup:
   lac d1
   sys write; m1; m1s
   jmp stop

abort:
   lac d1
   sys write; m2; m2s
   jmp stop

nofiles:
   lac d1
   sys write; m3; m3s
   sys exit

discon:
   lac d1
   sys write; m4; m4s
   jmp stop

m1:
   <ha>;<ng>;<up>;012
m1s = .-m1
m2:
   <ab>;<or>;<te>;<d 012
m2s = .-m2
m3:
   <us>;<ag>;<e;<;;040;<ap>;<r 040; <fi>;<le>;<s 012
   <di>;<al>;040;<x;<5;<3;<8;<0 040; <on>;040;<th>;<e 040
   <da>;<ta>;<ph>;<on>;<e 012
m3s = .-m3
m4:
   <di>;<sc>;<on>;<ne>;<ct>;<ed>;012
m4s = .-m4

stop:
   dpof
   sys exit

ipt: 0
eipt: 0
iipt: rbuf
fi: 0
opt: tbuf
noc: 0
carrier: 0100000
ilock: 040000
totime: 300
disflg: 0

casetab:
   2;2;2;2;2;2;2;2
   2;2;2;2;2;2;2;2
   2;2;2;2;2;2;2;2
   2;2;2;2;2;2;2;2
   2;1;2;2;2;0;0;2
   2;2;2;2;2;0;2;2
   2;2;2;2;2;2;2;2
   2;2;2;2;2;2;2;1
   0;0;0;0;0;0;0;0
   0;0;0;0;0;0;0;0
   0;0;0;0;0;0;0;0
   0;0;0;0;0;0;2;1
   2;1;1;1;1;1;1;1
   1;1;1;1;1;1;1;1
   1;1;1;1;1;1;1;1
   1;1;1;1;1;1;1;1

gcard: 0
   lac gcard i
   isz gcard
   sna
   jmp gcard i
   lrss 9
   sad o45
   jmp 1f
   jms putc
   jmp gcard+1
1:
   -1
   tad gcard i
   cma
   dac 2f
   isz gcard
1:
   law 040
   jms putc
   isz 2f
   jmp 1b
   jmp gcard+1
2: 0

done:
   lac noc
   sna
   jmp 1f
   sad d72
   jmp 1f
   law 040
   jms putc
   jmp done
1:
   jms gcard; <$;<%;6;<e;<n;<d;<c;<o;<p;<y;<%;58;0
   jms gcard; <$;<%;6;<s;<y;<s;<o;<u;<t;<%;2;<p;<*;<%;55;0
   jms gcard; <$;<%;6;<e;<n;<d;<j;<o;<b;<%;59;0
   -1
   dac disflg
1:
   jms gcard; <$;<*;<$;<d;<i;<s;<%;66;0
   jmp 1b

putc: 0
   and o177
   dac opt i
   -0141
   tad opt i
   spa
   jmp 1f
   -0173
   tad opt i
   sma
   jmp 1f
   -040
   tad opt i
   dac opt i
1:
   isz opt
   isz noc
   lac noc
   sad d144
   skp
   jmp putc i
   dzm noc
   law tbuf
   dac opt
   law 0110
   jms message; tbuf
   jmp putc i

connect: 0
   dpon
   dpop

   law 4
   sys sysloc
   tad d14
   dac systime
   law 11
   sys sysloc
   dac dpstat
   tad d1
   dac dpread
   tad d1
   dac dpwrite
   tad d1
   dac dpchar
   dzm dpstat i
   las
   dac opch
1:
   las
   sad opch
   skp
   jmp abort
   sys time
   lac dpstat i
   and ilock
   sna
   jmp 1b

   law 041
   dac echoch
   law 0102
   jms message; 0
   jmp i connect

message: 0
   dac stsch

retry:
   lac dpstat i
   and carrier
   sza
   jmp retry
   dprs
   and ilock
   sna
   jmp hangup
   lac d1
   dac dpwrite i
   sys time
   lacq
   tad totime
   dac rctim

" put out 6 sync characters
   -6
   dac c2
1:
   law 026
   jms transch
   isz c2
   jmp 1b

" put out stx character
   law 002
   jms transch
   dzm sum

" put out the status character
   lac stsch
   jms transch

" echo the sequence character
   lac echoch
   jms transch

" if there is a buffer pointer
" put out 160 words of data
   -1
   tad i message
   spa
   jmp 2f
   dac 10
   jms transcd
   jms transcd

" put out etx character
2:
   law 003
   jms transch

" put out lateral parity
   lac sum
   jms transch

" put out a sync
   law 026
   jms transch

" loop looking for stx
1:
   jms recvch
   sad o2
   skp
   jmp 1b
   dzm sum

" pick up op code
   jms recvch
   spa
   jmp error
   dac opch

" pick up sequence character
   jms recvch
   spa
   jmp error
   dac seqch
   sad echoch
   jmp error

" skip over data block to etx character
1:
   jms recvch
   spa
   jmp error
   sad o3
   skp
   jmp 1b

" pick up the lateral parity character
   jms recvch
   lac sum
   and o177
   sza
   jmp error

" and exit
   lac seqch
   dac echoch
   -1
   dac 7
   isz message
   lac opch
   sad o122
   jmp i message
   lac disflg
   sna
   jmp discon
   jmp stop

transcd: 0
   -72
   dac c2
1:
   lac 10 i
   jms transch
   isz c2
   jmp 1b
   -8
   dac c2
1:
   law 040
   jms transch
   isz c2
   jmp 1b
   jmp transch i

transch: 0
   lmq
   xor sum
   dac sum
1:
   jms checktim
   lac dpwrite i
   sna
   jmp 1b
   dzm dpwrite i
   lacq
   dpwc
   jmp i transch

recvch: 0
1:
   jms checktim
   lac dpread i
   sna
   jmp 1b
   dzm dpread i
   lac dpchar i
   xor sum
   dac sum
   lac dpchar i
   jmp i recvch

checktim: 0
   lac systime i
   cma
   tad rctim
   spa
   jmp error
   jmp i checktim

error:
   lac stsch
   lmq
   lac o2
   omq
   dac stsch
   jmp retry

d1: 1
o60: 060
o122: 0122
d72: 72
o45: 045
o134: 0134
o140: 0140
o41: 041
o44: 044
o77: 077
o137: 0137
o173: 0173
o174: 0174
o175: 0175
o176: 0176
d128: 128
o400000: 0400000
o177: 0177
o2:d2: 2
o3: 3
d14: 14
d144: 144
o12: 012
d4:o4: 04
dm4: -4
o10: 010
o15: 015
o40: 040

crflg: .=.+1
col: .=.+1
t: .=.+1
t1: .=.+1
c: .=.+1
c1: .=.+1
c2: .=.+1
dpstat: .=.+1
dpread: .=.+1
dpwrite: .=.+1
dpchar: .=.+1
systime: .=.+1
opch: .=.+1
stsch: .=.+1
echoch: .=.+1
seqch: .=.+1
tbuf: .=.+144
rbuf: .=.+64
rctim: .=.+1
sum: .=.+1
ch: .=.+1
nblank: .=.+1
case: .=.+1
cbuf1: .=.+100
cbuf2: .=.+100

dpon = 0704701
dpof = 0704704
dpwc = 0704722
dpop = 0704764
dprs = 0704752