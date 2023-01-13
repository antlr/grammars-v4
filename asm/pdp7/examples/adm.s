" adm

   lac 017777 i
   sad d4
   jmp nofiles
   lac 017777
   tad d1
   dac name
   jms connect
   sys time
   llss 9
   ecla lls 3
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
   jms gcard; <$;<*;<$;<7;<c;0
   jms gcard; <$;<*;<$;<r;<c;<d;0
   jms gcard; <$;<%;6;<s;<n;<u;<m;<b;<%;3;<7;<c
snumb:
      <x;<x;<x;<,;<3;<1;0
   jms gcard; <$;<%;6;<i;<d;<e;<n;<t;<%;3;<m;<0;<1;<3;<0;<,
      <m;<3;<2;<2;<,;<k;<e;<n;0
   jms gcard; <$;<%;6;<s;<e;<l;<e;<c;<t;<%;2;<k;<e;<n
      </;<d;<m;<p;<o;<f;<f;0
   jms gcard; <$;<%;6;<l;<i;<m;<i;<t;<s;<%;2;<3;<,;<,;<,
      <9;<0;<0;<0;<0
   jms gcard; <$;<%;6;<d;<a;<t;<a;<%;4;<i;<*;<,
      <n;<c;<k;<s;<u;<m;<,
	 <c;<o;<p;<y;0
   jmp floop1

floop:
   lac fi
   sys close
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

   -1
   tad name
   dac 8
   r4
   dac c1
1:
   lac 8 i
   jms putw
   isz c1
   jmp 1b
   jms gcard; 0
   jms flush

   lac o200500 " first card, 7/9
   dac buf
   dzm buf+1 " seq

cloop:
   dzm buf+2 " word count
   dzm buf+3 " checksum
   law buf+3
   dac 10
   -44
   dac c1

wloop:
   jms getword
      jmp eof
   dac 10 i
   add buf+3
   dac buf+3 " check sum
   isz buf+2 " word count
   isz c1
   jmp wloop

   lac buf+3
   add buf
   add buf+1
   add buf+2
   dac buf+3 " ffinal check sum
   jms putcard
   lac buf
   and o577777 " not first card
   dac buf
   isz buf+1 " sequence
   jmp cloop

eof:
   dzm 10 i
   isz c1
   jmp eof

   lac buf
   xor o400000
   dac buf " last card
   lac buf+3
   add buf
   add buf+1
   add buf+2
   dac buf+3 " final check sum
   jms putcard
   jmp floop

getword: 0
   lac ipt
   sad eipt
   jmp 1f
   lac ipt i
   isz ipt
   isz getword
   jmp getword i
1:
   lac fi
   sys read; ibuf; 64
   sna
   jmp getword i
   tad iipt
   dac eipt
   lac iipt
   dac ipt
   jmp getword+1
ipt: 0
eipt: 0
iipt: ibuf

putcard: 0
   -48
   dac c1
   law buf-1
   dac 10
1:
   lac 10 i
   lmq
   -3
   dac c2
2:
   ecla llss 6
   tad lactab
   dac .+1
   lac ..
   dac opt i
   isz opt
   isz c2
   jmp 2b
   isz c1
   jmp 1b

   -16
   dac c1
   cla
1:
   dac opt i
   isz opt
   isz c1
   jmp 1b
   law 0144
   jms message; tbuf
   law tbuf
   dac opt
   jmp putcard i

   jmp floop

ferror:
   lac name
   dac 1f
   lac d1
   sys write; 1:..; 4
   lac d1
   sys write; 1f; 1
   jmp floop1
1: 077012

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
m2s = .-m1
m2:
   <ab>;<or>;<te>;<d 012
m2s = .-m2
m3:
   <us>;<ag>;<e;<:;040;<ad>;<m 040; <fi>;<le>;<s 012
   <di>;<al>;040;<x;<5;<3;<8;<0 040; <on>;040;<th>;<e 040
   <da>;<ta>;<ph>;<on>;<e 012
m3s = .-m3
m4:
   <di>;<sc>;<on>;<ne>;<ct>;<ed>;012
m4s = .-m4

stop:
   dpof
   las
   and o400000
   sna
   sys save
   sys exit

carrier: 0100000
ilock: 040000
totime: 300
disflg: 0

flush: 0
   lac noc
   sna
   jmp flush i
   law 0104
   jms message; tbuf
   law tbuf
   dac opt
   dzm noc
   jmp flush i

gcard: 0
   lac gcard i
   isz gcard
   sna
   jmp 3f
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
3:
   lac noc
   sna
   jmp gcard i
   sad d80
   jmp gcard i
   law 040
   jms putc
   jmp 3b

done:
   jms gcard; <$;<%;6;<e;<n;<d;<c;<o;<p;<y;0
   jms gcard; <$;<%;6;<s;<y;<s;<o;<u;<t;<%;2;<p;<*;0
   jms gcard; <$;<%;6;<e;<n;<d;<j;<o;<b;0
   -1
   dac disflg
1:
   jms gcard; <$;<*;<$;<d;<i;<s;0
   jmp 1b

putw: 0
   dac 1f
   lrss 9
   jms putc
   lac 1f
   jms putc
   jmp putw i
1: 0

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
   sad d160
   skp
   jmp putc i
   dzm noc
   law tbuf
   dac opt
   law 0110
   jms message; tbuf
   jmp putc i
noc: 0
opt: tbuf

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
   -160
   dac c2
1:
   lac 10 i
   jms transch
   isz c2
   jmp 1b

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
   lac distlg
   sna
   jmp discon
   jmp stop

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
d4: 4
o60: 060
o12: 012
dm4: -4
o45: 045
o177: 0177
d160: 160
d80: 80
d14: 14
o400000: 0400000
o577777: 0577777
o200500: 0200500
o122: 0122
o3: 3
o2: 2

lactab: lac .+1
   0060;0061;0062;0063;0064;0065;0066;0067
   0070;0071;0133;0043;0100;0072;0076;0077
   0040;0101;0102;0103;0104;0105;0106;0107
   0110;0111;0046;0056;0135;0050;0074;0134
   0136;0112;0113;0114;0115;0116;0117;0120
   0121;0122;0055;0044;0052;0051;0073;0047
   0053;0057;0123;0124;0125;0126;0127;0130
   0131;0132;0137;0054;0045;0075;0042;0041

dpstat: .=.+1
dpread: .=.+1
dpwrite: .=.+1
dpchar: .=.+1
systime: .=.+1
opch: .=.+1
stsch: .=.+1
echoch: .=.+1
seqch: .=.+1
tbuf: .=.+160
buf: .=.+48
ibuf: .=.+64
rctim: .=.+1
fi: .=.+1
c1: .=.+1
c2: .=.+1
sum: .=.+1

dpon = 0704701
dpof = 0704704
dpwc = 0704722
dpop = 0704764
dprs = 0704752