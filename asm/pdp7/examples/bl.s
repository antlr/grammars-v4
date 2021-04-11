" bl

jmp start

.array: .+1
   s 2
   n 8
   n 7
   -1
   tad sp i
   cma
   tad lastv
   dac lastv
   lmq
   lac sp
   tad d1
   dac sp i
   isz sp
   lacq
   dac sp i
   isz sp
   jmp fetch

.read: .+1
   s 2
   n 8
   n 7
   lac sp
   tad d1
   dac sp i
   isz sp
   jms getc
   dac sp i
   isz sp
   jmp fetch

.write: .+1
   s 2
   n 8
   n 7
   lac sp i
   dac t1
   lrss 9
   jms putc
   lac t1
   jms putc
   jmp fetch

.flush: .+1
   n 8
   n 7
   jms flush
   jmp fetch

getc: 0
   lac iflg
   dzm iflg
   sza
   jmp getc i
   lac cibufp
   sad eibufp
   jmp 1f
   lac cibufp i
   and o777
   dac iflg
   lac cibufp i
   isz cibufp
   lrss 9
   jmp getc i
1:
   lac .fin
   sys read; ibufp: ..; 64
   sna spa
   jmp 1f
   tad ibufp
   dac eibufp
   lac ibufp
   dac cibufp
   jmp getc+1
1:
   lac o4
   jmp getc i

putc: 0
   and o777
   sna
   jmp putc i
   lmq
   lac oflg
   sza
   jmp 1f
   lacq
   alss 9
   dac cobufp i
   dac oflg
   jmp putc i
1:
   lac cobufp i
   omq
   dac cobufp i
   dac cobufp i
   dzm oflg
   isz cobufp
   lac cobufp
   sad eobufp
   skp
   jmp putc i
   lac .fout
   sys write; obufp: ..; 64
   lac obufp
   dac cobufp
   jmp putc i

stop:
   jms flush
   las
   sma
   sys exit     " XXX replaced for now:   sys save
   sys exit

flush: 0
   lac oflg
   sza
   isz cobufp
   lac cobufp
   cma
   tad obufp
   cma
   sna
   jmp flush i
   dac 1f+1
   lac obufp
   dac 1f
   lac .fout
   sys write; 1: ..; ..
   lac obufp
   dac cobufp
   dzm oflg
   jmp flush i

initio: 0
   lac 017777 i
   sad d4
   jmp 2f
   sad d8
   jmp 1f

   law 9
   tad 017777
   dac .+3
   law 017
   sys creat; ..
   spa
   jmp stop
   dac .fout
1:
   law 5
   tad 017777
   dac .+2
   sys open; ..; 0
   spa
   jmp stop
   dac .fin
2:
   lac lastv
   dac eibufp
   dac cibufp
   -64
   tad lastv
   dac lastv
   dac ibufp
   lac lastv
   dac eobufp
   -64
   tad lastv
   dac lastv
   dac obufp
   dac cobufp
   dzm oflg
   jmp initio i

.fin: 0
.fout: 1
eibufp: 0
cibufp: 0
iflg: 0
eobufp: 0
cobufp: 0
oflg: 0
lastv: 017770

o777: 0777
d4:o4: 4
d8: 8
