" brt

jmp start

" syscalls:

" .save: .+1
" .getuid: .+1

.open: .+1
   s 2
   n 8
   n 7
   lac sp i
   dac 1f
   isz sp
   lac sp i
   dac 2f
   lac sp
   tad d1
   dac sp i
   isz sp
   sys open; 1:0; 2:0
   dac sp i
   isz sp
   jmp fetch

.read: .+1
   s 2
   n 8
   n 7
   lac sp i
   dac t1
   isz sp
   lac sp i
   dac 1f
   isz sp
   lac sp i
   dac 2f
   lac sp
   tad d1
   dac sp i
   isz sp
   lac t1
   sys read; 1:0; 2:0
   dac sp i
   isz sp
   jmp fetch

.write: .+1
   s 2
   n 8
   n 7
   lac sp i
   dac t1
   isz sp
   lac sp i
   dac 1f
   isz sp
   lac sp i
   dac 2f
   lac sp
   tad d1
   dac sp i
   isz sp
   lac t1
   sys write; 1:0; 2:0
   dac sp i
   isz sp
   jmp fetch

" .creat: .+1
" .seek: .+1
" .tell: .+1
" .close: .+1
" .link: .+1
" .unlink: .+1
" .setuid: .+1
" .rename: .+1
" .exit: .+1
" .time: .+1
" .intrp: .+1
" .chdir: .+1
" .chmod: .+1
" .chown: .+1
" .sysloc: .+1
" .capt: .+1
" .rele: .+1
" .status: .+1
" .smes: .+1
" .rmes: .+1
" .fork: .+1

" library

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

.getchar: .+1
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

.putchar: .+1
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
 sys exit   "   sys save
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
   law .argv      " build argument list
   dac 8          " auto-inc pointer argvp = &argv[-1]
   lac 017777 i
   cll rtr
   dac 8 i        " *++argvp = arg count
   cma
   tad d1
   dac t1         " count = -argv[0]
   lac 017777
   dac lastv      " set top of heap
   tad d1         " ac = addr of arg0
1: 
   dac 8 i        " *++argvp = ac
   tad d4         " ac += 4
   isz t1         " while ++count != 0
   jmp 1b

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

.argv: .+1
.=.+10
.fin: 0
.fout: 1
eibufp: 0
cibufp: 0
iflg: 0
eobufp: 0
cobufp: 0
oflg: 0
lastv: 0

o777: 0777
d4:o4: 4
d8: 8