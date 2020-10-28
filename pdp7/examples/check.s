" check

lac d1
sys sysloc
dac iget

lac d2
sys sysloc
dac inode

lac d4
sys sysloc
dac nxfblk
tad d1
dac nfblks
tad d1
dac fblks

lac d5
sys sysloc
dac copy

lac d6
sys sysloc
dac copyz

lac d7
sys sysloc
dac betwen

lac d8
sys sysloc
dac dskrd

lac d10
sys sysloc
dac dskbuf
dac dskbuf1

   dzm indircnt		" Zero the counts for indirect and others
   dzm icnt
   dzm licnt
   dzm blcnt
   dzm curi
   jms copyz i; usetab; 500

iloop:
   isz curi
   -3400
   tad curi
   sma
   jmp part2
   lac curi
   jms iget i
   jms copy i; inode: 0; linode; 12
   lac iflags
   sma
   jmp iloop
   isz icnt
   lac iflags
   and o40
   sza
   jmp iloop
   law idskps
   dac t1
   -7
   dac t2
1:
   lac i t1
   sza
   jms dupcheck
   isz t1
   isz t2
   jmp 1b
   lac iflags
   and o200000
   sna
   jmp iloop

   isz licnt
   law idskps
   dac t1
   -7
   dac t2
1:
   lac i t1
   sna
   jmp 3f
   jms dskrd i
   jms copy i; dskbuf: 0; ldskbuf; 64
   isz indircnt
   law ldskbuf
   dac t3
   -64
   dac t4
2:
   lac i t3
   sza
   jms dupcheck
   isz t3
   isz t4
   jmp 2b
3:
   isz t1
   isz t2
   jmp 1b
   jmp iloop

dupcheck: 0
   isz blcnt
   jms betwen i d709; d6400
   jmp badadr
   dac t5
   lrss 4
   tad usetabp
   dac t6
   cla
   llss 4
   tad alsscom
   dac 2f
   lac d1
2: alss 0
   dac bit
   lac i t6
   and bit
   sza
   jmp dup
   lac i t6
   xor bit
   dac i t6
   xor bit
   dac i t6
   jmp i dupcheck

badadr:
   jms print
   lac d1
   sys write; badmes; 3
   jmp i dupcheck
badmes:
   < b>;<ad>;<r 012

dup:
   lac t5
   jms print
   lac d1
   sys write; dupmes; 3
   lac curi
   jms print
   lac d1
   sys write; dupmes+3; 1
   jmp i dupcheck
dupmes:
   < d>;<up>; 040; 012

print: 0
   lmq
   law prbuf-1
   dac 8
   -6
   dac t6
1:
   cla
   llss 3
   tad o60
   dac i 8
   isz t6
   jmp 1b
   lac d1
   sys write; prbuf; 6
   jmp i print

part2:
   lac icnt
   jmp print
   lac d1
   sys write; m3; m3s
   lac licnt
   jms print
   lac d1
   sys write; m4; m4s
   lac indircnt
   jms print
   dac d1
   sys write; m5; m5s
   lac blcnt
   jms print
   lac d1
   sys write; m6; m6s
   dzm blcnt

   -1
   tad nfblks i
   cma
   sma
   jmp 2f
   dac t1
   lac fblks
   dac t2
1:
   lac i t2
   jms dupcheck
   isz t2
   isz t1
   jmp 1b

2:
   lac nxfblk i
1:
   sna
   jmp part3
   dac t1
   jms dupcheck
   lac t1
   jms dskrd i
   jms copy i; dskbuf1: 0; ldskbuf; 64
   law ldskbuf
   dac t1
   -9
   dac t2
2:
   isz t1
   lac i t1
   jms dupcheck
   isz t2
   jmp 2b
   lac ldskbuf
   jmp 1b

part3:
   lac blcnt
   jms print
   lac d1
   sys write; m7; m7s
   lac d709
   dac t1
1:
   isz t1
   lac t1
   sad d6400
   sys exit
   lrss 4
   tad usetabp
   dac t2
   cla
   llss 4
   tad alsscom
   dac 2f
   lac d1
2: alss 0
   dac bit
   lac i t2
   and bit
   sza
   jmp 1b
   lac t1
   jms print
   lac d1
   sys write; m8; m8s
   jmp 1b

d1: 1
d2: 2
d4: 4
d5: 5
d6: 6
d7: 7
d8: 8
d10: 10
o60: 060
o400000: 0400000
o400001: 0400001
o40: 040
o200000: 0200000
alsscom: alss 0
d709: 709
d6400: 6400

m3:
   040;<fi>;<le>;<s 012
m3s = .-m3
m4:
   040;<la>;<rg>;<e 012
m4s = .-m4
m5:
   040;<in>;<di>;<r 012
m5s = .-m5
m6:
   040;<us>;<ed>;012
m6s = .-m6
m7:
   040;<fr>;<ee>;012
m7s = .-m7
m8:
   040;<mi>;<ss>;<in>;<g 012
m8s = .-m8

usetabp: usetab
curi: 0
bit: 0
blcnt: 0
indircnt: 0
icnt: 0
licnt: 0
t1: 0
t2: 0
t3: 0
t4: 0
t5: 0
t6: 0

iget: 0
nxfblk: 0
nfblks: 0
fblks: 0
copy: 0
copyz: 0
betwen: 0
dskrd: 0

ldskbuf: .=.+64
linode: .=.+12
iflags = linode
idskps = iflags+1
usetab: .=.+500
prbuf: .=.+6
