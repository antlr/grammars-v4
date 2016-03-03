"** 01-s1.pdf page 48

" s8

" manifests
mnproc = 10
dspbsz = 270
ndskbs = 4

" flags
.insys: 0
.int1: 0
.int2: 0
.ac: 0
.savblk: 0
.dsptm: 0
.dskb: 0
.dske: 0

" pointers
tadu: tad ulist
dacu: dac ulist
maxquant: 30
ofilesp: u.ofiles
idskpp: i.dskps
dskbufp: dskbuf
edspbuf: dspbuf+dspbsz
dspbufp3: dspbuf+3
fblksp: s.fblks
dacq1: dac q1
lacq1: lac q1
q2p: q2

" strings
initf:
   <i>n;<i>t;< > ;< > "

" constants
d0: 0
d1: 1
d2: 2
d3: 3
d4: 4
d5: 5
d6: 6
d7: o7: 07
d8: 8
d9: 9
o12: d10: 10
o14: 014
o15: 015
o17: 017
o20: 020
o33: 033
o40: 040
o55: 055
o77: 077
d65:o101: 0101
d33: 33
o132: 0132
o134: 0134
o137: 0137
o155: 0155
o177: 0177
"** 01-s1.pdf page 49
o212: 0212
o375: 0375
o777: 0777
o2000: 02000
o4000: 04000
d7999: 7999
o10000: 010000
o17762: 017762
o17777: 017777
o20001: 020001
o40000: 040000
o40001: 040001
o70000: 070000
o77777: 077777
o100000: 0100000
o140000: 0140000
o200000: 0200000
o200001: 0200001
o300000: 0300000
o400000: 0400000
o500000: 0500000
o577777: 0577777
o600000: 0600000
o640000: 0640000
o700000: 0700000
o777700: 0777700
o777760: 0777760
dm3: -3
dm1: -1

9: .=.+t
c1: .=.+1
q1: q2;q2+90  "** ?? 98 ??
   .=.+14
q2:
   .+2;0;.+2;0;.+2;0;.+2;0;.+2;0;.+2;0;.+2;0;.+2;0;.+2;0;.+2;0
   .+2;0;.+2;0;.+2;0;.+2;0;.+2;0;.+2;0;.+2;0;.+2;0;.+2;0;.+2;0
   .+2;0;.+2;0;.+2;0;.+2;0;.+2;0;.+2;0;.+2;0;.+2;0;.+2;0;.+2;0
   .+2;0;.+2;0;.+2;0;.+2;0;.+2;0;.+2;0;.+2;0;.+2;0;.+2;0;.+2;0
   .+2;0;.+2;0;.+2;0;.+2;0;.+2;0;.+2;0;.+2;0;.+2;0;.+2;0;0;0
dsploc: .=.+1
dsplno: .=.+1
dspbuf:
   0065057;0147740;0160000
   .=.+30
coldentry:
   dzm 0100 " not re-entrant
   caf
   ion
   clon
   law 3072
   wcga
   jms dspinit
   law dspbuf
   jms movdsp
   cla
   jms dskio; 06000
   jms copy; dskbuf; sysdata; ulist-sysdata
   lac d3
   jms namei; initf
      jms halt
"** 01-s1.pdf page 50
   jms iget
   cla
   jms iread; 4096; 4096
   jmp 4096
   . = dspbuf+dspbsz+3
dskbuf = 07700
dskbs: .=.+65+65+65+65
edskbsp: .
uquant: .=.+1
dspbufp: .=.+1
pbsflgs: .=.+2
mode: .=.+1
nttychar: .=.+1
npptchar: .=.+1
ttydelay: .=.+1
name: .=.+4
lnkaddr: .=.+1
char: .=.+1
dskaddr: .=.+1
uniqpid: 1
lu: .=.+4
sfiles: .=.+10
dpdata:
   dpstat: .=.+1
   dpread: .=.+1
   dpwrite: .=.+1
   dpchar: .=.+1
dspdata:
   .dspb: .=.+1
   .lpba: .=.+1	"** 4 written on listing
crdata:
   crread: .=.+1
   crchar: .=.+1
sysdata:
   s.nxfblk: .=.+1
   s.nfblks: .=.+1
   s.fblks: .=.+10
   s.uniq: .=.+1
   s.tim: .=.+2
ulist:
   0131000;1;0;0
   0031040;0;0;0
   0031100;0;0;0
   0031140;0;0;0
   0031200;0;0;0
   0031240;0;0;0
   0031300;0;0;0
   0031340;0;0;0
   0031400;0;0;0
   0031440;0;0;0
userdata:
   u.ac: 0
   u.mq: 0
   u.rq: .=.+9
   u.uid: -1
   u.pid: 1
   u.cdir: 3
   u.ulistp: ulist
   u.swapret: 0
   u.base: 0
   u.count: 0
"** 01-s1.pdf page 51
   u.limit: 0
   u.ofiles: .=.+30
   u.dspbuf: 0
   u.intflg: 1
      .=userdata+64
ii: .=.+1
inode:
   i.flags: .=.+1
   i.dskps: .=.+7
   i.uid: .=.+1
   i.nlks: .=.+1
   i.size: .=.+1
   i.uniq: .=.+1
      .= inode+12
di: .=.+1
dnode:
   d.i: .=.+1
   d.name: .=.+4
   d.uniq: .=.+1
      . = dnode+8
fnode:
   f.flags: .=.+1
   f.badd: .=.+1
   f.i: 0

