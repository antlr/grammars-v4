"** 01-s1.pdf page 48

" s8

" manifests
mnproc = 10
dspbsz = 270
ndskbs = 4

" flags
	" interupt flags
.insys: 0			" "in system"
.int1: 0			" inode for interrupt 1
.int2: 0			" inode for interrupt 2
.ac: 0				" saved AC from interrupt
.savblk: 0			" set by system call, cleared by disk i/o
.dsptm: 0			" display restart countdown (10 ticks)
.dskb: 0			" set on disk interrupt
.dske: 0			" status from disk interrupt

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
   caf				" clear all flags
   ion				" enable interrupts
   clon				" clear clock flag
   law 3072			" initialize display....
   wcga
   jms dspinit
   law dspbuf
   jms movdsp
   cla				" read system block from disk
   jms dskio; 06000
   jms copy; dskbuf; sysdata; ulist-sysdata	" copy to system data
   lac d3			" look for "init" in default directory
   jms namei; initf
      jms halt
"** 01-s1.pdf page 50
   jms iget
   cla
   jms iread; 4096; 4096	" read in "init"
   jmp 4096			" start process 1
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
uniqpid: 1			" pid generator
lu: .=.+4			" user (process) table entry copy
sfiles: .=.+10			" wait addresses for special files
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
sysdata:			" system data 64 words saved to disk
   s.nxfblk: .=.+1		" pointer to next free block??
   s.nfblks: .=.+1		" number of free blocks (in fblks?)
   s.fblks: .=.+10		" cached free block numbers
   s.uniq: .=.+1		" next unique value
   s.tim: .=.+2			" (up?)time in 60Hz ticks (low, high)
	" process table
	" first word
	"   bits 0:2 -- status
	"	0: free slot
	"	1: in/ready
	"	2: in/notready
	"	3: out/ready
	"	4: out/notready??
	"   bits 3:17 -- disk swap address/8
	" second word: process pid
	" third word:  used for smes/rmes
	" fourth word: ??
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
userdata:			" "ustruct" (swappable)
   u.ac: 0			" user AC
   u.mq: 0			" user MQ
   u.rq: .=.+9			" user 010-017, user PC
   u.uid: -1			" user id
   u.pid: 1			" process id
   u.cdir: 3			" connected directory (inode number?)
   u.ulistp: ulist		" pointer to process table entry
   u.swapret: 0			" kernel routine to resume at after swap in
   u.base: 0			" start of user buffer
   u.count: 0			" size of user buffer
"** 01-s1.pdf page 51
   u.limit: 0			" end of user buffer
   u.ofiles: .=.+30		" open files (10 "fnode" entries)
   u.dspbuf: 0
   u.intflg: 1
      .=userdata+64
ii: .=.+1			" number of i-node in inode:
inode:				" disk inode in memory:
   i.flags: .=.+1		" inode flags
				" 400000 free?? (checked/toggled by icreat)
				" 200000 large file
				" 000040 special device (indicated by inum)?
				" 000020  directory
				" 000010 owner read
				" 000004 owner write
				" 000002 world read
				" 000001 world write
   i.dskps: .=.+7		" disk block pointers (indirect if "large file")
   i.uid: .=.+1			" owner
   i.nlks: .=.+1		" link count
   i.size: .=.+1
   i.uniq: .=.+1		" unique number
      .= inode+12
di: .=.+1
dnode:				" directory entry:
   d.i: .=.+1			" inode number
   d.name: .=.+4		" name (space padded)
   d.uniq: .=.+1		" unique number from directory inode
      . = dnode+8
fnode:				" open file entry
   f.flags: .=.+1		" see below
   f.badd: .=.+1		" offset
   f.i: 0			" file i-number
"	f.flags:
"		400000	in use
"		000002	read
"		000001	write
