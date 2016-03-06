"** 01-s1.pdf page 21
" s4

	" allocate a free disk block for a file (data or indirect)
alloc: 0
   -1
   tad s.nfblks
   spa
   jmp 1f
   dac s.nfblks
   tad fblksp
   jms laci
   dac 9f+t
   jms copyz; dskbuf; 64
   lac 9f+t
   jms dskwr
   dzm .savblk
   lac 9f+t
   jmp alloc i
1:
   lac s.nxfblk
   sna
   jms halt " OUT OF DISK
   dac s.fblks
   jms dskrd
   lac dskbuf
   dac s.nxfblk
   jms copy; dskbuf+1; s.fblks+1; 9
   lac d10
   dac s.nfblks
   jmp alloc+1

	" free a disk block
free: 0
   lmq
   lac s.nfblks
   sad d10
   jmp 1f
   tad fblksp
   dac 9f+t
   lacq
   dac 9f+t i
   dzm .savblk
   isz s.nfblks
   jmp free i
1:
   lac s.nxfblk
   dac dskbuf
   jms copy; s.fblks+1; dskbuf+1; 9
   lacq
   dac s.nxfblk
   jms dskwr
   dzm .savblk
   lac d1
   dac s.nfblks
   jmp free i
t = t+1

	" load AC indirect (without using indirect!)
	" AC/ address
	"   jms laci
	" AC/ contents of address
laci: 0
   and o17777				" clear everything but addr
   tad o200000				" make into "lac addr"
   dac .+1
   lac ..				" fetch
   jmp laci i				" return

"** 01-s1.pdf page 22

	" skip if AC between two values (inclusive)
	"   jms betwen; low_ptr; high_ptr
	"    <not between>
	"  <between>
	" listing has an alternate written in
	"  (which would require 'lac ptr' instead of 'ptr' args?)
betwen: 0
   lmq cmq			" get ~AC in MQ
   lac betwen i			" get low_ptr
   dac 9f+t
   isz betwen			" skip low_ptr
   lacq				" get ~AC (-AC-1) from MQ
   tad 9f+t i			" get low-AC-1
   sma				" negative (AC >= low)?
   jmp 1f			"  no, return w/o skip
   lac betwen i			" fetch high_ptr
   dac 9f+t
   isz betwen			" skip high_ptr
   lacq				" get -AC-1
   tad 9f+t i			" add to high value (high-AC-1)
   cma				" complement (AC-high)
   spa sna			" AC-high <= 0?
1:
   isz betwen			"  no: give happy (skip) return
   lacq				" restore ~AC
   cma				" restore AC
   jmp betwen i			" return w/o skip

	" copy memory
	" call:
	"   jms copy; src; dest; count
copy: 0
   -1
   tad copy i
   dac 8
   isz copy
   -1
   tad copy i
   dac 9
   isz copy
   -1
   tad copy i
   cma
   dac 9f+t
   isz copy
1:
   lac 8 i
   dac 9 i
   isz 9f+t
   jmp 1b
   jmp copy i

	" copy zeroes (clear memory)
	" call:
	"   jms copyz; pointer; count
copyz: 0
   -1
   tad copyz i			" get call PC
   dac 8			" save in index (pre-increments)
   isz copyz			" skip pointer
   -1
   tad copyz i			" get count-1
   cma				" get negative count
   dac 9f+t			" save in t0
   isz copyz			" skip count
1:
   dzm 8 i			" zero word
   isz 9f+t			" done?
   jmp 1b			"  no: loop
   jmp copyz i			" return
t = t+1

putchar: 0
"** 01-s1.pdf page 23
   dac 9f+t
   cla
   jms takeq
      jmp putchar i
   tad o40001
   dac .+4
   lac 9f+t
   jms putq
   lac char
   dac q2+1 ..
   isz putchar
   jmp putchar i
t = t+1

getchar: 0
   jms takeq
      jmp i getchar
   tad o200001
   dac .+3
   cla
   jms putq
   lac q2+1 ..
   isz getchar
   jmp i getchar

takeq: 0
   rcl
   tad lacq1
   dac .+7
   tad o640000
   dac .+17
   tad d1
   dac .+14
   tad o500000
   dac .+5
   lac q1 ..
   sna
   jmp takeq i
   dac lnkaddr
   sad q1+1 ..
   jmp .+5
   tad o200000
   dac .+1
   lac q2 ..
   jmp .+3
   cla
   dac q1+1 ..
   dac q1 ..
   isz takeq
   lac lnkaddr
   jmp i takeq

putq: 0
   rcl
   tad dacq1
   dac .+14
   tad d1
   dac .+13
   tad o140000
   dac .+1
   lac q1-1 ..
"** 01-s1.pdf page 24
   sna
   jmp .+6
   tad o40000
   dac .+2
   lac lnkaddr
   dac q2 ..
   jmp .+3
   lac lnkaddr
   dac q1 ..
   dac q1+1 ..
   jmp putq i

srcdbs: 0
   dac 9f+t+2   "* lmq
   -ndskbs
   dac 9f+t
   law dskbs	"* -1 dac 8 written
   dac 9f+t+1	"* lacq
1:
   lac 9f+t+2	"** crossed out
   sad 9f+t+1	"** isz 8 written
   jmp srcdbs i
   law 65	"** ??? crossed out
   tad 9f+t+1	"** crossed out isz 8 written
   isz 9f+t+1
   isz 9f+t
   jmp 1b
   isz srcdbs
   jmp srcdbs i

collapse: 0
   cla
   jms srcdbs
      jmp 1f
   law dskbs
   dac 9f+t+1	"** 9f+t+1 crossed out: 8 written in
1:
   lac 9f+t+1	"** 9f+t+1 crossed out: 8 written in
   dac 0f+1
   tad d65	"** crossed out: d2-- original obscured
   dac 0f
   cma
   tad d1
   tad edskbsp
   and o17777
   sna
   jmp 0f+3
   dac 0f+2
   jms copy; 0:..; ..; ..
   -65
   tad edskbsp
   dac 9f+t
   tad d1
   dac 0f
   lac dskaddr
   dac 9f+t i
   jms copy; dskbuf; 0:..; 64
   jmp collapse i

dskrd: 0
   jms betwen; d2; d7999

"** 01-s1.pdf page 25
      jms halt
   sad dskaddr
   jmp dskrd i
   dac dskaddr
   jms srcdbs
      jmp 1f
   lac dskaddr
   jms dskio; 06000
   jmp 2f
1:
   dzm 9f+t+1 i
   law 1
   tad 9f+t+1
   dac .+2
   jms copy; ..; dskbuf; 64
2:
   jms collapse
   jmp dskrd i

	" write a file block (data, inode or indirect)
	" AC/ block
dskwr: 0
   jms betwen; d2; d7999
      jms halt
   jms dskio; 07000
   lac dskaddr
   jms srcdbs
      dzm 9f+t+1 i
   jms collapse
   jmp dskwr i
t = t+3

	" AC/ block
	"   jms dskio; dsld_bits
dskio: 0
   dac dskaddr
   cll; idiv; 80
   dac 9f+t
   lacq
   idiv; 10
   dac 9f+t+1
   lls 22
   xor 9f+t+1
   als 8
   dac 9f+t+1
   lac 9f+t
   idiv; 10
   dac 9f+t
   lls 22
   xor 9f+t
   xor 9f+t+1
   xor o200000
   dac 9f+t
   jms dsktrans; -64; dskbuf; 9f+t; dskio
   isz dskio
   jmp dskio i
t = t+1

	" called with:
	"   jms dsktrans; -WC; MAC; addr_ptr?; dsld_ptr
dsktrans: 0
   -10
   dac 9f+t
1:
   -1
   tad dsktrans
   dac 12
"** 01-s1.pdf page 26
   dscs				" clear status register
   lac 12 i
   dslw				" load WC
   lac 12 i
   dslm				" load MAC
   lac 12 i
   jms laci
   dsld				" load TA & SA
   dzm .dskb
   lac 12 i
   jms laci
   jms laci
   dsls				" load status
   lac .dskb			" check for interrupt
   sna
   jmp .-2
   lac .dske			" get status from interrupt
   sma
   jmp 12 i			" return
   isz 9f+t
   jmp 1b
   jms halt " 10 disk errors
t = t+1

halt: 0
   isz 9f+t			" spin for a while (process interrupts)
   jmp .-1
   iof				" disable interrupts
   hlt				" halt
   jms copy; law; 4096; 4096	" continued: copy system up to user memory?
   hlt; jmp .-1			" halt for good
t = t+1

