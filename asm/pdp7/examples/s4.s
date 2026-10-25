"** 01-s1.pdf page 21
" s4

	" allocate a free disk block for a file (data or indirect)
alloc: 0
   -1			" decrement the count
   tad s.nfblks		" of free blocks at s.fblks
   spa			" any left?
   jmp 1f		"  no
   dac s.nfblks		" Update the count of free block numbers
   tad fblksp		" get pointer to last valid entry in s.fblks
   jms laci		" fetch the word
   dac 9f+t		" save in t0
   jms copyz; dskbuf; 64	" zero dskbuf
   lac 9f+t		" get block number back
   jms dskwr		" write out the zeros
   dzm .savblk		" cancel system block write
   lac 9f+t		" get the block number back
   jmp alloc i		" Return from routine
1:
   lac s.nxfblk		" next block with list of free blocks
   sna			" any?
   jms halt " OUT OF DISK
   dac s.fblks		" save as first free block #
   jms dskrd		" read the block
   lac dskbuf		" get first word (pointer to next in chain)
   dac s.nxfblk		" save as new "next"
   jms copy; dskbuf+1; s.fblks+1; 9	" copy remaining 9 as free
   lac d10		" reset free count
   dac s.nfblks
   jmp alloc+1

	" free the disk block whose number is in AC
free: 0
   lmq			" save block in MQ
   lac s.nfblks		" get number of free blocks
   sad d10		" 10?
   jmp 1f		"  yes
   tad fblksp		" no: get addr in s.fblks to store new block in
   dac 9f+t		" save pointer
   lacq			" get block number
   dac 9f+t i		" save it in s.fblks
   dzm .savblk		" cancel system block write
   isz s.nfblks		" increment free count
   jmp free i		" return
1:
   lac s.nxfblk		" get head of free chain
   dac dskbuf		" save (as chain ptr) in first word of disk buf
   jms copy; s.fblks+1; dskbuf+1; 9	" with 9 of the 10 free blocks
   lacq			" get the newly freed block back
   dac s.nxfblk		" save as new head of free chain
   jms dskwr		" write dskbuf to the newly freed block
   dzm .savblk		" cancel system block write
   lac d1		" reset free count to one
   dac s.nfblks		" (the first word in s.fblks)
   jmp free i		" Return from the routine
t = t+1

	" load AC indirect (without using indirect!)
	" need to avoid use of indirect in interrupt service routines
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

	" Character queue management routines
	" (CALLED FROM PI: USE OF INDIRECT AVOIDED!)

	" Queue numbers:
	"  0: free list
	"  1: tty input
	"  2: tty output
	"  3: display keyboard
	"  4: paper tape reader
	"  5: paper tape punch

	" queue headers are two words: "first" and "last"(??)
	" queue entries are two words: "next" and "char"

	" put queued character
	" character to store in "char"
	" queue number in AC

putchar: 0
"** 01-s1.pdf page 23
   dac 9f+t		" save queue number in t0
   cla			" get entry from free list
   jms takeq
      jmp putchar i	" none free: return w/o skip
   tad o40001		" turn into "dac addr+1"
   dac .+4
   lac 9f+t		" get queue number back
   jms putq		" add entry
   lac char		" get char
   dac q2+1 ..		" save in second word of queue entry
   isz putchar		" give skip return
   jmp putchar i
t = t+1

	" get queued character
	" queue number in AC
	" returns with skip if something found
getchar: 0
   jms takeq		" get entry from head of queue
      jmp i getchar	"  nothing there: return w/o skip
   tad o200001		" turn into "lac qentry+1" (fetch stored char)
   dac .+3
   cla
   jms putq		" put qentry on free list
   lac q2+1 ..		" fetch queued character(!!!)
   isz getchar		" give skip return
   jmp i getchar

	" fetch first entry from a queue
	" queue number in AC
	" returns with skip if something dequeued
takeq: 0
   rcl			" multiply queue number by two
   tad lacq1		" add "lac q1"
   dac .+7
   tad o640000		" turn "lac" into "dac"
   dac .+17
   tad d1		" increment addr
   dac .+14
   tad o500000		" turn "dac" into "sad"
   dac .+5
   lac q1 ..		" load queue head
   sna			" non-zero?
   jmp takeq i		"  no: return zero w/o skip
   dac lnkaddr
   sad q1+1 ..		" different from tail(??)
   jmp .+5		"  no -- (last entry?)
   tad o200000		" yes: turn into lac
   dac .+1		" save lac
   lac q2 ..		" get next pointer from queue entry
   jmp .+3
   cla			" here with head == tail(??)
   dac q1+1 ..		" clear tail
   dac q1 ..		" save (clear) head
   isz takeq		" give skip return
   lac lnkaddr		" return queue entry pointer
   jmp i takeq

	" save queue entry (at lnkaddr) to a queue (queue number in AC)
putq: 0
   rcl			" multiply by two
   tad dacq1		" turn into "dac qhead"
   dac .+14
   tad d1		" turn into "dac qtail"
   dac .+13
   tad o140000		" turn into "lac qtail"
   dac .+1
   lac q1-1 ..		" fetch tail
"** 01-s1.pdf page 24
   sna			" non-zero?
   jmp .+6		"  no: is zero
   tad o40000		" turn into "dac qentry" (append to queue)
   dac .+2
   lac lnkaddr		" get new entry
   dac q2 ..		" append to tail entry
   jmp .+3
   lac lnkaddr		" here with tail == 0
   dac q1 ..		" save new entry as head
   dac q1+1 ..		" save new tail
   jmp putq i

	" NOTE!! srcdbs, collaps, dskrd, dskwr share the same "temp" vars:
	" "t0" temp!!
	" "t1" contains pointer to (addr, buffer)
	" "t2" contains block number
	
	" check if disk block number in AC in memory
	" give skip return if block NOT found

srcdbs: 0
   dac 9f+t+2   "* lmq			" save block number in t2
   -ndskbs				" loop for number of buffers
   dac 9f+t				" in t0
   law dskbs	"* -1 dac 8 written	" get address of first buffer
   dac 9f+t+1	"* lacq			" in t1
1:
   lac 9f+t+2	"** crossed out		" get desired block number
   sad 9f+t+1 i	"** "8 i" written	" match buffer block?
   jmp srcdbs i				"  yes: return without skip
   law 65	"** crossed out		" no: advance to next buffer
   tad 9f+t+1	"** crossed out isz 8 written
   dac 9f+t+1
   isz 9f+t
   jmp 1b
   isz srcdbs				" block not found: give skip return
   jmp srcdbs i

collapse: 0
   cla					" look for free buffer
   jms srcdbs				" found?
      jmp 1f				"  yes
   law dskbs				" no: reuse last buffer
   dac 9f+t+1	"** 9f+t+1 crossed out: 8 written in
1:
		"** written: tad dm1
		"** written: dac 8
   lac 9f+t+1	"** 9f+t+1 crossed out: 8 written in
   dac 0f+1				" save as copy dest
   tad d65	"** crossed out w/ d2	" get start of next buffer
   dac 0f				" save as copy src
   cma
   tad d1
   tad edskbsp				" subtract from end of buffers
   and o17777				" mask to 13 bits
   sna					" non-zero count?
   jmp 0f+3				"  no: skip copy
   dac 0f+2				" save as copy length
   jms copy; 0:..; ..; ..		" slide buffers up
   -65
   tad edskbsp				" get addr of last buffer
   dac 9f+t				" save in t0
   tad d1				" get block data pointer
   dac 0f				" save as copy dest
   lac dskaddr				" get block number
   dac 9f+t i				" save in buffer header
   jms copy; dskbuf; 0:..; 64		" copy dskbuf to last buffer
   jmp collapse i

	" read logical disk block number (2..7999) in AC into dskbuf
dskrd: 0
   jms betwen; d2; d7999

"** 01-s1.pdf page 25
      jms halt			" bad block number
   sad dskaddr			" block currently in dskbuf
   jmp dskrd i			"  yes: return
   dac dskaddr			" save block address
   jms srcdbs			" in memory?
      jmp 1f			"  yes
   lac dskaddr			" no: read from disk
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

	" called to read/write logical block into "dskbuf"
	" AC/ block
	"   jms dskio; DSLD_BITS
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

	" perform disk I/O (both filesystem buffer and swapping)
	" passed physical (BCD) disk address
	" called:
	"   jms dsktrans; -WC; MAC; addr_ptr; dsld_ptr
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

