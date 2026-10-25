"** 01-s1.pdf page 14
" s3

	" search for user (process) table entry
	" call:
	"   jms searchu; worker_routine_addr
	" worker called with copy of a process table entry in "lu"
	"	can return directly (from caller of searchu)
	"	index location 8 points to next process table entry
searchu: 0
   lac searchu i		" fetch argument
   dac 9f+t+1			" in t1
   -mnproc			" loop counter
   dac 9f+t			" in t0
   law ulist-1			" ulist ptr
   dac 8			" in index 8
1:
   lac 8 i			" copy ulist entry to lu
   dac lu
   lac 8 i
   dac lu+1
   lac 8 i
   dac lu+2
   lac 8 i
   dac lu+3
   jms 9f+t+1 i			" call argument as subroutine
   isz 9f+t			" returned: loop done?
   jmp 1b			"  no, do it again
   isz searchu			" skip argument
   jmp searchu i
t = t+2

	" look for a process with matching status
	"   jms lookfor; status
	"    found: ulist ptr in AC
	"   not found
lookfor: 0
   jms searchu; 1f
   isz lookfor			" skip argument
   isz lookfor			" give skip return
   jmp lookfor i
1: 0				" worker called by searchu
   lac lu
   rtl; rtl; and o7		" bits 0:2 of lu
   sad lookfor i		" match argument?
   skp				"  yes
   jmp 1b i			"   no, return, keep going
   -3
   tad 8			" roll index 8 back to this entry
   and o17777
   isz lookfor			" skip lookfor argument
   jmp lookfor i		" non-skip return

	" fork system call:
	"   sys fork
	"    return at +1 in parent, child pid in AC
	"   return at +2 in child, parent pid in AC
.fork:
   jms lookfor; 0 " not-used	" find an unused process slot
      skp
      jms error			" none found- return error
   dac 9f+t			" save ulist ptr in t0
   isz uniqpid			" generate new pid
   lac uniqpid
   dac u.ac			" return in child pid in AC
   law sysexit
   dac u.swapret		" return from system call when swapped back in
   lac o200000			" change process status to out/ready (1->3)
   tad u.ulistp i
   dac u.ulistp i
   jms dskswap; 07000		" swap parent out
   lac 9f+t			" get unused ulist slot back
   dac u.ulistp			" set ulist pointer
   lac o100000			" mark child in/notready? (3->2)
   xor u.ulistp i
   dac u.ulistp i
   lac u.pid			" get old (parent) pid
"** 01-s1.pdf page 15
   dac u.ac			" return parent pid in AC
   lac uniqpid
   dac u.pid			" set child pid
   isz 9f+t			" advance to second word in process table
   dac 9f+t i			" set pid in process table
   isz u.rq+8			" give skip return
   dzm u.intflg			" clear int flag
   jmp sysexit			" return in child process
t= t+1

badcal:				" bad (unimplemented) system call
   clon				" clear any pending clock interrupt?
   -1
   dac 7			" set location 7 to -1?!
	" fall into "save" system call
	" Ken says save files could be resumed, and used for checkpointing!
.save:				" "sys save" system call
   lac d1			" get inode 1 (core file?)
   jms iget
   cla
   jms iwrite; 4096; 4096	" dump core
   jms iwrite; userdata; 64	" and user area
   jms iput

.exit:
   lac u.dspbuf
   sna				" process using display?
   jmp .+3			"  no
   law dspbuf			"   yes: get default display buffer
   jms movdsp			"   move display
   jms awake
   lac u.ulistp i
   and o77777			" mark process table entry free
   dac u.ulistp i
   isz u.ulistp
   dzm u.ulistp i		" clear pid in process table
   jms swap			" find a new process to run
	" exit falls into "rmes" !!!

	" rmes system call
	"   sys smes
	" AC/ sending pid
	" MQ/ message
.rmes:
   jms awake
   lac o100000			" mark this process "not ready"
   tad u.ulistp i		" by incrementing status
   dac u.ulistp i
   law 2
   tad u.ulistp
   dac 9f+t			" pointer to msg status in proc table
   -1
   dac 9f+t i			" set to -1 (waiting for message)
   jms swap			" switch processes
   law 2
   tad u.ulistp
   dac 9f+t
   lac 9f+t i			" get msg status word
   cma				" complement (get sender pid)
   dac u.ac			" return in user AC
   dzm 9f+t i			" clear status word
   isz 9f+t
   lac 9f+t i			" get message
   dac u.mq			" return in user MQ
   dzm 9f+t i			" clear message
   jmp sysexit
t = t+1

"** 01-s1.pdf page 16
	" smes system call
	" AC/ pid
	" MQ/ message
	"   sys smes
	" returns with message delivered, or error if process does not exist
.smes:
   lac u.ac			" get pid from user AC
   sna spa			" >0?
   jms error			"  no: error
   jms searchu; 1f		" search for process
   law 2
   tad u.ulistp
   dac 9f+t
   dzm 9f+t i
   jms error
1: 0				" worker for searchu
   lac lu+1			" get pid
   sad u.ac			" match?
   skp				"  yes
   jmp 1b i			"   no
   lac lu+2			" get mailbox status
   sad dm1			" -1?
   jmp 1f			"  yes
   lac o100000			" no: bump our process status (to notready?)
   tad u.ulistp i
   dac u.ulistp i
   law 2
   tad u.ulistp
   dac 9f+t
   lac u.ac
   dac 9f+t i
   jms swap
   law 2
   tad u.ulistp
   dac 9f+t
   dzm 9f+t i
   jmp .smes
1:
   -3
   tad 8
   dac 9f+t			" pointer to dest process ulist entry
   lac o700000			" complement process status? marks ready??
   tad 9f+t i
   dac 9f+t i
   isz 9f+t
   isz 9f+t			" point to mailbox word?
   lac u.pid			" get our pid
   cma				" complement
   dac 9f+t i			" store in mailbox??
   isz 9f+t			" advance to next word
   lac u.mq			" get user MQ
   dac 9f+t i			" save as message
   jmp okexit
t = t+1

	" wake up process hanging on rmes for current process??
awake: 0
   jms searchu; 1f
   jmp awake i
1: 0				" searchu worker
   lac u.pid			" get caller pid
   sad lu+2			" match mailbox status?
   skp				"  yes
   jmp 1b i			"   no, return
   -3
   tad 8			" get pointer to process table entry
   dac 9f+t			" save in t0
"** 01-s1.pdf page 17
   lac o700000
   tad 9f+t i			" complement process status (mark ready)??
   dac 9f+t i
   jmp 1b i			" return from worker
t = t+1

swr:
sww:
   jmp .-4 i
   .halt; rttyi; rkbdi; rppti; .halt
   .halt; wttyo; wdspo; wppto

.halt: jms halt

	" read routine for ttyin special file
rttyi:
   jms chkint1
   lac d1		" ** written d3 ttyin2
   jms getchar
      jmp 1f
   and o177
   jms betwen; o101; o132	" upper case?
      skp			"  no
   tad o40			"   yes: convert to lower
   alss 9
   jmp passone
1:
   jms sleep; sfiles+0
   jms swap
   jmp rttyi

	" write routine for ttyout special file
wttyo:
   jms chkint1
   jms forall
   sna
   jmp fallr
   lmq
   lac sfiles+1
   spa
   jmp 1f
   xor o400000
   dac sfiles+1
   lacq
   tls				" load output buffer
   sad o12			" newline?
   jms putcr			"  yes, put CR as well
   jmp fallr
1:
   lacq
   dac char
   lac d2	"** written: d6 ttyout
   jms putchar
      skp
   jmp fallr
   jms sleep; sfiles+1
   jms swap
   jmp wttyo

	" read routine for (display) "keyboard" special file
rkbdi:
   jms chkint1
   lac d3
   jms getchar
"** 01-s1.pdf page 18
      jmp 3f
   lmq
   and o155
   sad o55
   jmp 1f			" -/=? map to =?-/ ???
   lacq
   and o137
   sad o134
   skp				" \| map to Ll ???
   jmp 2f
1:
   lacq
   xor o20
   lmq
2:
   lacq
   dac u.limit
1:
   jms chkint1
   lac u.limit
   jms dspput
      jmp 1f
   jms sleep; sfiles+6
   jms swap
   jmp 1b
1:
   lac u.limit
   alss 9
   jmp passone
3:
   jms sleep; sfiles+2
   jms swap
   jmp rkbdi

	" write routine for (graphic) "display" special file
wdspo:
   jms chkint1
   jms forall
   jms dspput			" put char (fallr "returns" here)
      jmp fallr			" go back for next (continuation!)
   jms sleep; sfiles+6
   jms swap
   jmp wdspo


	" read routine for paper tape reader special file
rppti:
   lac d4
   jms getchar
      jmp .+3
   alss 9
   jmp passone
   lac sfiles+3			" get sleep word
   sma				" high bit set?
   rsa				"  no: reader select alphanumeric mode
1:
   jms sleep; sfiles+3
   jms swap
   jmp rppti
"** 01-s1.pdf page 19

	" write routine for paper tape punch special file
wppto:
   jms forall
   sna
   jmp fallr
   lmq
   lac sfiles+4
   spa
   jmp 1f
   xor o400000
   dac sfiles+4
   lacq
   psa
   jmp fallr
1:
   lacq
   dac char
   lac d5
   jms putchar
      skp
   jmp fallr
   jms sleep; sfiles+4
   jms swap
   jmp wppto

	" common exit for special file input
passone:
   sad o4000			" CTRL/D?
   jmp okexit			"  yes: return zero
   dac u.base i			" no: save for user
   lac d1			" return 1
   dac u.ac
   jmp sysexit

error: 0
   -1
   dac u.ac
   jmp sysexit

chkint1: 0
   dzm .insys
   jms chkint
      skp
   jmp .save
   -1
   dac .insys
   jmp chkint1 i
