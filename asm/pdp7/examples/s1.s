" S1

.. = 0
t = 0
orig:
   hlt				" overwritten with interrupt return addr
   jmp pibreak			" dispatch to interrupt processing

. = orig+7
   -1				" only ever set (to -1): never read?!

. = orig+020			" syscall (CAL) and user "interrupt" processing
   1f				" addr for "CAL I": store return here on "CAL"
   iof				" interrupts off
   dac u.ac			" save user AC
   lac 020			" save user return addr
   dac 1f			" save as if "CAL I"
   lac 1f-1
   dac 020			" restore location 20
   lac u.ac			" restore user AC
   jmp 1f+1			" join "CAL I" processing
   1f				" literal to restore location 20
1: 0				" "CAL I" PC stored here
   iof				" interrupts off
   dac u.ac			" save user AC
   lacq
   dac u.mq			" save user MQ
   lac 8
   dac u.rq			" save user auto-index location 8
   lac 9
   dac u.rq+1			" save user auto-index location 9
   jms copy; 10; u.rq+2; 6	" save user auto-index locations 10-15
   lac 1b			" load user PC after system call
   dac u.rq+8			" save user PC
   -1				" load -1
   dac .savblk			" set "save" flag (cleared by disk I/O?)
   dac .insys			" set "in system" flag
   lac uquant			" load user quantum count
   jms betwen; d0; maxquant	" check if between 0 & maxquant??
      jms swap			" no: swap processes
   ion				" interrupts on
   -1
   tad u.rq+8			" get address of system call
   jms laci			" load AC indirect??
   jms betwen; o20001; swn	" range check
      jmp badcal		" bad system call
   tad swp			" add system call table base
   dac .+1			" save as next instruction
   jmp .. i			" dispatch system call

. = orig+0100
   jmp coldentry		" here to start kernel
   jms halt

okexit:
   dzm u.ac			" 'OK' system call exit: clear user AC
sysexit:			" common system call exit code
   ion				" enable interrupts
   lac .savblk			" load "save" flag
   sza				" is zero (cleared by disk I/O)?
   jmp 1f			" no: no disk I/O done?
   jms copy; sysdata; dskbuf; 64 " copy system data to disk buffer
   cla
   jms dskio; 07000		" save to disk?
1:
   dzm .insys			" clear "in system call" flag
   jms chkint			" pending user interrupt?
      skp			"  no, return to user
   jmp .save			"   yes: dump core
   jms copy; u.rq+2; 10; 6	" restore auto-index locations 10-15
   lac u.rq+1			" restore auto-index location 9
   dac 9
   lac u.rq			" restore auto-index location 8
   dac 8
   lac u.mq			" restore MQ register
   lmq
   lac u.ac			" restore AC register
   jmp u.rq+8 i			" return to user

	" scheduler / swapper / idle loop
swap: 0
   ion
1:
   jms lookfor; 3 " out/ready
      jmp 1f
   jms lookfor; 1 " in/ready
      skp
   jmp 1b			" loop until a process becomes ready
   dzm maxquant			" in/ready (self?): come back next tick!
   jmp 3f
1:				" here with out/ready process
   dac 9f+t			" save process pointer (swapped out) in t0
   jms lookfor; 2 " in/notready	" find a swapped in process to swap out?
      jmp 1f
   jms lookfor; 1 " in/ready
      jmp 1f
   jmp 2f
1:
   lac swap
   dac u.swapret		" return to caller when swapped back
   iof
   lac o200000			" change status to swapped out
   tad u.ulistp i
   dac u.ulistp i
   ion
   jms dskswap; 07000		" swap process out
   lac u.dspbuf
   sna				" process using display??
   jmp 2f			"  no
   law dspbuf			" reset to default display buffer
   jms movdsp
2:
   iof				" disable interrupts
   lac o600000			" change status (1->7?????)
   tad 9f+t i
   dac 9f+t i
   ion				" enable interrupts
   jms dskswap; 06000		" read process in?
   lac u.swapret		" set our return addr
   dac swap			" to saved return addr
   lac o20			" reset maxquant to 16 ticks
   dac maxquant
   lac u.dspbuf
   sza				" using display?
"** 01-s1.pdf page 4
   jms movdsp			"  yes. switch to user display bufferx
3:
   dzm uquant			" no. reset process tick count
   iof
   jmp swap i			" return
t = t+1

swp:			" system call dispatch table
   jmp .		" base instruction
   .save; .getuid; .open; .read; .write; .creat; .seek; .tell
   .close; .link; .unlink; .setuid; .rename; .exit; .time; .intrp
   .chdir; .chmod; .chown; badcal; .sysloc; badcal; .capt; .rele
   .status; badcal; .smes; .rmes; .fork
swn:
   .-swp-1 i		" count of system calls, plus indirect!

	" AC/ new value for intflg (non-zero to ignore interrupt char)
	"   sys intrp
.intrp:
   lac u.ac
   dac u.intflg
   jmp okexit

	" syscall to retrieve system addresses (data & routines!!)
	" AC/ index (1..17)
	"   sys sysloc
	" AC/ address (or -1 on bad index)
.sysloc:
   lac u.ac
   and o17777
   jms betwen; d1; locn
      jms error
   tad locsw
   dac .+1
   lac ..
   dac u.ac
   jmp sysexit

locsw:			" table of system addresses for sysloc
   lac .
   iget; inode; userdata; sysdata; copy; copyz; betwen; dskrd
   dskwr; dskbuf; dpdata; namei; pbsflgs; alloc; free; dspdata
   crdata
locn:
   .-locsw-1

	" check if "interrupt" for current process
	" checks .int1 and .int2 (contain i-number of interrupt source)
	" compared against process stdin
	"
	" call:
	" .insys/ 0
	"   jms chkint
	"    <already ".insys", no interrupt, or intflg set (ignore interrupt)>
	"   <found user interrupt: PI off, .insys set>
chkint: 0
   lac .insys
   sza				" in system?
   jmp chkint i			"  yes: return
   lac .int1			" get inumber of interrupt1 source?
   sna				" zero?
   jmp 1f			"  yes: skip stdin check
   sad u.ofiles+2		" non-zero: compare to stdin inumber
   jmp 2f			"  same
1:
   lac .int2			" get inum of interrupt 2 source?
   sna				" zero?
   jmp chkint i			"  yes: return
   sad u.ofiles+2		" non-zero: compare to stdin inumber
   skp				"  match!
   jmp chkint i			"   no match: return
   dzm .int2			" clear int2 source
   jmp 1f
2:
   dzm .int1			" clear int1 source
1:
"** 01-s1.pdf page 5
   lac u.intflg			" get user intflg
   sza				" zero?
   jmp chkint i			"  no: ignore
   -1
   dac .insys			" set "in system" flag
   ion				" enable interrupts
   isz chkint			" give skip return
   jmp chkint i

