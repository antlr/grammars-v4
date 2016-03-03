" S1

.. = 0
t = 0
orig:
   hlt
   jmp pibreak

. = orig+7
   -1

. = orig+020
   1f
   iof
   dac u.ac
   lac 020
   dac 1f
   lac 1f-1
   dac 020
   lac u.ac
   jmp 1f+1
   1f
1: 0
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
   jmp coldentry
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
   jms chkint
      skp
   jmp .save			" dump core??
   jms copy; u.rq+2; 10; 6	" restore auto-index locations 10-15
   lac u.rq+1			" restore auto-index location 9
   dac 9
   lac u.rq			" restore auto-index location 8
   dac 8
   lac u.mq			" restore MQ register
   lmq
   lac u.ac			" restore AC register
   jmp u.rq+8 i			" return to user

swap: 0
   ion
1:
   jms lookfor; 3 " out/ready
      jmp 1f
   jms lookfor; 1 " in/ready
      skp
   jmp 1b
   dzm maxquant
   jmp 3f
1:
   dac 9f+t
   jms lookfor; 2 " in/notready
      jmp 1f
   jms lookfor; 1 " in/ready
      jmp 1f
   jmp 2f
1:
   lac swap
   dac u.swapret
   iof
   lac o200000
   tad u.ulistp i
   dac u.ulistp i
   ion
   jms dskswap; 07000
   lac u.dspbuf
   sna
   jmp 2f
   law dspbuf
   jms movdsp
2:
   iof
   lac o600000
   tad 9f+t i
   dac 9f+t i
   ion
   jms dskswap; 06000
   lac u.swapret
   dac swap
   lac o20
   dac maxquant
   lac u.dspbuf
   sza
"** 01-s1.pdf page 4
   jms movdsp
3:
   dzm uquant
   iof
   jmp swap i
t = t+1

swp:			" system call dispatch table
   jmp .		" base instruction
   .save; .getuid; .open; .read; .write; .creat; .seek; .tell
   .close; .link; .unlink; .setuid; .rename; .exit; .time; .intrp
   .chdir; .chmod; .chown; badcal; .sysloc; badcal; .capt; .rele
   .status; badcal; .smes; .rmes; .fork
swn:
   .-swp-1 i		" count of system calls, plus indirect!

.intrp:
   lac u.ac
   dac u.intflg
   jmp okexit

.sysloc:		" "sysloc": syscall to return system addresses
   lac u.ac
   and o17777
   jms betwen; d1; locn
      jms error
   tad locsw
   dac .+1
   lac ..
   dac u.ac
   jmp sysexit

locsw:			" table of system data structures for "sysloc" call
   lac .
   iget; inode; userdata; sysdata; copy; copyz; betwen; dskrd
   dskwr; dskbuf; dpdata; namei; pbsflgs; alloc; free; dspdata
   crdata
locn:
   .-locsw-1

chkint: 0
   lac .insys
   sza
   jmp chkint i
   lac .int1
   sna
   jmp 1f
   sad u.ofiles+2
   jmp 2f
1:
   lac .int2
   sna
   jmp chkint i
   sad u.ofiles+2
   skp
   jmp chkint i
   dzm .int2
   jmp 1f
2:
   dzm .int1
1:
"** 01-s1.pdf page 5
   lac u.intflg
   sza
   jmp chkint i
   -1
   dac .insys
   ion
   isz chkint
   jmp chkint i

