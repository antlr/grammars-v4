"** 01-s1.pdf page 7
" s2

.status:
   jms arg
   dac .+5
   jms arg
   dac .+6
   lac u.cdir
   jms namei; ..
      jms error
   jms namei; ..
      jms error
   jms iget
   lac u.ac
   and o17777
   jms betwen; o10000; o17762
      jms error
   dac .+3
   jms copy; inode; ..; 12
   lac d.i
   dac 9 i
   jmp okexit

.capt:
   lac u.ac
   dac u.dspbuf
   jms movdsp
   jmp sysexit

.rele:
   dzm u.dspbuf
   law dspbuf
   jms movdsp
   jmp sysexit

.chmod:
   jms isown
   lac u.ac
   and o17
   lmq
   lac i.flags
   and o777760
   omq
   dac i.flags
   jms iput
   jmp okexit

.chown:
   jms isown
   lac u.ac
   dac i.uid
   jms iput
   jmp okexit

.getuid:			" getuid system call
   lac u.uid
   dac u.ac			" return u.uid in user AC
   jmp sysexit

.seek:
   jms seektell
   tad u.base
"** 01-s1.pdf page 8
   spa
   jms error
   lmq
   lac f.flags
   and d1
   sna
   jms 1f
   lacq
   jms betwen; d0; i.size
      jms dacisize
   jmp 2f
1:
   lacq
   jms betwen; d0; i.size
      lac i.size
2:
   dac f.badd
   dac u.ac
   jms fput
   jmp sysexit

.tell:
   jms seektell
   cma
   tad d1
   tad u.base
   dac u.ac
   jmp sysexit

.link:
   jms arg
   dac 0f
   jms arg
   dac 1f
   jms arg
   dac 2f
   lac d4
   jms namei; 0:0
      jms error
   jms namei; 1:0
      jms error
   dac u.base
   jms copy; 2:0; name; 4
   lac u.cdir
   jms namei; name
      skp
   jms error
   lac d1
   dac mode			" save mode bits for access
   jms access
   jms dslot
   lac u.base
   jms iget
   lac ii
   dzm d.i
   jms copy; name; d.name; 4
   lac i.uniq
   dac d.uniq
   -1
   tad i.nlks
   dac i.nlks
"** 01-s1.pdf page 9
   jms iput
   jms dput
   jmp okexit

.unlink:
   jms argname
   dac u.base
   lac d1			" mode bit 1 (write?)
   dac mode			" save for access call
   jms access
   dac d.i
   jms dput
   lac u.base
   jms iget
   isz i.nlks
   jmp 1f
   jms itrunc
   dzm i.flags
1:
   jms iput
   jmp sysexit

.setuid:			" setuid system call
   lac u.uid			" load current user id
   sma				" negative?
   jms error			" no: error!!
   lac u.ac			" load user AC
   dac u.uid			" save as new uid
   jmp sysexit

.rename:
   jms arg
   dac 0f
   jms arg
   dac 1f
   lac u.cdir
   jms namei; 0:0
      jms error
   lac d1				" mode bit 1 (write?)
   dac mode				" save for access call
   jms access
   jms copy; 1:0; d.name; 4
   jmp okexit

	" time system call returns line (mains) frequency ticks since boot?
	" note: returns uptime!?
	" at 60Hz, 36 bits would last 36+ years!
.time:
   lac s.tim			" load high order bits
   dac u.ac			" return in AC
   lac s.tim+1			" load low order bits
   dac u.mq			" return in MQ
   jmp sysexit

.chdir:
   jms argname
   jms iget
   lac i.flags
   and o20
   sna
   jms error
   lac ii
   dac u.cdir
"** 01-s1.pdf page 10
   jmp okexit

	" open system call
	"   sys open; filename_ptr; flags (0 for read, 1 for write)
	" returns w/ "fd" in AC (or -1 if not found)
.open:
   jms arg			" get filename
   dac 0f			" save for namei
   jms arg			" get flags
   sza				" zero (read)
   lac d1			"  no: get write mode bit
   sna				" non-zero (write)?
   lac d2			"  no: get read mode bot
   dac mode			" save for access call
   lac u.cdir			" get current working directory
   jms namei; 0:0		" search for file
      jms error			" error: return -1
   jms iget			" load inode
   jms access			" check access (may return w/ error to user)
   lac i.flags			" get file flags
   and o20			" get directory bit
   sna				" is directory?
   jmp open1			"  no, join common code
   lac mode			" get access mode
   and d1			" get write bit
   sna				" write access?
   jmp open1			"  no, continue
   lac u.uid			" yes: get uid?
   sma				" negative? (-1 is superuser)
   jms error			"  no: return error
   jmp open1			" yes: join common code

.creat:
   lac d1			" mode bit 1 (write)
   dac mode			" save for access call
   jms arg
   dac .+2
   jms copy; ..; name; 4
   lac u.cdir
   jms namei; name
      jmp 1f
   jms iget
   jms access
   lac i.flags
   and o20
   sna
   jmp .+4
   lac u.uid
   sma
   jms error
   jms itrunc
   cla
   jms dacisize
   jmp open1
1:
   jms access
   lac u.ac			" get access bits from user AC (zero for lock!)
   and o17			" mask to permissions
   jms icreat
open1:				" common exit for open/creat
   jms fassign			" assign fd slot
      jms error			"  none free, return -1
   jmp sysexit

"** 01-s1.pdf page 11
.close:
   jms finac
   dzm f.flags
   jms fput
   jmp sysexit

.read:
   jms arg
   and o17777
   dac u.base
   jms arg
   dac u.count
   lac u.base
   jms betwen; o10000; o17777
      jms error
   tad u.count
   jms betwen; u.base; o17777
      jms error
   dac u.limit
1:
   jms finac
   lac f.flags
   and d1
   sza
   jms error
   lac i.flags			" get inode flags
   and o40			" get special file bit
   sna				" special?
   jmp 1f			"  no
   iof				" yes: disable interrupts
   lac ii			" get i number
   tad swr			" get read routine table addr
   dac .+1
   jmp .. i			" dispatch to read routine
1:
   lac u.base			" get user base
   dac 1f+1			" save as iread base
   lac u.count			" get user count
   dac 1f+2			" save as iread count
   lac f.badd			" get file offset
1:
   jms iread; ..; ..
   jmp exitrw

	" write system call:
	" AC/ fd
	"   sys write; buffer; count
	" AC/ count or -1 on error
.write:
   jms arg			" pick up buffer
   and o17777			" mask to addr
   dac u.base			" save as I/O base
   jms arg			" pick up count
   dac u.count			" save as count
   tad u.base			" add base (get limit)
   jms betwen; u.base; o17777	" check between base and end of memory
      jms error			"  no: error
   dac u.limit			" yes: save as limit
   jms finac			" get fnode with fd from user AC
   lac f.flags			" get open file table flags
   and d1			" open for write?
   sna				"  yes, skip
   jms error			"   no: error
   lac i.flags			" get inode flags
   and o40			" get special bit?
"** 01-s1.pdf page 12
   sna				" special?
   jmp 1f			"  no
   iof				" special file (device node)
   lac ii			" get i number
   tad sww			" get write routine
   dac .+1
   jmp .. i			" dispatch to write routine
1:				" here with regular file
   lac u.base			" get base
   dac 1f+1			" save as iwrite arg 1
   lac u.count			" get count
   dac 1f+2			" save as iwrite 2
   lac f.badd			" get fd offset
1:
   jms iwrite; ..; ..		" write to file

exitrw:				" common exit for read/write system calls
   dac u.ac			" save return in user AC
   tad f.badd
   dac f.badd			" update file offset
   jms iput			" release inode
   jms fput			" release fnode
   jmp sysexit			" return to user
