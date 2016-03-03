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
   dac mode
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
   lac d1
   dac mode
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
   lac d1
   dac mode
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

.open:
   jms arg
   dac 0f
   jms arg
   sza
   lac d1
   sna
   lac d2
   dac mode
   lac u.cdir
   jms namei; 0:0
      jms error
   jms iget
   jms access
   lac i.flags
   and o20
   sna
   jmp open1
   lac mode
   and d1
   sna
   jmp open1
   lac u.uid
   sma
   jms error
   jmp open1

.creat:
   lac d1
   dac mode
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
   lac u.ac
   and o17
   jms icreat
open1:
   jms fassign
      jms error
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
   lac i.flags
   and o40
   sna
   jmp 1f
   iof
   lac ii
   tad swr
   dac .+1
   jmp .. i
1:
   lac u.base
   dac 1f+1
   lac u.count
   dac 1f+2
   lac f.badd
1:
   jms iread; ..; ..
   jmp exitrw

.write:
   jms arg
   and o17777
   dac u.base
   jms arg
   dac u.count
   tad u.base
   jms betwen; u.base; o17777
      jms error
   dac u.limit
   jms finac
   lac f.flags
   and d1
   sna
   jms error
   lac i.flags
   and o40
"** 01-s1.pdf page 12
   sna
   jmp 1f
   iof
   lac ii
   tad sww
   dac .+1
   jmp .. i
1:
   lac u.base
   dac 1f+1
   lac u.count
   dac 1f+2
   lac f.badd
1:
   jms iwrite; ..; ..

exitrw:
   dac u.ac
   tad f.badd
   dac f.badd
   jms iput
   jms fput
   jmp sysexit
