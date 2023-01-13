"** 01-s1.pdf page 7
" s2

	" file status (stat) system call
	" AC/ pointer to status (inode) buffer + i-num (13 words)
	"   sys status; dir_name_ptr; file_name_ptr
	"   NO_DD version: sys status; file_name_ptr
.status:
   jms arg			" fetch directory name pointer
   dac .+5
   jms arg			" fetch file name pointer
   dac .+6
   lac u.cdir			" get current working directory
   jms namei; ..		" look up source directory
      jms error			"  not found: return error to user
   jms namei; ..		" look up file
      jms error			"  not found: return error
   jms iget			" read file inode
   lac u.ac			" get user buffer pointer
   and o17777			" truncate to 13 bits
   jms betwen; o10000; o17762	" is user memory (but not last 14 wds)?
      jms error			"  no: error
   dac .+3			" save as copy destination
   jms copy; inode; ..; 12	" copy inode to user buffer
   lac d.i			" copy i-num from last dnode read ??
   dac 9 i			" save thru index 9 (pre-increment) ??
   jmp okexit

	" capture display?
.capt:
   lac u.ac			" get user AC
   dac u.dspbuf			" save as user display buffer
   jms movdsp			" switch to user display buffer
   jmp sysexit

	" release display?
.rele:
   dzm u.dspbuf			" clear user display buffer pointer
   law dspbuf			" get default display buffer
   jms movdsp			" change to it
   jmp sysexit

.chmod:
   jms isown			" check if user owns file arg
   lac u.ac			" get new permissions
   and o17			" mask to read/write bits
   lmq				" save in MQ
   lac i.flags			" get file flags
   and o777760			" clear permissions
   omq				" or in new permissions from MQ
   dac i.flags			" save in inode
   jms iput			" write inode back
   jmp okexit

.chown:
   jms isown			" check if user owns file arg
   lac u.ac			" get new owner
   dac i.uid			" save in inode
   jms iput			" write inode back
   jmp okexit

.getuid:			" getuid system call
   lac u.uid
   dac u.ac			" return u.uid in user AC
   jmp sysexit

.seek:
   jms seektell			" fetch offset & whence (return seek base)
   tad u.base			" add offset
"** 01-s1.pdf page 8
   spa				" positive?
   jms error			"  no: error
   lmq				" save position in MQ
   lac f.flags			" get file flags
   and d1			" get write bit
   sna				" open for write?
   jmp 1f			"  no
   lacq				" yes: get position
   jms betwen; d0; i.size	" between zero and size?
      jms dacisize		"  no: store new size
   jmp 2f
1:
   lacq				" reading: get position
   jms betwen; d0; i.size	" between zero and size?
      lac i.size		"  no: get current size
2:
   dac f.badd			" save as offset
   dac u.ac			" return in AC
   jms fput			" copy fnode back to user area
   jmp sysexit			" return to user

.tell:
   jms seektell			" fetch offset & whence (return seek base)
   cma
   tad d1			" negate base
   tad u.base			" add to user offset
   dac u.ac			" return in user AC
   jmp sysexit

.link:
   jms arg			" Save the argument pointers in
   dac 0f			"0f, 1f and 2f
   jms arg
   dac 1f
   jms arg
   dac 2f
   lac d4			" Search the directory at i-num 4
   jms namei; 0:0		" for the first argument
      jms error			" Didn't find it
   jms namei; 1:0		" In the i-num found by 1st namei,
      jms error			" search for 2nd argument, err not found
   dac u.base			" save in user data
   jms copy; 2:0; name; 4
   lac u.cdir			" Search the process' current directory
   jms namei; name		" for the third argument
      skp
   jms error			" Error if it already exists
   lac d1
   dac mode			" Save mode bits for access
   jms access			" check access (or return error to user)
   jms dslot			" allocate directory slot
   lac u.base			" get source file i-number
   jms iget			" read inode in
   lac ii			" get the i-num
   dac d.i			" Save the i-num in the directory entry
   jms copy; name; d.name; 4	" Copy the new link name into the directory entry
   lac i.uniq			" Copy the i-node unique number into
   dac d.uniq			" the directory entry
   -1
   tad i.nlks			" Decrement link count, i.e. one more link
   dac i.nlks
"** 01-s1.pdf page 9
   jms iput			" Save the i-node and directory entry for
   jms dput			" the new link
   jmp okexit			" and return OK

.unlink:
   jms argname			" fetch filename, inode
   dac u.base			" save i-number
   lac d1			" write mode bit
   dac mode			" save for access call
   jms access			" check access or return error (reads inode)
   dzm d.i			" clear directory i-num
   jms dput			" write directory entry back
   lac u.base			" get i-number back
   jms iget			" read inode back
   isz i.nlks			" increment link count (kept as negative count)
   jmp 1f			"  not zero
   jms itrunc			" zero links: free blocks
   dzm i.flags			" clear status (free inode)
1:
   jms iput			" write inode back to disk
   jmp sysexit

.setuid:			" setuid system call
   lac u.uid			" load current user id
   sma				" negative (super user)
   jms error			"  no: error!!
   lac u.ac			" load user AC
   dac u.uid			" save as new uid
   jmp sysexit

	" rename system call:
	"   sys rename; old_name_ptr; new_name_ptr
	" Questions:
	"   when is directory entry read??
	"   is access check on directory or src file??
	"   check for existing file with new name??
	"   when is directory entry written back??
.rename:
   jms arg			" fetch first arg (old name pointer)
   dac 0f			" save for namei
   jms arg			" fetch second arg (new name pointer)
   dac 1f			" save for copy
   lac u.cdir			" get CWD
   jms namei; 0:0		" search for (old) name
      jms error			"  not found: return error
   lac d1			" get write mode bit
   dac mode			" save for access call
   jms access			" access OK? (or return error to user)
   jms copy; 1:0; d.name; 4	" copy new name into directory entry
   jms dput			" and write it to disk
   jmp okexit

	" time system call returns line (mains) frequency ticks
	" high order bits returned in AC, low order in MQ
	" s.tim is located in "system" block (written to disk)
	" so this is a running count of uptime since first boot!
	" at 60Hz, 36 bits would last 36+ years!
.time:
   lac s.tim			" load high order bits
   dac u.ac			" return in AC
   lac s.tim+1			" load low order bits
   dac u.mq			" return in MQ
   jmp sysexit

.chdir:
   jms argname			" fetch argument as filename
   jms iget			" (re)read inode(???)
   lac i.flags			" get flags
   and o20			" get directory bit
   sna				" is a directory?
   jms error			"  no: return error to user
   lac ii			" yes: get i-number
   dac u.cdir			" save as current working directory
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
   lac d2			"  no: get read mode bit
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
   jms arg			" get name pointer
   dac .+2			" save for copy
   jms copy; ..; name; 4	" copy filename to "name"
   lac u.cdir
   jms namei; name		" look up in current working directory
      jmp 1f			"  not found
   jms iget			" file exists: read inode
   jms access			" check access (or return error to user)
   lac i.flags			" get flags
   and o20			" get directory bit
   sna				" is a directory?
   jmp .+4			"  no: skip to truncate
   lac u.uid			" get user
   sma				" is super user?
   jms error			"  no: error
   jms itrunc			" yes: truncate
   cla
   jms dacisize			" clear i.size
   jmp open1
1:
   jms access			" here if not found
   lac u.ac			" get access bits from user AC (zero for lock!)
   and o17			" mask to permissions
   jms icreat
open1:				" common exit for open/creat
   jms fassign			" assign fd slot
      jms error			"  none free, return -1
   jmp sysexit

"** 01-s1.pdf page 11
.close:
   jms finac			" get fnode (open file) for fd in user AC
   dzm f.flags			" clear flags
   jms fput			" write fnode back to u.ofiles
   jmp sysexit

.read:
   jms arg			" get argument
   and o17777			" mask to address
   dac u.base			" save as I/O base
   jms arg			" get second argument
   dac u.count			" save as count
   lac u.base			" get base
   jms betwen; o10000; o17777	" end inside user memory?
      jms error			"  no: error
   tad u.count			" get end of buffer
   jms betwen; u.base; o17777	" inside buffer/user memory?
      jms error			"  no: error
   dac u.limit			" yes: save as I/O limit
1:
   jms finac			" get fnode for fd in user AC
   lac f.flags			" get open file flags
   and d1			" get write bit
   sza				" open for write?
   jms error			"  yes: error
   lac i.flags			" get inode flags
   and o40			" get special file bit
   sna				" special?
   jmp 1f			"  no
   iof				" yes: disable interrupts
   lac ii			" get i number
   tad swr			" add to base instruction
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
   sna				" if yes, skip
   jms error			"   no: error
   lac i.flags			" get inode flags
   and o40			" get device bit
"** 01-s1.pdf page 12
   sna				" special?
   jmp 1f			"  no
   iof				" yes, special: turn interrupts off
   lac ii			" get i number
   tad sww			" get write routine entry addr
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
