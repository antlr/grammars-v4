"** 01-s1.pdf page 28
" s5
	" read/write a process from/to swap space
	" call:
	" AC/ first word of process table
	"   jms dskswap; DSLD bits
dskswap: 0
   cll; als 3			" get process disk address
   dac 9f+t			" save in t0
   jms dsktrans; -64; userdata; 9f+t; dskswap	" read/write user area
   lac 9f+t			" get swap addr back
   tad o20			" advance by 16??
   dac 9f+t			" save
   jms dsktrans; -4096; 4096; 9f+t; dskswap	" read/write user memory
   isz dskswap			" skip bits
   jmp dskswap i		" return
t = t+1

access: 0
   lac i.flags			" get inode flags
   lmq				" save in MQ
   lac u.uid			" get user id
   spa				" negative?
   jmp access i			"  yes: super user, return
   sad i.uid			" compare to file owner
   lrs 2			"  same: shift flags down two
   lacq				" get inode flags back
   and mode			" AND with mode from system call
   sza				" access allowed?
   jmp access i			"  yes: return
   jms error			" no: return error from system call

fassign: 0
   -10				" loop count
   dac 9f+t			" in t0
1:
   lac 9f+t			" get count
   tad d10			" turn into fd
   jms fget			" fetch open file into "fnode"
      jms halt " will not happen
   lac f.flags			" get fnode flags
   sma				" sign bit set (active)?
   jmp 1f			"  no: free
   isz 9f+t			" increment loop count & loop until zero
   jmp 1b
   jmp fassign i
1:
   lac mode			" get mode from system call
   xor o400000			" set sign bit
   dac f.flags			" save in fnode
   lac ii			" get i-number
   dac f.i			" save in fnode
   lac 9f+t
   tad d10			" get fd
   dac u.ac			" return in user AC
   dzm f.badd			" clear file offset in fnode
   jms fput			" copy fnode back into u.ofiles
   isz fassign			" give skip return
   jmp fassign i
t = t+1

	" load fnode (open file entry) from u.ofiles
	" AC/ user fd
	"   jms fget
	"    bad fd
	"   return with fnode set
fget: 0
   jms betwen; d0; d9		" fd 0..9?
      jmp fget i		"  no, return
   cll; mul; 3			" multiply by three
   lacq
"** 01-s1.pdf page 29

   tad ofilesp			" get pointer into u.ofiles
   dac 9f+t			" save in t0
   dac .+2			" save as copy source
   jms copy; ..; fnode; 3	" copy to "fnode"
   isz fget			" give skip return
   jmp fget i

	" copy fnode back to u.ofiles
	" uses temp value set by "fget"
	" (fget and fput calls must be paired)
fput: 0
   lac 9f+t
   dac .+3
   jms copy; fnode; ..; 3
   jmp fput i
t = t+1

	" helper for special device write routines
	" return to caller with next character to write:
	" when done return character count to user.
forall: 0
   lac u.base
   sad u.limit
   jmp 1f			" done
   lac u.base			" get base pointer (again?)
   ral				" rotate MSB into LINK
   lac u.base i			" fetch word via base pointer
   snl				" link set?
   lrs 9			"  no: get low 9 bits
   and o777			" mask to 9 bits
   jmp forall i			" return char
fallr:				" jump here for subsequent characters
   lac u.base			" get base
   add o400000			" advance pointer
   dac u.base			" start from top
   jmp forall+1
1:
   lac u.count
   dac u.ac
   jmp sysexit

	" wait for a condition variable
	" call:
	"   jmr sleep; sfiles+N
sleep: 0
   law ulist-1		" pointer to process table
   dac 8		" in index register
   lac o200000
   lmq			" get 200000 in MQ
1:
   lac u.ulistp i	" get current process status word
   sad 8 i		" compare to next process table entry
   jmp 1f		"  match
   isz 8		" no match: skip other 3 words of process table
   isz 8
   isz 8
   cla; lrs 1		" shift MQ down one
   jmp 1b		" loop
1:
   tad o100000		" mark process not ready
   dac u.ulistp i
   lac sleep i		" get sleep variable pointer
   dac 9f+t
   lac 9f+t i		" get sleep variable contents
   omq			" or in MQ (bit vector of processes waiting)
   dac 9f+t i		" save
   isz sleep		" skip sleep variable pointer
   jmp sleep i
t = t+1

"** 01-s1.pdf page 30

	" look for a free directory entry??
dslot: 0
   dzm di
   skp
1:
   isz di
   lac di
   jms dget			" get directory entry
   lac d.i			" get i number
   sza				" zero (free)?
   jmp 1b			"  no keep looking
   jmp dslot i			" yes: return index

	" called with:
	" AC/ mode
	" name/ file name
icreat: 0
   dac 9f+t			" save mode in t0
   jms dslot
   lac o20			" look for a free inode starting at 17
   dac ii
1:
   isz ii
   lac ii
   jms iget
   lac i.flags
   spa				" "in use" bit bit clear?
   jmp 1b			"  no, guess again
   lac ii
   dac d.i
   jms copy; name; d.name; 4
   isz s.uniq			" get a new unique number
   lac s.uniq
   dac d.uniq			" save in directory entry
   dac i.uniq			" and inode
   lac 9f+t			" get mode
   xor o400000			" set "in use"
   dac i.flags
   lac u.uid			" get user
   dac i.uid			" set owner
   -1
   dac i.nlks			" -1 links?!
   dzm i.size			" empty
   jms copyz; i.dskps; 7	" clear disk block numbers
   jms iput			" write inode
   jms dput			" write directory entry
   jmp icreat i
t = t+1

	" output character to display
dspput: 0
   and o177			" mask to 7 bits
   sna
   jmp i dspput			" discard NUL
   sad o14			" Form Feed?
   jmp 1f			"  yes
   lmq				" save char in MQ
   sad o12			" newline?
   jms dspnl			"  yes
   lac dsploc i			" get display word
   sad o400000			" TRAP?
   jmp dspleft			"  yes (first character)
   omq				" no: or in second character
   dac dsploc i			" save both characters
   isz dsploc			" advance display pointer
   jmp i dspput

"** 01-s1.pdf page 31

1:				" here on form feed
   jms dspinit			" clear display
   jmp dspput i

dspleft:
   lac dsploc			" get display pointer
   sad edspbuf			" end of the buffer?
   jmp 1f			"  yes: light up a button!?
   dac 8			" no: store in index
   lac o400000			" get TRAP instruction
   dac 8 i			" store as next
   cla; llss 18+7		" shift MQ upto "char1" position
   dac dsploc i			" store word with char1
   jmp dspput i

dspnl: 0			" only called once!
   lac dsplno			" get display line number
   sad d33			" 33?
   jmp 1f			"  yes: light a button!
   isz dsplno			" no: increment line number
   jmp dspnl i
1:
   lac o2000			" get bit for push button 7
   wbl				" write button lights
   isz dspput
   jmp dspput i

dspinit: 0
   lac dspbufp3			" get pointer to dspbuf+3
   dac dsploc			" store location
   lac o400000			" display "TRAP" instruction (end of list)
   dac dspbuf+3			" save in buffer
   dzm dsplno			" clear display line number
   jmp dspinit i

	" "move display" (change display buffer pointer)
	" called with new buffer pointer in AC
movdsp: 0
   iof
   cdf				" clear display flags (stop)
   dac dspbufp			" save display buf pointer
   -1
   dac .dspb			" .dspb = -1
   ion
   jmp movdsp i

	" fetch user argument from word after "sys" call, return in AC
arg: 0
   lac u.rq+8 i			" fetch word after return PC
   isz u.rq+8			" bump PC
   jmp arg i

	" fetch user pathname argument from pointer word after "sys" call
	" into "name" and look it up (in current working directory)
	" if lookup fails: returns error to user (not caller)
	" if lookup OK: returns with file i-inode read (into "inode")
	"	and i-number in AC
argname: 0
   jms arg			" fetch name pointer
   dac .+2			" save as copy source
   jms copy; ..; name; 4	" copy to "name"
   lac u.cdir			" get CWD
   jms namei; name		" look up file
      jms error			"  failed: return error directly to user
   jmp argname i

	" common code for seek/tell system calls
seektell: 0
   jms arg			" fetch 1st arg after sys
   dac u.base			" save in u.base

"** 01-s1.pdf page 32
   jms arg			" fetch 2nd arg (whence??)
   dac u.limit			" save in u.limit
   jms finac			" fetch fnode & inode for fd in AC
   lac u.limit			" get whence arg back
   sna				" zero?
   jmp seektell i		"  yes, return zero (relative to start)
   sad d1			" one?
   jmp .+3			"  yes: return pos (relative to current)
   lac i.size			" no: return file size (relative to end)
   jmp seektell i
   lac f.badd
   jmp seektell i

	" system call helper
	" fetch system call argument name pointer from after "sys" call
	" read inode; if current user is not owner, return error to user
isown: 0
   jms argname
   jms iget
   lac u.uid			" get user id
   sma				" super user?
   sad i.uid			"  no: does user own file?
   skp				"   yes, or super user
   jms error			"    not super user, not owner: return error
   jmp isown i

