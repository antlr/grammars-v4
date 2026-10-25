" "lsd" -- phil's (minimal) "ls directory"
" for UNIX-7 without "." links (requires a dirname argument)
" adapted from warren's "historic version of ls"

" Usage: lsd dirname
"        lsl dirname
"
" with "lsl" displays long listing:
"    inum [dcl-][r-][w-][r-][w-] nlink uid size name
" with numbers in octal

argptr=017777			" pointer to argc, argv
argc=argptr i

main:
   lac argptr			" get pointer to argc, names
   tad d2			" point to second word of command name
   dac nextarg
   lac nextarg i		" fetch it
   dac longopt			" save (for long check)

   lac argc
   sad d8
    skp
     jmp usage			" no second arg

   lac nextarg
   tad d3			" skip rest of argv[0]

   dac dirname			" save for dir status
   dac dirname2			" save for dir open
   dac dirname3			" save for file status

   lac statbufptr		" stat the dir
   sys status; dd; dirname: 0
   spa
    jmp error
   lac s.flags
   and dirflg			" get dir bit
   sna				" a directory?
    jmp error			"  no: not a dir

   sys chdir; dd		" chdir to "dd" so we can open directory
   spa
    jmp error
   sys open; dirname2:0; 0 " open the directory
   spa
    jmp error
   dac dirfd			" save the fd

" loop reading blocks from directory file   
blkloop:
   lac dirfd; sys read; dbuf; 64 " read block from directory
   spa sna			" OK?
     jmp fileend		"  no: EOF or error
   dac dcount			" save word count

   lac idirptr			" reset dirptr
   dac dirptr

" loop reading 8-word directory entries:
" word 0 is i-number, words 1-4 are the file name
dentloop:
   lac dirptr i			" get i-number
   sna				" i-num non-zero (in use)?
    jmp nextdent		"  no, skip to the next directory entry

   lac lbufp			" get line buffer pointer
   dac 8			" save in index register

   lac longopt
   sad deesp			" short output?
    jmp prname			"  yes: just print name

" long output: "stat(us)" the file:
   lac dirptr			" get pointer to name
   tad d1
   dac fname			" save for status call

   lac statbufptr		" stat the file
   sys status; dirname3: 0; fname: 0
   spa
     jms fileend		" exit if the status call fails

   lac s.inum; jms octal; -5	" format i-number

" loop for file types: output one char or dash
   law types-1
   dac 9
   lac mntypes
   dac c
tloop:
   lac s.flags			" get mode bits
   and 9 i			" check bit
   sna
    jmp 1f			" not set
   lac 9 i			" bit set: get char
   jmp foundtype
1: isz 9			" skip char
   isz c			" more bits?
    jmp tloop			"  yes, loop and check
   lac dash			" no type found ("small" file) output '-'
foundtype:
   dac 8 i			" save in buffer

" loop for permission bits.  for each bit, output either char or dash
   law perms-1
   dac 9
   lac mnperms
   dac c
ploop:
   lac s.flags			" get flags
   and 9 i			" bit set?
   sna
     jmp 1f			"  no: output dash
   lac 9 i			" yes: get char
   jmp 2f
1: lac dash
   isz 9			" skip char
2: dac 8 i			" put char or dash in buffer
   isz c			" more bits?
    jmp ploop			"  yes, loop

   lac o40			" put space
   dac 8 i

   lac s.nlks			" format link count
   cma				" negate
   tad d1
   jms octal; -2

   lac s.uid			" format owner
   jms octal; -3

   lac s.size			" format size
   jms octal; -5

prname:
   lac dirptr			" get pointer to "dnode" (one before name)
   dac 9
   -4				" get word count
   dac c			" in c
1: lac 9 i			" copy name
   dac 8 i
   isz c
    jmp 1b

   lac o12			" add newline
   dac 8 i

   lac lbufp			" negate starting pointer
   cma
   tad d1
   tad 8			" add ending pointer
   dac 1f
   lac d1; sys write; lbuf; 1: 0 " output line

nextdent:
   lac dirptr			" advance to next "dnode"
   tad d8
   dac dirptr
   -8
   tad dcount			" Decrement the count of words in the buffer by 8
   dac dcount
   sza				" Anything left in the buffer?
    jmp dentloop		"  yes: keep going
   jmp blkloop			" no: try reading some more

fileend:
   nop
   lac dirfd
   sys close			" close dir fd
   sys exit

" octal print from ds.s
octal: 0
   lmq			" get number to print in MQ
   lac d5		" get octal "digits" (oits) to lose:
   tad octal i		" 5-oits
   cma			" get -(6-oits)
   dac c		" save as loop count
1: llss 3		" Lose top 3 bits of the MQ
   isz c		" nothing left to lose?
    jmp 1b		"  no: lose some more

   lac octal i		" get negative oit count
   dac c		" save as loop count
1: cla			" clear AC
   llss 3		" slide an oit's worth up into AC
   tad o60		" make ASCII
   dac 8 i		" output
   isz c		" more?
     jmp 1b		"  yes, loop

   lac o40		" output space
   dac 8 i
   isz octal		" skip the oits argument
   jmp octal i		" and return

usage:
   lac d1; sys write; usages; lusage
   sys exit

error:
   lac dirname
   dac 1f
   lac d1; sys write; 1:0; 4	" Write out the bad dirname
   lac d1; sys write; qnl; 1	" write ? NL
   sys exit

" constants

usages: <di>; <r 040; <na>; <me> " also writes next word!!
qnl: <? 012
lusage=.-usages

" file type bits, each followed by character to tag with
types:
dirflg:020; 0144	" directory: d
o40:040; 0143		" character device: c
   0200000; 0154 	" large file: l
mntypes: -3		" negative number of types

perms:
d8:000010; 0162		" user read: r
   000004; 0167		" user write: w
d2:000002; 0162  	" other read: r
d1:000001; 0167		" other write: w
mnperms: -4		" minus number of perms

d3: 3
d5: 5
d7: 7
o12: 012
dash: 055
o60: 060

dd: <dd>; 040040; 040040; 040040
deesp: <d 040

idirptr: dbuf		" Constant pointer to the directory buffer
statbufptr: statbuf	" Pointer to the statbuf
lbufp: lbuf-1		" initial index pointer to line buf

" variables
longopt: 0		" User set the -l option when this is 1

nextarg: .=.+1		" Pointer to the next argument
dirptr: .=.+1		" Pointer into dirbuf
dirfd: .=.+1		" File descriptor for the directory
dcount: .=.+1		" Count of words left in dbut
c: .=.+1		" loop counter

dbuf: .=.+64		" Directory buffer

statbuf:		" status buffer:
s.flags: .=.+1		" type/permission bits
s.dskps: .=.+7		" block numbers
s.uid: .=.+1
s.nlks: .=.+1		" negative count of links
s.size: .=.+1		" size in words
s.uniq: .=.+1
s.inum: .=.+1		" i-number: not in inode!

lbuf:	" buffer a line of output (using location 8 as index)

