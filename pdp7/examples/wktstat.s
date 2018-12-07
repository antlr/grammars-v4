" Warren's version of stat. stat file
" Like ls -l, except that we don't print out the i-node

main:

   lac 017777 i		" Load the pointer pointer in 017777
   sad d4		" to see if we have any arguments
     jmp nofiles	" No arguments, stop now
   lac 017777           " Move five words past the argument word count
   tad d5               " so that AC points at the first argument
   dac statfile		" and save in statfile and name
   dac name

   lac statbufptr	" Get the file's details into the statbuf
   sys status; curdir; statfile:0
   spa
     jmp badfile

			" Ugly code. Improvements welcome!
   lac s.perm		" See if this is a directory
   and isdirmask
   sna
     jmp 1f
   lac fd1
   sys write; d; 1	" Yes, print a d
   jmp 2f
1: lac s.perm		" Not a dir, see if its a large file
   and largemask
   sna			
     jmp 1f
   lac fd1
   sys write; l; 1	" Yes, print an l
   jmp 2f
1: lac fd1
   sys write; s; 1	" Not a dir, not large, print an s

2: lac s.perm		" Readable by owner?
   and ureadmask
   sna
     jmp 1f
   lac fd1
   sys write; r; 1	" Yes, print an r
   jmp 2f
1: lac fd1
   sys write; minus; 1	" No, print a - sign

2: lac s.perm		" Writable by owner?
   and uwritemask
   sna
     jmp 1f
   lac fd1
   sys write; w; 1	" Yes, print a w
   jmp 2f
1: lac fd1
   sys write; minus; 1	" No, print a - sign

2: lac s.perm		" Readable by other?
   and oreadmask
   sna
     jmp 1f
   lac fd1
   sys write; r; 1	" Yes, print an r
   jmp 2f
1: lac fd1
   sys write; minus; 1	" No, print a - sign

2: lac s.perm		" Writable by other?
   and owritemask
   sna
     jmp 1f
   lac fd1
   sys write; w; 1	" Yes, print a w
   jmp 2f
1: lac fd1
   sys write; minus; 1	" No, print a - sign

2: lac fd1
   sys write; space; 1	" Print a space

   lac s.nlinks		" Print the number of links out
                        " but first make it positive
   cma
   tad d1
   jms octal; -2
   lac s.uid		" Print the user-id out
   jms octal; -3
   lac s.size		" Print the size out
   jms octal; -5

printname:
   lac fd1
   sys write; name:0; 4	" Write the filename out to stdout
   lac fd1
   sys write; newline; 1	" followed by a newline
   sys exit

nofiles:
   lac d8
   sys write; 1f; 5             " Write "No files\n" to stderr
   sys exit                     " and exit

1: <no>; 040;  <fi>;<le>;<s 012

badfile:
   lac name                     " Get the pointer to the filename
   dac 1f                       " Store it in 1f below
   lac d8                       " Load fd 8 which is stderr
   sys write; 1:0; 4            " Write the four words of the filename
   lac d8
   sys write; 1f; 2             " and then write " ?\n"
   sys exit

1: 040;077012                   " String literal: " ?\n"

			" Octal print code: This code borrowed from ds.s
octal: 0
   lmq			" Move the negative argument into the MQ
			" as we will use shifting to deal with the
			" number by shifting groups of 3 digits.

   lac d5		" By adding 5 to the negative count and
   tad octal i		" complementing it, we set the actual
   cma			" loop count up to 6 - count. So, if we
   dac c		" want to print 2 digits, we lose 6 - 2 = 4 digits
1:
   llss 3		" Lose top 3 bits of the MQ
   isz c		" Do we have any more to lose?
     jmp 1b		" Yes, keep looping
   lac octal i		" Save the actual number of print digits into c
   dac c		" as a negative number.
1:
   cla
   llss 3		" Shift 3 more bits into AC
   tad o60		" Add AC to ASCII '0'
   dac cbuf		" and print out the digit
   lac fd1
   sys write; cbuf; 1
   isz c		" Any more characters to print out?
     jmp 1b		" Yes, loop back
   lac fd1		" Print out a space
   sys write; space; 1
   isz octal		" Move return address 1 past the argument
   jmp octal i		" and return from subroutine

longopt: 0		" User set the -l option when this is 1
d1: fd1: 1		" File descriptor 1, stdout
d4: 4
d5: 5
d8: 8
o60: 060
count: 0		" Count of # of directory words read in
cbuf: 0			" Used to print out in the octal routing
c: .=.+1		" Loop counter for printing octal digits

statbufptr: statbuf		" Pointer to the statbuf
statbuf:			" Status buffer fields below
s.perm: 0
s.blk1: 0
s.blk2: 0
s.blk3: 0
s.blk4: 0
s.blk5: 0
s.blk6: 0
s.blk7: 0
s.uid: 0
s.nlinks: 0
s.size: 0
s.uniq: 0
largemask:  200000  		" large file, bigger than 4096 words
isdirmask:  000020  		" is a directory
ureadmask:  000010  		" user read
uwritemask: 000004		" user write
oreadmask:  000002  		" other read
owritemask: 000001		" other write

d: 0144				" ASCII characters: d, l, s, r, w, -, space, \n
l: 0154
s: 0163
r: 0162
w: 0167
minus: 055
space: 040
newline: 012
curdir: <. 040; 040040; 040040; 040040          " i.e. "."
