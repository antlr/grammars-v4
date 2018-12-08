" Warren's version of od: od filename
"

main:
   lac 017777 i		" Do we have any arguments?
   sad d4
     jmp noarg		" No, an error

   lac 017777           " Move five words past the argument word count
   tad d5		" so that AC points at the first argument
   dac argptr

   sys open; argptr:0; 0 " Open up the file, save the file descriptor
   spa
     jmp badfile
   dac fd

lineloop:
   lac fd
   sys read; buf; 8	" Read 8 words from the file
   sna
     sys exit		" Nothing read, exit now
   dac count

   lac offset		" Print out the offset
   jms octal;
   lac fd1
   sys write; colon; 1	" Write colon space
   lac ibufptr
   dac bufptr		" Start at the beginning of the buffer
  

printloop:
   lac bufptr i		" Print out the next word
   jms octal;
   lac fd1
   sys write; space; 1	" Write a space
   -1
   tad count		" Decrement the count
   dac count
   sna
     jmp endofline	" Nothing left to print
   isz bufptr
   jmp printloop


endofline:
   lac fd1
   sys write; newline; 1 " Write a space
   lac d8
   tad offset		" Bump up the offset
   dac offset
   jmp lineloop





octal: 0
   lmq			" Move the argument into MQ
   -6
   dac c		" Set the loop count to 6
1:
   cla
   llss 3		" Shift 3 more bits into AC
   tad o60		" Add AC to ASCII '0'
   dac cbuf		" and print out the digit
   lac fd1
   sys write; cbuf; 1
   isz c		" Any more characters to print out?
     jmp 1b		" Yes, loop back
   jmp octal i		" and return from subroutine


noarg:
   " Print a "no arg" error and exit
   lac d1
   sys write; noargstr; 4
   sys exit
noargstr: <no>; < a>; <rg>; 012

badfile:
   lac argptr       	" Get the pointer to the argument
   dac 1f               " Store it in 1f below
   lac d1
   sys write; 1:0; 4    " Write the name, max 4 words
   lac d1               " Then write " ?\n"
   sys write; 1f; 2
   sys exit             " and exit
1: 040; 077012          " String literal: " ?\n"

ibufptr: buf
bufptr: buf
buf: .=.+8		" Buffer of eight words
count: 0		" Count of words read in
offset: 0		" Offset from the beginning of file
cbuf: 0                 " Used to print out in the octal routing
c: .=.+1                " Loop counter for printing octal digits
fd: 0

fd1: d1: 1
d4: 4
d5: 5
d8: 8
o60: 060

colon: 072040		" Colon space string
space: 040		" Space string
newline: 012		" Newline string
