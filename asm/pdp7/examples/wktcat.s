" Warren's cat program: cat [arg1 arg2 ...]

main:
   " Load the pointer pointer in 017777 to see if we have any arguments
   lac 017777 i
   sad d4		" Skip if we have more than four argument words
     jmp stdinout	" Only four argument words, so no arguments

   lac 017777           " Move five words past the argument word count
   tad d1               " so that AC points at the first argument
   tad d4

" This section opens files and copies their contents to standard output
catfiles:
   " We start with AC pointing to an argument. Save it at the "name" label
   dac name

   " Open the file and get the fd into AC
   sys open; name:0; 0;
   spa
     jmp badfile	" Negative fd, exit with an error message
   dac fd		" Save the file descriptor

fileloop:
   " Read 64 words into the buffer from the input file
   lac fd
   sys read; buf; 64
   spa			" Skip if result was >= 0
     jmp error		" Result was -ve, so error result
   sna			" Skip if result was >0
     jmp fileend	" Result was zero, so nothing left to read

   " Save the count of words read in
   dac 1f

   " Write five words from the buffer to stdout
   lac d1
   sys write; buf; 1:0

   " and loop back for more words to read
   jmp fileloop

fileend:
   " Close the open file descriptor
   lac fd
   sys close

   " Subtract 4 from the count of argument words
   -4
   tad 017777 i
   dac 017777 i
   sad d4		" Is the value 4, i.e. no args left?
     jmp end		" Yes, so exit

   " Still an argument, so move up to the next filename argument
   lac name
   tad d4
   dac name
   jmp catfiles		" and loop back to cat this file

end:
   " exit
   sys exit

" This section copies from standard input to standard output
" We cheat by setting the fd value to zero and storing 8
" into the argc word count, so that when the code hits
" fileend, the word count drops to 4 and we exit.
stdinout:
   lac d8
   dac 017777 i		" Save 8 into the word count
   lac d0
   dac fd		" Save file descriptor 0
   jmp fileloop

" This code comes from the real cat.s
badfile:
   lac name		" Get the pointer to the filename
   dac 1f		" Store it in 1f below
   lac d8		" Load fd 8 which is stderr
   sys write; 1:0; 4	" Write the name, max 4 words
   lac d8		" Then write " ?\n"
   sys write; 1f; 2
   sys exit		" and exit

1: 040; 077012		" String literal: " ?\n"

error:
   " Print an "err read" string on stderr and exit
   lac d8
   sys write; noreadstr; 5
   sys exit

noreadstr:
   <er>;<r 040;<re>;<ad>;012000

fd: 0		" fd of the open file
d0: 0		" Constants 0, 1, 4 and 8
d1: 1
d4: 4
d8: 8		" stderr seems to have fd 8
minus4:	0777774	" Constant -4

" Input buffer for read
buf: .=.+64
