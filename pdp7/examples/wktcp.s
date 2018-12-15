" Warren's cp program: cp arg1 arg2

main:
   " Load the pointer pointer in 017777 to see if we have any arguments
   lac 017777 i
   sad d12
     jmp 1f		" We have 12 words, so we have 2 arguments
   jmp argserror	" Otherwise, print an error and exit

1: lac 017777           " Move five words past the argument word count
   tad d5               " so that AC points at the first argument

   " Save the pointer to the file name
   dac name

   " Open the input file and get the fd into AC
   sys open; name:0; 0;
   spa
     jmp badfile	" Negative fd, exit with an error message
   dac infd		" Save the file descriptor

   lac 017777           " Move nine words past the argument word count
   tad d9		" so that AC points at the second argument
   dac name
   dac name2

   " Open the ouput file and get the fd into AC
   sys open; name2:0; 1;
   spa
     jmp badfile	" Negative fd, exit with an error message
   dac outfd		" Save the file descriptor

fileloop:
   " Read 64 words into the buffer from the input file
   lac infd
   sys read; buf; 64
   spa			" Skip if result was >= 0
     jmp readerror	" Result was -ve, so error result
   sna			" Skip if result was >0
     jmp fileend	" Result was zero, so nothing left to read

   " Save the count of words read in
   dac 1f

   " Write five words from the buffer to the output file
   lac outfd
   sys write; buf; 1:0
   spa			" Skip if result was >= 0
     jmp writeerror	" Result was -ve, so error result

   " and loop back for more words to read
   jmp fileloop

fileend:
   " Close the open file descriptors
   lac infd
   sys close
   lac outfd
   sys close
   sys exit

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

readerror:
   " Print an "err read" string on stderr and exit
   lac d8
   sys write; noreadstr; 5
   sys exit

noreadstr:
   <er>;<r 040;<re>;<ad>;012000

writeerror:
   " Print an "err write" string on stderr and exit
   lac d8
   sys write; nowritestr; 6
   sys exit

nowritestr:
   <er>;<r 040;<wr>;<it>;<e 012

argserror:
   " Print an "bad args" string on stderr and exit
   lac d8
   sys write; badargs; 5
   sys exit

badargs:
   <ba>;<d 040;<ar>;<gs>;012000


infd: 0		" fd of the input file
outfd: 0	" fd of the output file
d5: 5
d9: 9
d8: 8		" stderr seems to have fd 8
d12: 12

" Input buffer for read
buf: .=.+64
