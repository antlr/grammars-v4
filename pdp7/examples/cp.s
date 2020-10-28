" cp: cp file1 file2 [file3 file4 [file5 file6] ...]
"
" Copies in pairs: file1 to file2, file3 to file4 etc.

   lac 017777			" Skip past argc and save
   tad d1			" argv[0] (our name) into name2
   dac name2			" We will skip past it later

loop:
   lac 017777 i			" Any arguments left?
   sad d4
     sys exit			" 4 words = no args left, exit
   sad d8			" Do we have 2 args?
     jmp unbal			" No, an unbalanced set of arguments
   tad dm8			" Subtract 8 (two args) from the argc
   dac 017777 i			" and save it
   lac name2			
   tad d4
   dac name1			" Skipping pairs of filenames? not sure
   tad d4
   dac name2
   sys open; name1: 0; 0	" Open the input file
   spa
     jmp error			" File open error
   lac o17			" Why load 15 (017) into AC?
   sys creat; name2: 0		" Create the output file
   spa
     jmp error			" File create error
   dzm nin			" Set the number of input words to zero

1:
   lac bufp			" Set up the base of the upcoming read
   tad nin			" to be the buffer + nin so we skip
   dac 0f			" the existing words in the buffer
   -1
   tad nin			" Calculate 1024 - nin, i.e. the number
   cma				" of empty words yet to be filled in the
   tad d1024			" buffer, and use it as the read count
   dac 0f+1
   lac d2			" Read from fd 2: hard-wired in fd!
   sys read; 0:..;..
   sna
     jmp 2f			" No words were read in, go to 2f
   tad nin			" Add the number of words read in
   dac nin			" to the existing number of words
   sad d1024
     jmp 2f			" We do have 1,024 words, go to 2f
   jmp 1b			" Loop back if we don't have 1,024 words

2:
   lac nin			" Load the number of words in the input buffer
   dac 2f			" Save in the write word count argument
   lac d3			" Write to fd 3: hard-wired out fd!
   sys write; buf; 2:0
   dzm nin			" Set nin back to zero
   lac 2b			" Get the write count (updated by sys write)
   sad d1024			" Did we write the buffer out?
     jmp 1b			" Yes, we wrote 1,024 words, so loop back
				" to read another buffer's worth
   lac d2		
   sys close			" Close fd 2 and fd 3
   lac d3
   sys close
   jmp loop			" Loop back to deal with the next arguments

error:				" File error, print out the name1 on
   lac name1			" standard output, fd 1 followed by " ?\n"
   dac 1f
   lac d1			
   sys write; 1:0; 4
   lac d1
   sys write; mes; 1
   lac name2			" Then do the same with name 2
   dac 1f
   lac d1
   sys write; 1: 0; 4
   lac d1
   sys write; mes; 2
   jmp loop			" Loop back to deal with the next arguments

mes:
   040000;077012		" String literal: " ?\n"

unbal:				" We had an unbalanced set of arguments
   lac name2			" so print out the name after name2
   tad d4			" on standard output followed by " ?\n"
   dac 1f
   lac d1
   sys write; 1: 0; 4
   lac d1
   sys write; mes; 2
   sys exit

d1: 1				" Numeric constants
d2: 2
d3: 3
d4: 4
d8: 8
o17: 017
dm8: -8
d1024: 1024
nin: 0				" Number of words in the input buffer
bufp: buf			" Pointer to the buffer
buf: .=.+1024			" Buffer of 1,024 words
