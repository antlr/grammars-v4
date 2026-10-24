" cat: cat arg1 [arg2 ...]

   " Load the pointer pointer in 017777 to see if we have any arguments
   lac 017777 i
   sad d4			" Skip if we have more than four argument words
     jmp nofiles		" Only four argument words, so no arguments
   lac 017777			" Move five words past the argument word count
   tad d1			" so that AC points at the first argument
   tad d4			" and save the pointer in name
   dac name

loop:
   sys open; name:0; 0		" Open file, get fd back
   spa
     jmp badfile		" Negative fd, exit with an error message
   dac fi			" Save file descriptor in fi

1:
   jms getc			" Get a character in AC
   sad o4
     jmp 1f			" Break the loop when we get a ctrl-D
   jms putc			" Write the character on stdout
   jmp 1b			" and loop back

1:
   lac fi			" Close the file descriptor in fi
   sys close

loop1:
   -4
   tad 017777 i			" Subtract 4 from the count of argument words
   dac 017777 i
   sad d4			" Is the value 4, i.e. no args left?
     jmp done			" Yes, so exit

   lac name			" Still an argument, so move up
   tad d4			" to the next filename argument
   dac name
   jmp loop			" and loop back to cat this file

badfile:
   lac name			" Get the pointer to the filename
   dac 1f			" Store it in 1f below
   lac d1			" Load fd 1 which is stdout
   sys write; 1:0; 4		" Write the four words of the filename
   lac d1
   sys write; 1f; 2		" and then write " ?\n"
   jmp loop1			" Now try doing the next argument

1: 040;077012			" String literal: " ?\n"

nofiles:
   lac d1
   sys write; 1f; 5		" Write "No files\n" to stderr
   sys exit			" and exit

1: <no>; 040;  <fi>;<le>;<s 012

done:
   lac noc			" Is the number of characters left zero?
   sna
     sys exit			" Yes, exit
   and d1
   sna cla
     jmp 1f
   jms putc			" Store the character in the buffer
   jmp done			" and loop back
1:
   lac noc			" Get the number of characters in the buffer
   rcr				" Divide by two to convert to words
   dac 1f			" Save in the write's word count below
   lac fo			" Load fd 1, stdout
   sys write; iopt+1; 1:..	" Write the leftover buffer and exit
   sys exit

getc: 0
   lac ipt			" Load the pointer to the next word in the buffer
   sad eipt
     jmp 1f			" We've reached the end of the buffer, so read more
   dac 2f			" Save the pointer
   add o400000			" Flip the msb and save into ipt
   dac ipt
   ral				" Move the msb into the link register
   lac 2f i			" Load the word from the buffer
   szl				" Skip if this is the second character in the word
     lrss 9			" It's the first char, shift down the top character
   and o177			" Keep the lowest 7 bits
   sna
     jmp getc+1			" Skip a NUL characters and read another one
   jmp getc i			" Return the character from the subroutine
1:
   lac fi			" Buffer is empty, read another 64 characters
   sys read; iipt+1; 64
   sna
     jmp 1f			" No characters were read in
   tad iipt			" Add the word count to the base of the buffer
   dac eipt			" and store in the end buffer pointer
   lac iipt			" Reset the ipt to the base of the buffer
   dac ipt
   jmp getc+1			" and loop back to get one character
1:
   lac o4			" No character, return with ctrl-D
   jmp getc i			" return from subroutine

putc: 0
   and o177			" Keep the lowest 7 bits and save into 2f+1
   dac 2f+1
   lac opt			" Save the pointer to the empty buffer
   dac 2f			" position to 2f
   add o400000			" Flip the msb and save back into opt
   dac opt			" This also has the effect of incrementing
				" the opt pointer every second addition!

   spa				" If the bit was set, we already have one
     jmp 1f			" character at 2f+1. If no previous character,
   lac 2f i			" merge the old and new character together
   xor 2f+1
   jmp 3f			" and go to the "save it in buffer" code
1:
   lac 2f+1			" Move the character up into the top half
   alss 9
3:
   dac 2f i			" Save the word into the buffer
   isz noc			" Add 1 to the char count, never skipping
   lac noc			" Have we reached 128 characters, 64 words?
   sad d128
     skp
   jmp putc i			" No, so return (more room still in the buffer)
   lac fo			" Load fd1 (i.e stdout)
   sys write; iopt+1; 64	" and write out the 64 words in the buffer
   lac iopt
   dac opt			" Set opt pointing back to base of buffer
   dzm noc			" Set the number of chars in the buffer to 0
   jmp putc i			" and return

2: 0;0				" Current input and output word pointers
ipt: 0				" Current input buffer base
eipt: 0				" Pointer to end of data read in input buffer
iipt: .+1; .=.+64		" 64 word input buffer and pointer to it
fi: 0				" Input file descriptor
opt: .+2			" Current output buffer base
iopt: .+1; .=.+64		" 64 word output buffer and pointer to it
noc: 0				" Number of output characters
fo: 1				" Output file descriptor, fd 1 is stdout

d1: 1				" Octal and decimal constants
o4:d4: 4
d8: 8
o400000: 0400000		" Msb toggle bit
o177: 0177			" ASCII mask
d128: 128			" 128 words in the output buffer
