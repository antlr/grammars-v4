" chmod: chmod mode file [file file ...]
"
" mode is an octal number

   lac 017777 i		" Have we got any arguments?
   sad d4
     jmp error		" No, give an error
   lac 017777
   tad d4		" Skip past argc
   dac 8		" and save the ptr to the octal value at location 8
   tad d1		" Why only 1 here?
   dac name		" Save the filename pointer
   dzm octal		" Zero the octal value and
   dzm nchar		" the number of characters
   -8			" Set c1 to -8 so we can increment up to zero
   dac c1		" and thus count the number of chars in the argument

1:
   lac nchar		" Get any left-over character from the last loop
   dzm nchar		" and set nchar to empty now
   sza			" It was already empty, so get a new word
     jmp 2f		" Not empty, it has the second ASCII value, goto 2f
   lac 8 i		" Read the next word with two chars in it
   lmq			" Copy it to MQ
   and o177		" Trim off all but the second ASCII value
   dac nchar		" Save it into nchar
   lacq			" Bring it back from MQ
   lrss 9		" and shift down the first ASCII value

2:
   sad o40		" If the character is a space
     jmp 3f		" don't try to make it part of the octal value
   tad om60		" Not a space, subtract 060 i.e. '0'
   lmq			" Move it into MQ
   lac octal		" Load the octal value up to here
   cll; als 3		" Shift it left 3 bits
   omq			" OR in the digit from MQ
   dac octal		" and save back in the octal value

3:
   isz c1		" Is that the last character?
     jmp 1b		" No, go back and get another one

loop:
   lac 017777 i			" How many arguments left?
   sad d8
     sys exit			" None, exit
   tad dm4			" Subtract 4 to indicate one less argument
   dac 017777 i			" and save for next time
   lac name			" Also move to the next filename
   tad d4
   dac name
   lac octal			" Set AC to have the new file mode
   sys chmod; name:0		" and change the file's mode
   sma
     jmp loop			" Loop if no error
   lac name			" We got back -1, error
   dac 1f			" Write out the filename on stdout
   lac d1			" followed by the " ?\n" string
   sys write; 1:0; 4		" and loop back
   lac d1
   sys write; 1f; 2
   jmp loop

1:
   040;077012			" String literal " ?\n"

error:
   lac d1			" Write " "\n" to stdout and exit
   sys write; 1b+1; 1
   sys exit

d1: 1				" Numeric constants
d4: 4
d8: 8
dm4: -4
o40: 040
o177: 0177
om60: -060

nchar: .=.+1			" Number of characters in the value entered
c1: .=.+1
octal: .=.+1			" The resulting octal value
