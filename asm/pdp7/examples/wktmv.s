" Warren's version of mv. mv file1 file2

main:

   lac 017777 i		" Load the pointer pointer in 017777
   sad d8		" to see if we have two
     jmp nofiles	" No arguments, stop now
   lac 017777           " Move five words past the argument word count
   tad d5               " so that AC points at the first argument
   dac name1		" and save it
   tad d4		" Then do the same for the second name
   dac name2

   sys rename; name1:0; name2:0		" Rename the file
   spa
     jmp badfile	" Print out an error on failure
   sys exit


nofiles:
   lac d1
   sys write; 1f; 5             " Write "No files\n" to stdout
   sys exit                     " and exit

1: <tw>; <o 040;  <fi>;<le>;<s 012

badfile:
   lac name1                    " Get the pointer to the filename
   dac 1f                       " Store it in 1f below
   lac d1                       " Load fd 1 which is stdout
   sys write; 1:0; 4            " Write the four words of the filename
   lac d1
   sys write; 1f; 2             " and then write " ?\n"
   sys exit

1: 040;077012                   " String literal: " ?\n"

d1: 1
d4: 4
d5: 5
d8: 8
