" chrm: chrm dir file [file file ...]
"
" chdir into the named directory and unlink the files that are in there
"
" The code depends on a "dd" directory existing which holds the named dir

   lac 017777			" Go to the argc
   tad d5			" Skip past the argc and argv[0]
   dac 1f			" Save argv[1] in the chdir arg below
   dac 2f			" and in the unlink as well? Yes, we skip it
   lac 017777 i			" How many arguments do we have?
   sad d4
     sys exit			" None, so exit
   tad dm4			" Subtract 1
   dac 017777 i			" and save in the argc

   sys chdir; dd		" chdir to dd
   sys chdir; 1:0		" and then into the first argument
1:
   lac 017777 i			" Any arguments left?
   sad d4
     sys exit			" No, exit the program
   tad dm4			" Subtract 4 from the argc and update it
   dac 017777 i
   lac 2f			" Move up to the next filename
   tad d4
   dac 2f			" and save it in the unlink arg
   sys unlink; 2:0		" Unlink the file
   sma
     jmp 1b			" Loop back if the unlink was OK, or issue err
   lac 2b			" Copy the filename pointer below
   dac 2f
   lac d1			" Write the filename on stdout
   sys write; 2:0; 4
   lac d1
   sys write; 1f; 2		" Write " ?\n" on stdout
   jmp 1b			" and loop back

1:
   040077;012000		" String literal " ?\n"
dd:
   <dd>;040040;040040;040040	" Filename dd
d1: 1
d4: 4
d5: 5
dm4: -4
