" -*-fundamental-*-
" pbsh -- a shell
" started by p budne 3/4/2016
" with code from init.s, cat.s and looking at the v1 (pdp-11) shell

" In particular, the newline/newcom/newarg/newchar processing loop(s)
" are copied from the v1 shell:
" 1. Redirection must occur at the start of a name (after whitespace)
" 2. In parroting the v1 shell code, I did NOT add whitespace removal
" after > and <

" includes ';' and '&' (which are mentioned as added close after "fork")

" does NOT include quoting (backslash or quotes), which are mentioned
" in the v1 shell man page: http://man.cat-v.org/unix-1st/1/sh

" does NOT implement >> (not mentioned in the v1 sh man page)

" No "globbing" (performed by /etc/glob in v1 shell); McIlroy's
" "Reader" paper reports that cp/mv syntax changed in response to the
" introduction of globbing, the the surviving "cp" command takes src
" dest pairs.

" v1 shell expects "-" as argument from init or login, will read a
" filename passed as the first argument.  *BUT* the PDP-7 init.s
" doesn't set up an arg pointer at the top of memory, so it's unlikely
" the PDP-7 shell took command line arguments!!!

" cat.s seems to write error output on fd 8, but "init" (which opened
" stdin and stdout) doesn't set it up, and the shell doesn't know what
" device is on stdout (passed by init, and init doesn't pass fd 8) and
" there isn't a "dup" call, or an "indirect" device like /dev/tty
" (nor does init make an equivalent link), so the shell doesn't have
" a way to open the correct device).

" Direct exection of "runcom" files _could_ be implemented by reading
" the first block of the "binary" (into 010000) and seeing if any have
" any bits in 0300300 set before jumping into the "boostrap" code.

" If that's the case, then the child could open "sh" instead of the
" command file, close stdin, and open the command file, read the first
" block of "sh" into 010000, and jumping into the bootstrap, but there
" isn't any evidence that this was the case.

" Arguments for new processes are located at the end of memory.
" Location 17777 points to a word with the argument count (argc),
" followed by blocks of four words with (filename) arguments.

" Currently leave room for ONLY maxargs items.
" 10 is enough to build "cold start" system (as sop.s s1.s ... s9.s)
" this could be made dynamic (collect args lower in memory, calculate
" argptr, and copy the names up).
maxargs=10

" see if reading from a special file
   cla; sys seek; 1; 0		" try seeking stdin forward one word
   sna				" new offset non-zero?
    jmp 1f			"  no: input is a special file (ttyin,keyboard)
   dzm prompt			" yes: regular file, kill prompt
   cla; sys seek; 0; 0		" seek file back to start
   jmp newline

1: lac d1
   sys intrp			" make shell uninterruptable
   sys getuid
   sma				" <0?
    jmp newline			"  no
   lac hash			" yes: superuser
   dac prompt			" change prompt

newline:
   lac d1; sys write; prompt; 1	" output prompt
   jms rline			" read (edited) line into lbuf
   lac ilpt
   dac lpt			" reset line pointer

" parse new command from current input line
" (after ';' or '&')
newcom:
   dzm char			" clear saved char
   dzm infile			" clear input redirect file name
   dzm outfile			" clear output redirect file name
   lac iopt			" reset output buffer pointer
   dac opt
   dac nextarg

" reset high memory
   dzm argc			" clear arg count
   lac argcptr			" (re) set arg pointer
   dac argptr
   dzm argv0			" clear out argv0 for chdir comparison
   dzm argv0+1
   dzm argv0+2

" NOTE! behavior copied from v1 shell!!!
" "improvements" here may be non-historic!!
newarg:
   -8				" save 8 chars
   dac bcount
   dzm redirect			" clear redirect flag

   lac opt			" save start for print (TEMP)
   dac nextarg

   jms blank			" skip whitespace
   jms delim			" command sep?
    jmp eoc			"  yes
   sad lt			" input redirect?
    jmp redirin			"  yes
   sad gt			" output redirect?
    jmp redirout		"  yes
   jmp savechar			" no: save as filename

redirin:			" saw <
   dac redirect			" flag redirect
   lac infilep
   dac opt
   jmp newchar

redirout:			" saw >
   dac redirect			" flag redirect
   lac outfilep
   dac opt
   " fall

newchar:			" loop reading a file name
   jms getc
   sad o40			" space?
    jmp eoname			"  yes
   jms delim			" no: end of line?
    jmp eoname			"  yes
savechar:
   jms putc			" no: save
   isz bcount			" loop unless full
    jmp newchar

" here after 8 chars: discard until terminator seen
1: jms getc
   jms delim			" end of line?
    jmp 2f
   sad o40
    jmp 2f
   jmp 1b

2:
   dac char			" save terminator
   jmp full

" name ended (short) with whitespace or delim
" pad out last name to 8 with spaces
eoname:
   dac char			" save terminator
1: lac o40
   jms putc			" copy into argv
   isz bcount			" loop until full
    jmp 1b
full:
   lac redirect
   sza
    jmp 2f			" last name was a redirect file, skip increment

" file was not a redirection:
   lac argc			" increment argc
   tad d4
   dac argc
   lac nextarg
   tad d4			" advance nextarg
   dac nextarg

2:
   dzm redirect			" clear redirect flag
   lac nextarg
   dac opt			" set output pointer

   lac char			" get terminator
   jms delim			" end of command?
    jmp eoc			"  yes

   lac argc			" another arg
   sad maxargwords		" full up?
    skp				"  yes
     jmp newarg			"   no: get another

" too many args
4: jms getc
   jms delim
    skp
     jmp 4b
   lac d1; sys write; toomany; ltoomany
   jmp newline			" ignore rest of line

" here at end of command
eoc:
   dac delimchar		" save command delimiter
   lac argc			" check for empty command line
   sna				" get anything?
    jmp nextcmd			" no, go back for another

" check for built-in "chdir" command
   lac argv0
   sad chdirstr
    skp
     jmp 1f
   lac argv0+1
   sad chdirstr+1
    skp
     jmp 1f
   lac argv0+2
   sad chdirstr+2
    jmp changedir

" here to execute command (not a builtin)
1: sys fork
    jmp parent

child:				" debug symbol
   sys open; argv0; 0		" try cwd (no link required)
   sma				" error?
    jmp 1f			"  no

   sys link; system; argv0; argv0
   spa
    jmp cmderr
   sys open; argv0; 0
   spa
    jmp cmderr
   dac cmdfd
   sys unlink; argv0
   skp
1:  dac cmdfd			" save command file descriptor
   cla				" check for input redirection
   sad infile			" input redirct?
    jmp 1f			"  no
   sys close			" close fd 0
   sys open; infile; 0		" open new stdin
   spa
    jmp inerror
   cla
1: sad outfile			" output redirect?
    jmp exec			"  no
   lac d1; sys close		" close fd 1
   lac o17; sys creat; outfile	" open new stdout
   spa
    jmp outerror

" here to "exec" file open on cmdfd, adapted from init.s
exec:
   law boot-1			" Get source addr
   dac 8			" set up index (pre-increments)
   law bootloc-1		" Copy "boot" code into high memory
   dac 9			" set up index
   -bootlen			" isz loop count for bootstrap copy
   dac bootcount
1: lac 8 i
   dac 9 i
   isz bootcount		" can only do this once!
     jmp 1b
   lac cmdfd			" get fd for the executable
   lmq				" Save the fd into MQ
   jmp bootloc			" and then jump to the code

" copied up to bootloc in high memory (below argc)
boot:
   sys read; userbase; userlen	" read executable in (check for non-zero ret?)
   lacq				" Get the fd back and close the file
   sys close			" close command file
   jmp userbase			" and jump to the beginning of the executable
bootlen=.-boot			" length of bootstrap

" error in child process:
inerror:			" error opening input redirection
  lac infilep
  jmp error
outerror:			" error opening new stdout (stdout closed!)
  lac outfilep
  skp
cmderr:				" error opening command
   lac argv0p
error:				" error in child: filename pointer in AC
  dac 1f			" save filename to complain about
  lac d1; sys write; 1: 0; 4
  lac d1; sys write; qmnl; 1
  lac d2; sys close		" close executable, if any
  sys exit			" performs implicit rmes

" chdir command: executed in shell process
" takes a series of directory names to chdir to in turn
changedir:
   lac argc
   sad d4
    jmp qm			" need at least one arg!
   lac argv0p
   skp
1:  lac 0f			" increment dir pointer
   tad d4
   dac 0f
   -4				" decrement argc
   tad argc
   dac argc
   sna				" done?
    jmp nextcmd			"  yes: join parent code
   sys chdir; 0:0
   sma				" error?
    jmp 1b			"  no: look for another directory


   lac 0b			" chdir failed: get offending name
   dac 0f			" store for write
   lac d1; sys write; 0:0; 4	" complain
qm:lac d1; sys write; qmnl; 1
   jmp nextcmd			" join parent code

" here in parent, child pid in AC
parent:
"	https://www.bell-labs.com/usr/dmr/www/hist.html
"	The message facility was used as follows: the parent shell, after
"	creating a process to execute a command, sent a message to the new
"	process by smes; when the command terminated (assuming it did not
"	try to read any messages) the shell's blocked smes call returned an
"	error indication that the target process did not exist. Thus the
"	shell's smes became, in effect, the equivalent of wait.
"
"	PLB: The "exit" system call code apears to "fall" into the
"	rmes code So Dennis' memory of what the shell did may have
"	been correct, but not for the reason he remembered.
   dac pid			" save child pid
   lac delimchar		" get command delimiter
   sad o46			" ampersand?
    jmp newcom			"  yes: go back without wait
   lac pid			" no: get pid
   sys smes			" hang until child exits
   nop
nextcmd:
   lac delimchar
   sad o73			" semi?
    jmp newcom			"  yes: look for another command on line
   jmp newline			" no: output prompt

" ================
" subroutines

" eat spaces
" v1 routine name:
blank: 0
1: jms getc
   sad o40
    jmp 1b
   jmp blank i

" give skip return if AC *NOT* a command delimiter
" v1 routine name:
delim: 0
   sad o12			" newline
    jmp delim i
   sad o46			" ampersand
    jmp delim i
   sad o73			" semi
    jmp delim i
   isz delim			" ran the gauntlet: skip home
   jmp delim i

" get character from lbuf
getc: 0
   lac lpt i			" fetch char from line buffer
   isz lpt			" increment pointer
   jmp getc i

" from init.s rline: read line into lbuf with editing
" (store one character per word)
rline: 0
   law lbuf-1			" pointer in location 8
   dac 8
1: jms readc
    jmp quit			" EOF
   sad o100			" '@' (kill) character?
    jmp rline+1			"  yes: start from scratch
   sad o43			" '#' (erase) character?
    jmp 2f			"  yes: handle below
   dac 8 i			" Store the character in the buffer
   sad o12			" Newline?
     jmp rline i		"  yes: return
   jmp 1b			" no: keep going
2:
   law lbuf-1			" # (erase) handling
   sad 8			" at start?
    jmp 1b			"  yes: noop, loop back
   -1				" no: decrement poiner
   tad 8
   dac 8
   jmp 1b

quit:
   lac d1; sys smes	" wake up init
   sys exit

" copied from cat.s

" was "getc"
readc: 0
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
     jmp readc+1		" Skip a NUL characters and read another one
   isz readc			" give skip return
   jmp readc i			" Return the character from the subroutine
1:
   cla				" Buffer is empty, read 64 chars from stdin
   sys read; iipt+1; 64
   sna
     jmp readc i		" No characters read: return without skip
   tad iipt			" Add the word count to the base of the buffer
   dac eipt			" and store in the end buffer pointer
   lac iipt			" Reset the ipt to the base of the buffer
   dac ipt
   jmp readc+1			" and loop back to get one character

putc: 0
   and o177			" Keep the lowest 7 bits and save into 2f+1
   dac 2f+1
   lac opt			" get output buffer pos
   dac 2f			" save
   add o400000			" Flip the msb (advance) and save back into opt
   dac opt
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
   jmp putc i			" No, so return (more room still in the buffer)

2: 0;0				" pointer, char
ipt: 0				" Current input buffer base
eipt: 0				" Pointer to end of data read in input buffer
iipt: .+1; .=.+64		" 64 word input buffer and pointer to it
" end from cat.s

" literals
d1: 1
d2: 2
o4:d4: 4
dm8: -8
o12: 012			" newline
o17: 017
o40: 040			" space
o43: 043			" #
o46: 046			" ampersand
o73: 073			" semi
o74:lt: 074			" <
o76:gt: 076			" >
o100: 0100			" @
o177: 0177			" 7-bit (ASCII) mask
o400000: 0400000		" MSB

hash: <#> " superuser prompt
qmnl: <? 012			" question mark, newline

system:
   <sy>;<st>;<em>; 040040

chdirstr:
   <ch>;<di>;<r 040

" TEMP FOR DEBUG:
star: <*> "

toomany: <to>;<o> ;<ma>;<ny>;< a>;<rg>;<s 012
ltoomany=.-toomany

maxargwords: maxargs+maxargs+maxargs+maxargs
argcptr: argc

infilep: infile
outfilep: outfile

ilpt: lbuf			" initial line buffer pointer
iopt:argv0p: argv0		" initial value for nextarg, opt

" ################ variables

prompt: <@ 040			" v1 prompt! cleared if input is regular file

redirect: .=.+1			" last file was a redirect (lt or gt)
nextarg: .=.+1			" next slot in argv to fill
bcount: .=.+1			" byte counter for current filename
opt: .=.+1			" "output pointer" (in/outfile or into argv)
delimchar: .=.+1		" character that terminated line
char: .=.+1			" char that terminated word (merge w/ delimchar?)

outfile: .=.+4			" buffer for output redirect file name
infile: .=.+4			" buffer for input redirect file name
pid: .=.+1			" "other" pid
cmdfd: .=.+1			" fd for executable
bootcount: .=.+1		" loop count for "boot" copy

lpt: .=.+1			" line buf pointer
lbuf:				" edited line, one char per word

" enter addresses into namelist:
.=017777			" last word points to argc + argv data
argptr:

.=argptr-maxargs-maxargs-maxargs-maxargs-1 " argc followed by argv
argc: .=.+1
argv0:				" 4 word blocks following argc

" "bootstrap" (reads executable into userbase) copied JUST below argc
.=argc-bootlen
bootloc:

userbase=010000			" user starts at 4K
userlen=bootloc-userbase	" max executable
