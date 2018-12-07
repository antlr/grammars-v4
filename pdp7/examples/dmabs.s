   " dmabs

	" takes a list of files on command line and punches out each
	" to paper tape with a "hardware read-in" (HRI) bootstrap
	" program in front of them.

	" Bob Supnik says:
	" The PDP-7/9/15 hardware read-in used a simple format where
	" the start address was specified in the address switches, and
	" then 18b words were deposited sequentially in memory. Binary
	" format was distinguished by having the high order bit (0200)
	" in each 8b tape frame set, so that leader (all 0s) could be
	" ignored. A frame with bit 1 set (0100) indicated end of
	" load; the last word was supposed to be either a JMP (to
	" start the program running) or a HLT.

   lac o17
   sys creat; punout		" open paper tape punch
   spa
   sys save			" open failed: dump core
   dac fo
   lac 017777
   tad d1
   dac name			" get pointer to argv[0]
   jms space			" punch 10 inch leader
   100

loop:				" loop for input files
   dzm oldsum			" clear checksum
   lac initcmd
   dac comand			" reset command to "dac"
   lac i 017777			" get arg count
   sad d4			" anything left?
   jmp stop			"  no: stop
   tad dm4			" decrement argc by 4
   dac i 017777
   lac name			" advance arg pointer
   tad d4
   dac name

dump1:		" (label not used)
   lac comand			" get command word
   xor dactra			" transmute dac to jmp
   dac tracmd

dump2:		" (label not used)
   sys open; name: 0; 0		" open file
   spa
   jmp opnerr			" open failed
   dac fi			" save input fd
   -bootsiz			" get length of bootstrap
   dac c1
   law boot-1			" get pointer to boostrap in i8
   dac 8
1:
   lac i 8			" fetch word from bootstrap
   jms put			" write to tape
   isz c1			" done?
   jmp 1b			"  no: loop
   lac bootcmd			" write "jmp" instruction to tape
   lrs 12
   jms put1
   lac bootcmd
   lrs 6
   jms put1
   lac bootcmd
   and o77
   xor o300			" final frame of jmp word: 0100 set
   jms put2

   jms space			" punch three empty frames
   3

dump3:				" block loop
   -1				" reset i8 to buffer
   tad bufp
   dac 8
   -64				" get count
   dac c1
1:
   dzm i 8			" loop zeroing buffer
   isz c1
   jmp 1b
   lac fi
   sys read; bufp: buf; 64	" read block from input file
   sna				" got some?
   jmp done			"  done
   dac count			" save count
   -1
   tad bufp
   dac 8			" point i8 to input buffer
   -64
   dac c1			" reset count
   cla
1:				" loop for 1's complement sum of buffer
   add i 8
   isz c1
   jmp 1b
   sna				" sum non-zero?
   jmp dump4			"  no: is zero: skip this block
   dac newsum			" save as new sum
   lac comand			" get command word
   jms put			" write to tape
   lac count			" get positive count
   jms put			" write to tape
   lac oldsum
   add comand
   add count
   jms put			" write oldsum+command+count
   lac newsum			" copy new sum to old sum
   dac oldsum
   jms space			" write three empty frame
   3
   -1				" reset i8 to input buffer
   tad bufp
   dac 8
   -1
   tad count
   cma
   dac c1			" reset counter
1:				" loop writing block to tape
   lac i 8
   jms put
   isz c1
   jmp 1b
   jms space			" follow with 1 inch trailer
   10

dump4:				" end of block
   lac comand			" advance command word by count
   tad count
   dac comand
   jmp dump3			" loop for another block

done:				" here at end of input file
   lac tracmd			" get transfer command (jmp)
   jms put			" write to tape
   cla				" write a zero word
   jms put
   lac oldsum			" get checksum
   add tracmd			" add in jmp command word
   jms put			" write to tape
   jms space			" write two inch trailer
   20
   lac fi			" close input file
   sys close
   jmp loop			" loop for another input file

stop:
   cla
   jms put			" write a zero binary word to tape
   jms space			" write 10 inch trailer
   100
   sys exit

space: 0			" punch empty frames (count after jms)
   -1
   tad i space
   cma
   dac c1			" store negative count
   isz space			" skip count word
1:
   lac o400			" defeat NUL removal
   jms put2			" write one empty frame
   isz c1			" done?
   jmp 1b			"  no
   jmp i space

put: 0				" write a (binary) word (as three frames)
   dac 1f
   lrs 12
   jms put1
   lac 1f
   lrs 6
   jms put1
   lac 1f
   jms put1
   jmp i put
1:0

put1:0				" write one frame of a binary word
   and o77
   xor o200			" light the high bit
   jms put2
   jmp i put1

put2: 0				" write one frame to tape
   dac 1f
   lac fo
   sys write; 1f; 1
   jmp i put2
1: 0

boot:
   org = 017740
2:
   jms get1-boot+org		" get command word from tape
   dac cmd-boot+org
   jms get1-boot+org		" get count from tape
   cma
   dac cnt-boot+org		" store complemented (incremented below)
   jms get2-boot+org		" get checksum word
   xor sum-boot+org		" clear those bits in sum
   dzm sum-boot+org		" clear out sum
   cla cll sza			" checksum match? & clear AC & LINK
   hlt				"  no: checksum mismatch
   isz cnt-boot+org		" yes: increment count (complete negate)
1:
   jms get1-boot+org		" get data word from tape
cmd: 0				" command word (read from tape)
   isz cmd-boot+org		" increment command instruction
   isz cnt-boot+org		" increment count
   jmp 1b-boot+org		"  not done, keep going
   jmp 2b-boot+org
get1: 0				" get checksummed word from tape
   jms get2-boot+org		" read a word
   dac get2-boot+org		" save (in get2 return addr word!)
   add sum-boot+org		" 1's complement sum
   dac sum-boot+org		" save sum
   lac get2-boot+org		" get word back
   jmp i get1-boot+org		" return
get2: 0				" get unchecksummed word from tape
   iot 0144			" "rsb" ptr select binary mode
1:
   iot 0101			" "rsf" ptr skip if flag set
   jmp 1b-boot+org		"  loop until ready
   iot 0112			" "rrb" ptr clear flag, or in read buffer
   jmp i get2-boot+org
sum: 0
   cnt = sum+1
   bootsiz = .-boot
bootcmd: jmp org

opnerr:
   lac name
   dac 1f
   lac d1
   sys write; 1: 0; 4
   lac d1
   sys write; mes; 2
   jmp loop
mes:
   040;077012			" space, ?, NL

comand: 0
tracmd: 0
   d1: 1
o17777: 017777			" unused?
o77: 077
o200: 0200
o300: 0300
d4: 4
d64: 64
dm4: -4
o400: 0400
punout: <pp>;<to>;<ut>;040040
o17: 017

fi: 0
fo: 0
count: 0
oldsum: 0
newsum: 0
daccmd: dac
dactra: dac jmp
initcmd: dac 0
c1: 0
buf:

iot = 0700000
