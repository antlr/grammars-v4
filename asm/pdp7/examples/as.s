"** 05-1-4.pdf page 32
" as

   jms init1			" initialize for pass 1

assm1:
   lac eofflg
   sza				" saw EOF?
   jmp assm2			"  no.
   lac passno			" yes
   sza				" pass==0?
   jmp finis			"  no, pass 2: done
   jms init2			" initialize for pass 2

assm2:				" main loop
   jms gchar			" get character
   sad d4			" comma space or tab?
   jmp assm1			"  yes, ignore
   sad d5			" newline or ';'??
   jmp assm1			"  yes, ignore
   lac char
   dac savchr			" no, push back
   jms gpair
   lac rator
   jms betwen; d1; d6		" plus, minus, space comma tab or semi?
   jmp assm3			"  no
   jms expr
   lac passno
   sza				" pass 1?
   jms process			"  no, process on pass 2
   isz dot+1			" increment "."
   nop
   lac dot+1
   and o17777
   sad dot+1			" overflow?
   jmp assm1			"  no
   jms error; >>		" '>' error: past end of memory
   dzm dot+1			" start again at zero!
   jmp assm1

assm3:
   lac rand
   sad d2
   jmp assm4			"  yes
   sza
   jmp assm6
   lac rator			" fetch operator
   sza				" ":"?
   jmp assm6			"  no
   lac rand+1
   jms betwen; dm1; d10		" numeric and 0..9?
   jmp assm6			"  no
   dac name			" yes, save as name
   tad fbxp
   dac lvrand
   lac i lvrand			" get fbx entry
   dac name+1			" save in second word of name
   isz i lvrand			" increment fbx entry
   lac o146			" get 'f'
   dac name+2			" save in third word of name
   dzm name+3			" clear fourth word
   jms tlookup			" look it up
   -1
"** 05-1-4.pdf page 33
   dac fbflg			" set fbflg to -1
assm4:
   lac rand+1
   tad d4
   dac lvrand
   lac rator			" get operator
   sza				" ':'?
   jmp assm5			"  no
   lac dot			" load dot type
   dac r			" save as r type
   lac dot+1			" get dot value
   dac r+1			" save as r value
   jmp 1f

assm5:
   jms gpair
   jms expr
1:
   lac r
   dac i lvrand
   isz lvrand
   lac r+1
   dac i lvrand
   lac fbflg
   sna				" fb flag set?
   jmp assm1			"  no
   dzm fbflg			" clear fb flag
   dzm name+1
   lac o142			" get 'b'
   dac name+2
   jms lookup
   jmp assm4

assm6:
   jms error; x>		" "x" error -- various errors
   jmp assm1

init1: 0			" init for pass 1
   lac d1
   sys write; 1f; 2f-1f		" output I, newline
   dzm passno			" clear passno
   lac o56040			" load ". "
   dac dot-4
   lac o56056			" load ".."
   dac cmflx-4
   lac o40040			" pad ". " and ".." names with spaces
   dac dot-3
   dac dot-2
   dac dot-1
   dac cmflx-3
   dac cmflx-2
   dac cmflx-1
   dzm iof			" clear input fd
   jms init
   jmp i init1
1:
   0111012			" I\n
2:

init2: 0			" start pass 2
   lac d1
"** 05-1-4.pdf page 34
   dac passno			" passno = 1
   sys write; 1f; 2f-1f		" output II\n
   jms init
   lac o17
   sys creat; 2f		" create a.out
   dac bfo
   sys open; 2f; 0		" open a.out for read too!
   dac bfi
   dzm bufadd
   jms copyz; buf; 64		" clear buffer
   jmp i init2
1:
   0111111;012000		" II\n
2:
   0141056;0157165;0164040;040040	" a.out

init: 0				" common init for both passes
   lac i 017777
   dac narg			" save arg count
   lac 017777
   tad d1
   dac fname			" point to first file name
   -1
   dac eofflg
   jms nextfil
   jms ioinit
   dzm savchr			" clear saved char
   dzm comflg			" clear line comment flag
   lac d1
   dac dot			" set "." type to one??
   dzm dot+1			" clear "." value??
   dzm cmflx			" set ".." type to zero??
   lac d4096			" set ".." value to 4K
   dac cmflx+1
   dzm fbflg			" clear f/b flag and array
   jms copyz; fbxp: fbx; 10
   jmp i init			" return

finis:
   lac iof			" close input file
   sys close
   jms bufwr			" flush output buffer
   lac bfi			" close a.out input fd
   sys close
   lac bfo			" close a.out output fd
   sys close
   -1
   tad namsiz
   cma				" get positive count of namelist entries
   rcl				" multiply by 6 to get words
   dac char
   rcl
   tad char
   dac 1f
   lac o17			" ?? creat mode bits??
   sys creat; n.out		" create "n.out"
   dac bfi
   sys write; namlst; 1: 0	" write name list
   lac bfi
   sys close			" close n.out
   sys exit

"** 05-1-4.pdf page 35
n.out:
   0156056;0157165;0164040;040040	" n.out

process: 0
   lac dot+1			" get "." value
   dac lvrand
   lac dot			" get "." type??
   sad d3			" three (user defined symbol?)?
   jmp proc4			"  yes, give "." error
   sza				" zero?
   jmp proc1			"  no
   -1				" yes (".." type)
   tad cmflx+1
   cma
   tad lvrand			" get "." - ".."
   dac lvrand

proc1:
   lac lvrand
   spa				" is relocated value positive?
   jmp proc4			"  no, give "." error
   and o17700			" mask to block
   sad bufadd			" same block as buffer?
   jmp proc2			"  yes, same block
   jms bufwr			" different block, write out current block
   jms copyz; buf; 64		" clear buffer
   lac lvrand
   and o17700
   dac bufadd
   dac 1f
   lac bfi
   sys seek; 1: 0; 0		" seek to current block from file
   spa
   jmp proc2
   lac bfi
   sys read; buf; 64

proc2:
   lac lvrand			" destination address
   and o77			" word within block
   jms betwen; dm1; maxsto	" inside buffer?
   dac maxsto			"  no, increment buffer size
   tad bufp			" add pointer to buffer
   dac lvrand			" save buffer pointer
   lac r			" get r type?
   sna				" non-zero ("." or label)?
   jmp proc3			"  no: zero (..)
   sad d3			" three (user label)?
   jmp proc5			"  yes
   lac cmflx+1			" get ".." value
   tad r+1			" add to r value
   dac r+1

proc3:
   lac r+1			" get r value
   dac i lvrand			" save in buffer
   jmp i process		" return

proc4:
   jms error; .>
   lac d1
   dac dot		" set '.' type to 1
"** 05-1-4.pdf page 36
   dzm dot+1		" clear dot value
   jmp skip

proc5:
   jms error; u>
   jmp proc3

bufwr: 0		" write current buffer to a.out file
   lac bfo
   sys seek; bufadd: 0; 0
   isz maxsto
   lac bfo
   sys write; bufp: buf; maxsto: -1
   -1
   dac maxsto
   jmp i bufwr

number: 0		" print decimal number: append to buffer at index 8
   dac 3f		" save number
   lac d1000
   dac 2f		" save divisor
1:
   lac 3f
   cll
   idiv; 2: 0
   dac 3f
   lacq
   tad o60		" add ascii '0'
   dac i 8		" save char
   lac 2b
   cll
   idiv; 10
   lacq
   dac 2b
   sza
   jmp 1b
   jmp i number
3: 0

	" get character from buffer (two characters per word)
	" call with:
	"   jms getsc; pointer_pointer
	" where pointer_pointer refers to a pointer to buffer
	" high bit in pointer indicates low char is next
getsc: 0
   lac i getsc			" get pointer pointer
   dac sctalp			" save
   isz getsc			" skip pointer pointer
   lac i sctalp			" fetch pointer
   dac sctal			" save
   add o400000			" toggle high bit, increment on wrap
   dac i sctalp			" save pointer back
   ral				" rotate high bit into link reg
   lac i sctal			" load word from buffer
   szl				" skip if link zero
   lrss 9			"  link set: get high char from word
   and o177			" strip to 7 bits
   jmp i getsc			" return

	" save characters: word after call is addr of pointer, -count pair
	" high bit in pointer used to indicate high/low
putsc: 0
   and o177			" strip character to 7 bits
   lmq				" save in MQ
   lac i putsc			" get address of pointer
   dac sctalp			" save
   isz putsc			" skip over pointer to pointer
"** 05-1-4.pdf page 37
   lac i sctalp			" get pointer
   dac sctal			" save
   add o400000			" toggle pointer sign bit, increment on wrap
   dac i sctalp			" save pointer
   sma cla			" skip if minus & clear AC
   jmp 1f			"  AC positive
   llss 27			" get char in high 9 bits, zero in low
   dac i sctal			" store word
   lrss 9			" shift char back down
   jmp i putsc			" return

1:
   lac i sctal			" load target word
   omq				" or in low char from MQ
   dac i sctal			" save word back
   lacq				" restore character
   jmp i putsc			" return

sctalp: 0
sctal: 0

	" test if between two values (low,high]
	" call with value in AC
	"    jms betwen; lowptr; highptr
	" skip returns if AC in range
	" AC returned unmodified
	" NOTE!
	" the test appears to be non-inclusive
	" on the low side, and inclusive on the high side

betwen: 0
   dac 2f			" save value to test
   lac i betwen			" load range start addr
   dac 3f			" save
   isz betwen			" increment return PC
   lac i 3f			" load range start
   cma				" complement
   tad 2f			" AC = AC - start - 1
   spa				" still positive?
   jmp 1f			"  no
   lac i betwen			" load range end addr
   dac 3f			" save
   isz betwen			" skip range high on return
   lac i 3f			" load range high
   cma
   tad d1			" negate AC (~AC + 1)
   tad 2f			" add test value
   spa				" if not positive, don't skip on return!!
1:
   isz betwen			" discard "high" (or skip return)!
   lac 2f			" restore AC
   jmp i betwen			" return
2: 0
3: 0

	" zero a block of memory
	" call with:
	"   jms copyz; ptr; count
copyz: 0
   -1
   tad i copyz			" get address-1
   dac 8			" store in first "index register"
   isz copyz			" skip over address
   lac i copyz			" load count
   cma				" get -count
   tad d1
   dac 2f			" save
   isz copyz			" skip over count
1:
   dzm i 8			" increment index, clear word
   isz 2f			" increment count, skip if done
   jmp 1b			"  not done, loop
   jmp i copyz			" done: return
"** 05-1-4.pdf page 38
2: 0

error: 0
   lac passno			" get pass number
   sza				" pass one?
   jmp 1f			"  no, pass two
   isz error			" pass one: skip error char
   jmp i error			" return
1:
   -1
   tad mesp			" get mes-1
   dac 8			" save as index
   lac i error			" get error
   dac i 8			" save in mess
   lac o40			" get space
   dac i 8			" save in mess
   lac rator			" get operator
   sad d5			" word break (semi, newline)?
   jmp 1f			"  yes
   lac savchr			" no, get saved char
   sad o12			" newline?
   jmp 1f			"  yes
   lac lineno			" get lineno
   jmp 2f
1:
   -1
   tad lineno			" get lineno -1
2:
   jms number			" convert line number to ascii
   lac o12			" get newline
   dac i 8			" append to mess
   -2
   tad mesp
   cma
   tad 8
   dac 1f
   lac d1
   sys write; mesp: mes; 1: 0
   isz error
   jmp i error

skip:
   lac rator			" get operator
   sad d5			" EOL?
   jmp assm1			"  yes, start from top
1:
   jms gchar			" loop until ';' or NL seen
   sad d5			" EOL?
   jmp assm1			"  yes, start from top
   jmp 1b

ioinit: 0
   jms copyz; iobuf; 64		" clear iobuf
   lac iof
   sys read; iobufp: iobuf; 64	" read from input
   sna				" EOF?
   jms nextfil			"  yes, skip to next file
   lac iobufp			" load iobuf pointer
   dac tal			" save
   -129				" get -bytecount-1
   dac talc			" save as count
"** 05-1-4.pdf page 39
   jmp i ioinit			" return

nextfil: 0			" advance to next file
   lac d1
   dac lineno			" reset lineno to 1
   lac iof			" load input fd
   sza				" zero?
   sys close			"  no: close
nf1:
   lac narg			" load arg count
   sad d4			" ==4? (done)
   skp				"  yes, skip
   jmp 1f			"   no
   dzm eofflg			" flag eof (set to zero)
   jmp i nextfil		" return
1:
   tad dm4			" subtract 4
   dac narg			" store narg
   lac fname			" get fname pointer
   tad d4			" subtract 4
   dac fname			" save fname
   sys open; fname: 0; 0	" open fname
   dac iof			" save fd
   sma				" open ok?
   lac passno			"  yes: load pass number
   sna				" no: open failed: skip or open ok, pass 2
   jmp nextfil i		"  pass 1, open OK, return.
   lac fname			" load filename pointer
   dac 1f			" save for write
   lac d1			" stdout
   sys write; 1: 0; 4		" output filename
   lac iof			" load fd
   sma				" open ok?
   jmp 1f			"  yes, continue
   lac d1
   sys write; emes; 2		" output "? \n"
   sys exit			" quit.
1:
   lac d1
   sys write; emes+1; 1		" output newline after filename
   jmp i nextfil		" return
emes:
   040077;012000		" question mark, space, newline

gchar: 0
   lac savchr			" load saved char
   dzm savchr			" clear saved char
   sza				" was there a saved char?
   jmp gch3			"  yes, process it
   lac eofflg			" no: get eof flag
   sza				" seen eof (zero if true)
   jmp 1f			"  no.
   lac o12			" yes: get NL
   jmp gch3			" process it
1:
   isz talc			" increment (negative count): is it zero?
   skp				"  non-zero: skip
   jms ioinit			"   count was zero: call ioinit
   jms getsc; tal		" fetch character
   sna				" is char non-zero?
   jmp gchar+1			"  no: char is zero, get another
"** 05-1-4.pdf page 40
   sad o177			" is char 0177
   jmp gchar+1			"  yes, ignore it
   sad o12			" is char newline?
   skp				"  yes
   jmp 1f			"   no
   dzm comflg			" saw newline: clear comflg
   isz lineno			" increment line number
1:
   sad o42			" is char '"'?
   dac comflg			"  yes, set comflg
   dac char			" save char
   lac comflg			" load comflg
   sza				" comflg clear?
   jmp gchar+1			"  no: ignore reset of line
   lac char			" get char

gch3:
   dac char			" save char in char
   jms betwen; d0; o200		" legal char?
   cla				"  no, clear
   tad lactab			" add to "lac labtab+1"
   dac .+1			" save as next instruction
   lac 0			" get character class in AC
   jmp i gchar			" return

gsymb: 0
   jms gchar			" get char
   dac rator			" save class
   tad jmpsw1			" add table base instruction
   dac 1f			" save for later
   lac char			" what was it?
   sad o74			" '<'??
   jmp lqot			"  yes: process as "left quote"
   dac namc			" no, save as namc
   jms gchar			" get another
   lac char			" what was it?
   sad o76			" '>'?
   jmp rqot			"  yes: process "right quote"
   dac savchr			" no: save as savchr
   lac namc			" restore first char
   dac char			" resave as "char"
1:
   jmp 0			" jmpsw1[0] + class
jmpsw1:				" indexed by character class
   jmp .+1			" base instruction (added to class)
   jmp i gsymb			" 0: ":" return
   jmp i gsymb			" 1: "=" return
   jmp i gsymb			" 2: "+" return
   jmp i gsymb			" 3: "-" return
   jmp gs1			" 4: comma, space, tab
   jmp i gsymb			" 5: EOL (semi, newline)
   jmp gs2			" 6: dot, star, letter
   jmp gs3			" 7: digits

badchr:				" here with bad char (class 8)
   jms error; g>		" error "g"
1:
   jms gchar			" discard until newline
   lac char
   sad o12
"** 05-1-4.pdf page 41
   skp
   jmp 1b
   dac savchr			" push newline back
   jmp gsymb+1			" restart gsymb

lqot:				" left quote (<)
   jms gchar			" get another char
   lac o40
   dac savchr			" put a space in savchr
   lac char			" get quoted character
   alss 9			" shift up 9 bits
   jmp 1f			" join with right quote

rqot:				" right quote (>)
   lac namc			" get previous(?) char
1:
   dac rand+1			" save value
   lac d7			" return as literal
   dac rator
   jmp i gsymb

gs1:				" here with space, tab, comma
   jms gchar			" get another char
   sad d4			" another space?
   jmp gs1			"  yes, loop
   lac char			" no, save for later
   dac savchr
   jmp i gsymb			" return

gs2:				" here with dot, star, letter
   lac namep			" load name buffer pointer
   dac tal1			" save in temp pointer
   -7				" load negative char count
   dac tal1c			" save as temp counter
   lac char			" restore char
   jms putsc; tal1		" save it in name buffer

gnam1:				" here to collect a name
   jms gchar
   jms betwen; d5; d8		" alphanumeric?
   jmp gnam3			"  no, done
   lac char
   jms putsc; tal1
   isz tal1c
   jmp gnam1

gnam2:				" here when 8 characters read, eat the rest
   jms gchar			" next char
   jms betwen; d5; d8		" alphanumeric?
   skp				"  no
   jmp gnam2			"   yes, loop
   lac char
   dac savchr			" push last char back
   jms lookup			" look up symbol
   jmp i gsymb			" return

gnam3:				" here before 8 characters
   lac char			" push last char back
   dac savchr
1:
   lac o40			" pad to 8 with spaces
"** 05-1-4.pdf page 42
   jms putsc; tal1
   isz tal1c
   jmp 1b
   jms lookup
   jmp i gsymb

gs3:				" here with digit
   dzm rand+1			" clear number
   lac char
   sad o60			" zero?
   jmp 1f			"  yes
   lac d10			" no: process as decimal
   jmp 2f
1:
   lac d8			" leading zero: process as octal
2:
   dac num2			" save radix

num1:
   lac rand+1			" get number
   cll				" clear link
   mul				" mutiply by radix
num2: 0				" (radix stored here)
   lacq				" get multiply result
   tad char			" add char
   tad dm48			" subtract '0'
   dac rand+1			" save
   jms gchar			" get another
   sad d7			" digit?
   jmp num1			"  yes: process
   lac char			" no: get it
   dac savchr			" push back
   lac rand+1			" get number
   jms betwen; dm1; d10		" between 0..9?
   jmp i gsymb			"  no, return
   dac name
   tad fbxp			" make index into "fbx" array
   dac name+1			" save for indirect fetch
   lac i name+1			" fetch fbx array entry
   dac name+1			" save fbx count in name
   lac savchr			" get break character
   sad o146			" was it 'f'?
   jmp 1f			"  yes
   sad o142			" 'b'?
   skp				"  yes
   jmp i gsymb			"   not f or b, return
   dzm name+1			" 'b': clear loaded fbx entry????
1:				" here with DIGITS[fb]
   dac name+2			" save f/b in third word of name
   dzm name+3			" clear last word of name
   lac d6			" class 6: alpha, dot star
   dac rator			" save as (ope)rator
   jms lookup			" lookup (create) symbol entry????
   dzm savchr			" clear saved char
   jmp i gsymb

	" symbol lookup/creation
	" tlookup doesn't create new entries???
tlookup: 0
      jmp 1f
lookup: 0
      dzm tlookup		" NOT a tlookup call
1:
"** 05-1-4.pdf page 43
   -1
   tad namlstp
   dac 8			" get namelist ptr in index reg
   lac namsiz
   dac namc			" negative namelist size in namc
lu1:
   lac i 8			" get first word of namelist entry
   sad name			" match name?
   jmp 1f			"  yes
   lac d5			" no, skip next 5 words
lu2:
   tad 8
   dac 8
   isz namc			" at end of list?
   jmp lu1			"  no, keep going
      lac tlookup		" yes, reached end
      sna			" was tlookup?
      jmp 2f			"  no, was lookup
      lac fnamep
      dac rand+1		" set rand+1 (value?) to fakename
      jmp i tlookup		" return
2:
   lac name			" make new entry
   dac i 8			" save word one of name
   lac 8
   dac rand+1
   lac name+1
   dac i 8			" save word two
   lac name+2
   dac i 8			" save word three
   lac name+3
   dac i 8			" save word four
   lac d3
   dac i 8			" set type(?) to three
   dzm i 8			" clear value??
   -1				" decrement namsiz
   tad namsiz
   dac namsiz
   jmp i lookup			" return
1:				" here when first word matched
   lac i 8
   sad name+1			" check second word
   jmp 1f			"  matched, keep going
   lac d4			" no match: skip ahead four words
   jmp lu2
1:
   lac i 8
   sad name+2			" does third word match?
   jmp 1f			"  yes, keep going
   lac d3			" no, skip ahead three words
   jmp lu2
1:
   lac i 8
   sad name+3			" final word match?
   jmp 1f			"  yes
   lac d2			" no, skip two words
   jmp lu2
1:				" name matched
   -3
   tad 8			" get next word minus three
   dac rand+1			" return as value
"** 05-1-4.pdf page 44
      lac tlookup
      sza
      jmp i tlookup
   jmp i lookup
namep: name

gpair: 0
   jms gsymb			" get a symbol
   lac rator			" get operator
   sad d4			" space tab or comma?
   jmp gpair+1			"  yes, get another
   jms betwen; dm1; d6		" plus, minus, comma, semi?
   jmp gp1			"  no
   dzm rand			" clear "rand"
   dzm rand+1
   jmp i gpair			" return
gp1:
   sad d7			" digit??
   lac d4			"  yes: switch to space??
   tad dm4			" subtract 4??
   dac rand			" save as operand??
   jms gsymb
   lac rator
   sad d4			" whitespace?
   jmp gp2			"  yes
   jms betwen; dm1; d6		" anything but digit?
   skp				"  no, a digit
   jmp i gpair			"   yes, return
   jms error; x>		" here with digit: give 'x' error
   jmp skip
gp2:				" here after whitespace
   jms gchar			" get next char
   jms betwen; d5; d8		" alphanumeric?
   jmp gp3			"  no
   lac char			" yes, push back
   dac savchr
   jmp i gpair
gp3:
   lac char			" push back break char
   dac savchr
   jms gsymb
   jmp i gpair

expr: 0
   jms grand
   -1
   dac srand
exp5:
   lac rand
   dac r
   lac rand+1
   dac r+1
exp1:
   lac rator
   jms betwen; d1; d5		" plus, minus, comma, space, tab or comma?
   jmp exp3			"  no
   dac orator
   jms gpair
   jms grand
   lac orator			" get operator back
   sad d4			" comma space or tab?
"** 05-1-4.pdf page 45
   jmp exp2			"  no
   jms oper; rand
   jmp exp1
exp2:
   jms pickup
   lac r
   dac srand
   lac r+1
   dac srand+1
   jmp exp5
exp3:
   sad d5
   jmp exp4
   jms error; x>
   jmp skip
exp4:
   jms pickup
   jmp i expr

pickup: 0
   lac srand
   spa
   jmp i pickup
   lac d4
   jms oper; srand
   jmp i pickup

grand: 0
   lac rand
   sad d2
   skp
   jmp i grand
   lac rand+1
   tad d4
   dac rand+1
   lac i rand+1
   dac rand
   isz rand+1
   lac i rand+1
   dac rand+1
   jmp i grand

	" called with
	"    jms oper; argument
oper: 0
   tad opsw
   dac oper1
   -1
   tad i oper		" pick up argument
   dac 8		" store as index
   isz oper		" skip argument
   lac r
   sad d3
   jmp oper2
   lac i 8
   sad d3
   jmp oper2
oper1:
   jmp 0
opsw:
   jmp .-1
   jmp oplus
   jmp ominus
"** 05-1-4.pdf page 46
   tad r
   dac r
   lac r+1
   lmq
   lac i 8
   omq
   jmp oret
oplus:
   tad r
   dac r
   lac r+1
   tad i 8
   jmp oret
ominus:
   cma
   tad d1
   tad r
   dac r
   -1
   tad i 8
   cma
   tad r+1
oret:
   dac r+1
   lac r
   jms betwen; dm1; d2
   skp
   jmp i oper
   jms error; r>
   lac d1
   dac r
   jmp i oper
oper2:
   dac r
   dzm r+1
   jmp i oper

d0: 0
d1: 1
d4096: 4096
d2: 2
d3: 3
d4: 4
d5: 5
d6: 6
d7: 7
d8: 8
o12: d10: 10
dm1: -1
o40: 040
o60: 060
dm48: -48
o400000: 0400000
o177: 0177
dm4: -4
o200: 0200
o42: 042
o142: 0142
o40040: 040040		" space, space
o56056: 056056		" ".."
o56040: 056040		" ". "
"** 05-1-4.pdf page 47
o146: 0146
o17777: 017777
d1000: 1000
o17: 017
o17700: 017700
o77: 077
o74: 074
o76: 076

namsiz: -2		" negative numberof namelist entries
namlstp: namlst		" pointer to namelist
fnamep: fakename	" pointer to fake namelist entry
lactab: lac .+1		" character (operator) class table (8 unless noted)
8;8;8;8;8;8;8;8
8;4;5;8;8;8;8;8		" TAB=4 NL=5
8;8;8;8;8;8;8;8
8;8;8;8;8;8;8;8
4;8;8;8;8;8;8;8		" SP=4
8;8;6;2;4;3;6;8		" *=6 +=2 ,=4 -=3 .=6
7;7;7;7;7;7;7;7		" digits=7
7;7;0;5;8;1;8;8		" :=0 ;=5 ==1
8;6;6;6;6;6;6;6		" A-Z=6
6;6;6;6;6;6;6;6
6;6;6;6;6;6;6;6
6;6;6;8;8;8;8;8
8;6;6;6;6;6;6;6		" a-z=6
6;6;6;6;6;6;6;6
6;6;6;6;6;6;6;6
6;6;6;8;8;8;8;8

fbflg: .=.+1		" f/b label flag
tal: .=.+1		" iobuf pointer
talc: .=.+1		" -bytecount-1
tal1: .=.+1		" namebuf pointer
tal1c: .=.+1		" -bytecount-1
narg: .=.+1		" argc
lvrand: .=.+1		" numeric constant, word address
eofflg: .=.+1		" 0 on EOF??
namc: .=.+1		" saved char, temporary
passno: .=.+1		" 0=pass1, 1=pass2
char: .=.+1		" current character
savchr: .=.+1		" pushed back char
comflg: .=.+1		" comment flag
rator: .=.+1		" (opo)rator (char type)
orator: .=.+1		" ?? (op)orator
rand: .=.+2		" ?? (ope)rand (type/address pair)
srand: .=.+2		" ?? another operand
r: .=.+2		" ?? yet another??
name: .=.+4		" buffer for accumulating names
buf: .=.+64		" a.out output buffer
iobuf: .=.+64		" input buffer
fbx: .=.+10		" forward/backward counters
mes: .=.+20		" (error) message buffer
iof: .=.+1		" source file fd
bfi: .=.+1		" a.out input fd
bfo: .=.+1		" a.out output fd
lineno: .=.+1		" source file line number

fakename: .=.+6		" dummy namelist entry returned by tlookup??
namlst:			" symbol table
.=.+4			" dot name
dot:			" dot type, value
.=.+6			" dot dot name
cmflx:			" dotdot type, value
		" namelist (symbol table) entries are 6 words.
		" four words of symbol (space padded) name
		" next word is type??
		"   0: initial dotdot type
		"   1: initial dot type (reset on error)
		"   3: set by "lookup" (user symbol)
		" last word is value??
