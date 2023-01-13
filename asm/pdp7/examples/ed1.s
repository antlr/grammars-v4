"** 08-rest.pdf page 8
"[handwritten page number top right of scan - 1]
" ed1
   lac d1
   sys write; 1f; 3		" say hello
   lac o17
   sys creat; tname		" create e.tmp for write
   spa
   sys save			" dump core on error!
   dac sfo
   sys open; tname; 0		" open e.tmp for read
   spa
   sys save			" dump core on error!
   dac sfi
   -1
   tad lnodp
   dac zermp
   tad d1
   dac zerop
   dac dot
   tad d1
   dac eofp
   dzm i eofp
   dzm i zerop
   dzm lastre
   dzm fbuf
   lac d1
   dac dskadr
   dac wrflg
   dzm diskin
 "o------------> [scan markup]
   jmp advanc
1:
   <ed>; <it>; 012
advanc:
   jms rline
   lac linep
   dac tal
   dzm adrflg			" clear address flag
   jms addres			" get address?
   jmp comand			"  no: check if command
   -1
   dac adrflg			" set address flag
   lac addr
   dac addr1			" set both addr1 & 2
   dac addr2
1:
   lac char
   sad o54			" comma?
   jmp 2f
   sad o73			" semi?
   skp
   jmp chkwrp
   lac addr
   dac dot
2:				" here for address range
   jms addres
   jmp error
   lac addr2
   dac addr1
   lac addr
   dac addr2
   jmp 1b
"** 08-rest.pdf page 9
"[handwritten page number top right of scan - 2]
chkwrp:
   -1
   tad addr1
   jms betwen; d1; addr2
   jmp error

comand:
   lac char
   sad o141			" a?
   jmp ca
   sad o143			" c?
   jmp cc
   sad o144			" d?
   jmp cd
   sad o160			" p?
   jmp cp
   sad o161			" q?
   jmp cq
   sad o162			" r?
   jmp cr
   sad o163			" s?
   jmp cs
   sad o167			" w?
   jmp cw
   sad o12			" newline?
   jmp cnl
   sad o75			" =?
   jmp ceq
   jmp error
ca:				" a(ppend)
   jms newline
   jms setfl
   lac addr2
   dac dot
ca1:
   jms rline
   lac line
   sad o56012			" .NL?
   jmp advanc
   jms append
   jmp ca1

cc: cd:				" c(change) and d(elete)
   jms newline
   jms setdd
   lac addr1
   sad zerop
   jmp error
   dac dot
   tad dm1
   dac 9
   lac addr2
   dac 8
2:
   lac i 8
   dac i 9
   sza
   jmp 2b
   lac 9
"** 08-rest.pdf page 10
"[handwritten page number top right of scan - 3]
   dac eofp
   lac char
   sad o144			" was delete?
   jmp advanc			"  yes
   -1
   tad dot
   dac dot
   jmp ca1

cp:				" p(rint)
   jms newline
cp1:
   jms setdd
   lac addr1
   sad zerop
   jmp error
1:
   lac addr1
   dac dot
   lac i addr1
   jms gline
   dac 2f
   lac d1
   sys write; line; 2: 0
   lac addr1
   sad addr1
   jmp advanc
   tad d1
   dac addr1
   jmp 1b

cq:				" q(uit)
   jms newline
   lac adrflg
   sza
   jmp error
   sys exit

cr:				" r(ead)
   jms setfl
   lac addr2
   dac dot
   jms rname
 "------------>  [scan markup]
   sys open; fbuf; 0
   spa
   jmp error
   dac tfi
   lac linep
   dac tal
   dzm num
1:
   lac tfi
   sys read; tbuf; 64
   sza
   jmp 2f
   lac tfi
   sys close
   jms number
   jmp advanc
2:
   cma
"** 08-rest.pdf page 11
"[handwritten page number top right of scan - 4]
   tad d1
   rcl
   dac c1
   lac tbufp
   dac tal1
2:
   jms getsc; tal1
   sna
   jmp 3f
   jms putsc; tal
   isz num
   sad o12
   skp
   jmp 3f
   lac tal
   add o400000
   and o17777
   cma
   tad linep
   cma
   dac linsiz
   jms append
   lac linep
   dac tal
3:
   isz c1
   jmp 2b
   jmp 1b
cw:				" w(rite)
   jms setfl
   lac i addr1
   sna
   jmp error
   jms rname
   lac o17
   sys creat; fbuf
   spa
   jmp error
   dac tfi
   -128
   dac c2
   lac tbufp
   dac tal1
   dzm num
1:
   lac i addr1
   jms gline
   rcl
   cma
   tad d1
   dac c1
   lac linep
   dac tal
2:
   jms getsc; tal
   sna
   jmp 3f
   isz num
   jms putsc; tal1
   isz c2
"** 08-rest.pdf page 12
"[handwritten page number top right of scan - 5]
   jmp 3f
   lac tfi
   sys write; tbuf; 64
   -128
   dac c2
   lac tbufp
   dac tal1
3:
   isz c1
   jmp 2b
   lac addr1
   sad addr2
   jmp 1f
   isz addr1
   jmp 1b
1:
   lac tal1
   sma cla
   jmp 1f
   jms putsc; tal1
1:
   -1
   tad tbufp
   cma
   tad tal1
   dac 1f
   lac tfi
   sys write; tbuf; 1: 0
   lac tfi
   sys close
   jms number
   jmp advanc

cnl:				" newline
   lac adrflg
   sna
   jmp 1f
   lac addr2
   dac addr1
   jmp cp1
1:
   lac dot
   tad d1
   sad eofp
   jmp error
   dac dot
   jmp cp1

ceq:				" = command
   jms newline
   jms setfl
   lac addr2
   dac dot
   cma
   tad zerop
   cma
   dac num
   jms number
   jmp advanc

setdd: 0
"** 08-rest.pdf page 13
"[handwritten page number top right of scan - 6]
   lac adrflg
   sza
   jmp i setdd
   lac dot
   dac addr1
   dac addr2
   jmp i setdd

setfl: 0
   lac adrflg
   sza
   jmp i setfl
   lac zerop
   tad d1
   dac addr1
   -1
   tad eofp
   dac addr2
   jmp i setfl

newline: 0
   jms getsc; tal		" get input char
   sad o12			" newline?
   jmp i newline
   jmp error			" no, complain

addres: 0
   dzm minflg "..) [stray scan mark?]
   -1
   dac addr
ad1:
   jms getsc; tal
ad2:
   jms betwen; d47; d58		" digit?
   skp
   jmp numb
   sad o40 "[o40 circled in scan]	" space?
   jmp ad1 "[hand drawn check mark follows operand in scan]
   sad o11			" tab?
   jmp ad1 "[hand drawn check mark follows operand in scan]
           "[check mark underlined in scan]
   sad o55			" -?
   jmp amin "[hand drawn check mark follows operand in scan]
   sad o56			" .?
   jmp adot "[hand drawn check mark follows operand in scan]
   sad o53			" +?
   jmp ad1 "[hand drawn check mark follows operand in scan]
   sad o44			" $?
   jmp adol "[hand drawn check mark follows operand in scan]
   sad o57			" /?
   jmp fsrch "[hand drawn check mark follows operand in scan]
   sad o77			" '?'?
   jmp bsrch "[hand drawn check mark follows operand in scan]
   dac char
   lac minflg
   sza
   jmp error
   lac addr
   sma
   isz addres
   jmp i addres
"** 08-rest.pdf page 14
"[handwritten page number top right of scan - 7]
adot:
   lac minflg
   sza
   jmp error
   lac addr
   sma
   jmp error
   lac dot
   dac addr
   jmp ad1

adol:
   lac minflg
   sza
   jmp error
   lac addr
   sma
   jmp error
   -1
   tad eofp
   dac addr
   jmp ad1

amin:
   -1
   dac minflg
   jmp ad1

numb:
   dac char
   sad o60			" leading zero?
   jmp 1f			"  yes: octal
   lac d10			" no: decimal
   jmp 2f
1:
   lac d8			" here for octal
2:
   dac 2f			" save radix for mul
   dzm num
1:
   lac num
   cll; mul; 2: 0		" multiply by radix
   lacq
   tad char			" add digit
   tad dm48			" subtract ASCII zero
   dac num			" save
   jms getsc; tal		" get another char
   dac char			" save
   jms betwen; d47; d58		" a digit?
   skp				"  no: done
   jmp 1b			" yes: process
   lac minflg			" minus flag
   sna				" set?
   jmp 1f			"  no
   -1				" yes: negate
   tad num
   cma
   dac num
   dzm minflg			" clear minus flag
1:
   lac addr
"** 08-rest.pdf page 15
"[handwritten page number top right of scan - 8]
   spa
   lac zerop
   tad num
   dac addr
   jms betwen; zermp; eofp
   jmp error
   lac char
   jmp ad2

number: 0		" format "num" in decimal to "tbuf"
   lac d100000
   dac n1
   law tbuf-1
   dac 8
n0:
   lac num
   cll; idiv; n1: 0
   dac num
   lacq
   tad d48
   dac i 8
   lac n1
   cll; idiv; 10
   lacq
   dac n1
   sza
   jmp n0
   lac o12
   dac i 8
   law tbuf-1
   dac 8
   dac 9
   -5
   dac n1
n2:
   lac i 8
   sad d48
   skp
   jmp n3
   dzm i 9
   isz n1
   jmp n2
n3:
   lac d1
   sys write; tbuf; 7
   jmp i number

rname: 0
   lac fbufp
   dac tal1
   -8
   dac c1
1:
   jms getsc; tal
   sad o40
   jmp 1b
   sad o12
   jmp 1f
   jms putsc; tal1
   isz c1
   jmp 1b
"** 08-rest.pdf page 16
"[handwritten page number top right of scan - 9]
   jmp i rname
1:
   lac tal1
   sad fbufp
   skp
   jmp 1f
   lac fbuf
   sna
   jmp error
   jmp i rname
1:
   lac o40
   jms putsc; tal1
   isz c1
   jmp 1b
   jmp i rname

gline: 0
   dac glint1
   jms getdsk
   lac glint1 " [these 6 lines were surrounded by a box
   and o1777  " that was Xed out with an arrow pointing to it]:
   tad dskbfp " --
   dac ital   "|\/|
   lac linep  "|/\|<---
   dac otal   " --
1:
   lac ital
   sad edskbfp
   skp
   jmp 2f
   lac diskin
   tad d1024
   jms getdsk
   lac dskbfp
   dac ital
2:
   jms getsc; ital
   jms putsc; otal
   sad o12
   skp
   jmp 1b
   lac otal
   sma
   jmp 1f
   cla
   jms putsc; otal
1:
   lac linpm1
   cma
   tad otal
   jmp i gline

rline: 0			" read line from stdin
   lac linep
   dac tal

1:
   cla
   sys read; char; 1		" read a char (or two)
   lac char
"** 08-rest.pdf page 17
"[handwritten page number top right of scan - 10]
   lrss 9
   jms esc			" process high char
   lac char
   and o777
   jms esc			" process low char
   jmp 1b

esc: 0				" edit/save char
   sna
   jmp i esc			" skip NUL
   jms putsc; tal
   sad o12			" newline?
   jmp 2f
   sad o100			" @ (kill)?
   jmp 1f
   sad o43			" # (erase)?
   skp
   jmp i esc
   -1				" erase
   tad tal			" decrement pointer
   dac tal
   and o17777			" mask to addr
   sad linpm1			" before start?
   jmp 1f			"  yes: reset
   jmp i esc

1:				" saw @ (kill) reset pointer
   lac linep
   dac tal
   jmp i esc

2:				" here with newline
   lac tal
   sma cla
   jmp 1f
   jms putsc; tal
1:
   -1
   tad linep
   cma
   tad tal
   dac linsiz
   jmp i rline			" return from rline

getsc: 0			" call: jms getsc; pbufp
   lac i getsc			" fetch buffer pointer addr from after call
   dac sctalp
   isz getsc			" skip buffer pointer pointer
   lac i sctalp			" fetch buffer pointer
   dac sctal
   add o400000			" flip high bit (carry wraps around)
   dac i sctalp			" save pointer back
   ral				" shift high bit into link
   lac i sctal			" fetch word from buffer
   szl				" link set?
   lrss 9			"  yes: shift high char down
   and o777			" mask to character
   jmp i getsc

putsc: 0			" call: jms putsc; pbufp
   and o777			" mask to single 9-bit character
"** 08-rest.pdf page 18
"[handwritten page number top right of scan - 11]
   lmq				" put in MQ
   lac i putsc			" fetch buffer pointer pointer
   dac sctalp
   isz putsc			" skip over pointer pointer
   lac i sctalp			" get pointer
   dac sctal			" save it for later
   add o400000			" flip high bit, wrap carry
   dac i sctalp			" save back as buffer pointer pointer
   sma cla			" high bit set?
   jmp 1f			"  no
   llss 27			" yes: get MQ in high half of AC
   dac i sctal			" save to buffer
   lrss 9			" shift char back down
   jmp i putsc			" return

1:
   lac i sctal			" get word from buffer
   and o777000			" mask to high character
   omq				" OR in new char
   dac i sctal			" save word
   lacq				" restore char
   jmp i putsc			" return

append: 0
   -1
   tad eofp
   dac 8
   cma
   tad dot
   dac apt1
1:
   lac i 8
   dac i 8
   -3
   tad 8
   dac 8
   isz apt1
   jmp 1b
   isz eofp
   dzm i eofp
   isz dot
   jms addline
   jmp i append

addline: 0
   lac dskadr
   dac i dot
   jms getdsk "[line crossed out - scan markup]
   -1
   tad linsiz
   cma
   dac apt1
   law line-1
   dac 8
   lac dskadr "[line crossed out - scan markup]
   and o1777  "[line crossed out - scan markup]
   tad dskbfp "[line crossed out - scan markup]
   dac otal   "[line crossed out - scan markup]
   lac dskadr "[line crossed out - scan markup]
   tad linsiz "[line crossed out - scan markup]
   dac dskadr "[line crossed out - scan markup]
"** 08-rest.pdf page 19
"[handwritten page number top right of scan - 12]
1:
   lac otal    " [these 9 lines were surrounded by a box
   sad edskbfp " that was Xed out]:
   skp         " --
   jmp 2f      "|\/|
   lac diskin  "|/\|
   tad d1024   " --
   jms getdsk  "
   lac dskbfp  "
   dac otal    "
2: "[line crossed out - scan markup]
   lac i 8
   dac i otal "[line crossed out and a note that looks like *jms prtwrd*]
   isz otal
   dzm wrflg "[line crossed out - scan markup]
   isz apt1
   jmp 1b
   jmp i addline


getdsk: 0                  "[the entire getdsk procedure was surrounded
   and o776000             " by a box that was Xed out]:
   sad diskin              " --
   jmp i getdsk            "|\/|
   dac 2f                  "|/\|
   lac wrflg               " --
   sza                     "
   jmp 3f                  "
   lac diskin              "
   dac 1f                  "
   lac sfo                 "
   sys seek; 1: 0; 0       "
   lac sfo                 "
   sys write; dskbuf; 1024 "
   lac d1                  "
   dac wrflg               "
3:                         "
   lac 2f                  "
   dac diskin              "
   lac sfi                 "
   sys seek; 2: 0; 0       "
   spa                     "
   jmp i getdsk            "
   lac sfi                 "
   sys read; dskbuf; 1024  "
   jmp i getdsk            "

betwen: 0
   dac bett1
   lac i betwen
   dac bett2
   isz betwen
   lac i bett2
   cma
   tad bett1
   spa
   jmp 1f
   lac i betwen
   dac bett2
   isz betwen
   -1
   tad i bett2
"** 08-rest.pdf page 20
"[handwritten page number top right of scan - 13]
   cma
   tad bett1
   spa
1:
   isz betwen
   lac bett1
   jmp i betwen

error:
   lac d1
   sys write; 1f; 1
   jmp advanc
1:
   077012
