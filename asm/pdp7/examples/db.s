" db
	"** pages 2-21  of 07-13-19.pdf
	" Usage: db [ corefile [ namelist ] ]

   narg = ..+07777
   lac i narg
   sad d4
   jmp start			" no args
   lac narg
   tad d5			" pointer to first arg
   dac rcorep
   dac wcorep
   lac i narg
   sad d8			" one arg?
   jmp start			"  yes: start
   sad d12			" two args?
   skp				"  yes
   jmp error			"   no: error
   lac narg
   tad d9			" pointer to second arg
   dac nlnamep
   "
start:
   lac nlbufp
   cma
   tad o17777
   cll
   idiv
   6
   cll
   lacq
   mul
   6
   lacq
   dac namesize
   sys open; nlnamep: nlname; 0
   dac symindex
   sma
   jmp 1f
2:
   dzm nlcnt
   lac nlbufp
   dac nlsize
   jmp 3f
1:
   sys read; nlbuff; namesize:0
   spa
   jmp 2b
   dac nlcnt
   tad nlbufp
   dac nlsize
3:
   lac symindex
   sys close
   sys open
wcorep: corename; 1
   dac wcore
   sys open; rcorep: corename; 0
   dac rcore
   spa
   jmp error
   "
   lac o52012			" prompt: "*\n"
   jms wchar
   law dotdot
   dac nsearch
   jms nlsearch
   jmp mloop
   lac value
   dac relocval
   cma
   tad d1
   dac mrelocv
   dzm sysflag
mloop:
   -1
   dac nwords
ml1:
   jms rch
   dac nchar1
   sad o52
   skp
   jmp 1f
   dzm nchar1
   dac indflg
1:
   jms getexp
   lac errf
   sna
   jmp cmd
error:
   dzm indflg
   lac o40
   dac rator
   dzm errf
   lac d1
   sys write; errmes; 1
   jmp mloop
cmd:
   lacq
   sad o41			" '!'?
   jmp patch
   lac opfound
   sna
   jmp 1f
   lac comflg
   dzm comflg
   sza
   jmp 1f
   lac curval
   dac curdot
   lac curreloc
   dac dotrel
1:
   lacq
   sad o42			" '"'?
   jmp ascii
   sad o12			" NL?
   jmp newln
   sad o77			" '?'?
   jmp symbol
   sad o47			" "'"?
   jmp saddress
   sad o75			" '='?
   jmp address
   sad o57			" '/'?
   jmp octal
   sad o72			" ':'?
   jmp decimal
   sad o136			" '^'?
   skp
   jmp 1f
   dac sysflag
   jmp mloop
1:
   sad o45			" '%'?
   skp
   jmp 1f
   dzm sysflag
   jmp mloop
1:
   sad d1
   sys exit
   sad o54			" ','?
   skp
   jmp error
   " comma
   jms getexp
   lac errf
   sza
   jmp error
   law
   dac comflg
   lac curreloc
   sza
   jmp 1f
   lac value
   cma
   dac nwords
   jmp cmd
1:
   lac reldot
   sna
   jmp error
   lac curval
   cma
   tad curdot
   sma
   jmp error
   dac nwords
   jmp cmd
   "
saddress:
   lac curdot
   dac 9f
   dzm curdot
   lac reldot
   sza
   lac relocval
   tad 9f
   jms prsym
   lac o12
   jms wchar
   lac 9f
   dac curdot
   jmp mloop
9:0
   "
symbol:
   law prsym
   dac type
   jmp print
   "
octal:
   law proct
   dac type
   jmp print
   "
ascii:
   law prasc
   dac type
   jmp print
   "
decimal:
   law prdec
   dac type
   jmp print
   "
address:
   lac curdot
   jms octw
   5
   lac curreloc
   sza
   jmp 1f
   lac o12
   jms wchar
   jmp mloop
1:
   lac o162012			" r\n
   jms wchar
   jmp mloop
   "
print:
   lac sysflag
   sza
   jmp 2f
   lac reldot
   sza
   jmp 1f
   lac curdot
   dac addr
   jmp sprint
2:
   lac curdot
   dac addr
   jmp sprint
1:
   law inbuf
   dac addr
   lac curdot
   dac 1f
   lac rcore
   sys seek; 1:0; 0
   spa
   jmp error
   lac rcore
   sys read; inbuf; 64
   spa
   jmp error
   sna
   jmp error
   "
sprint:
   lac indflg
   sna
   jmp 2f
   dzm indflg
   lac i addr
   and o17777
   dac curdot
   dzm reldot
   tad mrelocv
   spa
   jmp print
   dac curdot
   lac d1
   dac reldot
   jmp print
   "
2:
   lac o40
   jms wchar
   lac addr
   dac 3f
   lac i addr
   jms i type
   lac 3f
   dac addr
   isz addr
   law prasc
   sad type
   jmp 1f
   lac nl
   jms wchar
1:
   isz nwords
   jmp 1f
   law prasc
   sad type
   skp
   jmp mloop
   lac nl
   jms wchar
   jmp mloop
1:
   isz curdot
   nop
   lac nwords
   spa
   jmp print
   jmp 2b
3:0
   "
prdec:     "temp
proct:0
   jms octw
   6
   jmp i proct
   "
patch:
   lac opfound
   sna
   jmp error
   lac curreloc
   sna
   jmp 1f
   lac curval
   tad relocval
   skp
1:
   lac curval
   dac inbuf
   lac sysflag
   sna
   jmp 1f
   lac inbuf
   dac i curdot
   jmp bump
1:
   lac reldot
   sna
   jmp error
   lac curdot
   dac 2f
   lac wcore
   spa
   jmp error
   sys seek; 2:0; 0
   spa
   jmp error
   lac wcore
   sys write; inbuf; 1
   spa
   jmp error
bump:
   -1
   dac nwords
   isz curdot
   jmp print
   "
newln:
   lac opfound
   sna
   jmp bump
   jmp print
   "
getexp:0
   dzm errf
   lac o40
   dac rator
   dzm curval
   dzm curreloc
   dzm reloc
   dzm value
   dzm opfound
xloop:
   jms rch
   lmq
   sad o044
   skp
   jmp 1f
   jms getspec
   jms oprand
   jmp xloop
1:
   tad om60
   spa
   jmp 1f
   tad om10
   sma
   jmp 1f
   lacq
   jms getnum
   jms oprand
   jmp xloop
1:
   lacq
   sad o56
   jmp 1f
   tad om141
   spa
   jmp 2f
   tad om32
   sma
   jmp 2f
1:
   lacq
   jms getsym
   jms oprand
   jmp xloop
2:
   lacq
   tad om101
   spa
   jmp 1f
   tad om32
   spa
   jmp 1b
1:
   lacq
   sad o74
   skp
   jmp 1f
   jms rch
   alss 9
   dac value
   dzm reloc
   jms oprand
   jmp xloop
1:
   sad o40
   jmp xloop
   sad o55 "-
   skp
   jmp 1f
2:
   lac o40
   sad rator
   skp
   jmp error
   lacq
   dac rator
   jmp xloop
1:
   sad o53 "+
   jmp 2b
   lac curreloc
   sna
   jmp 1f
   sad d1
   skp
   dac errf
1:
   lac o40
   sad rator
   jmp i getexp
   dac errf
   jmp i getexp
   "
getspec: 0
      jms rch
      sad o141			" 'a'?
      jmp spcac			"  get AC
      sad o161 "q		" 'q'?
      jmp spcmq			"  get MQ
      sad o151 "i		" 'i'
      jmp spcic			"  get index
      lmq
      tad om60			" subtract '0'
      spa			" positive?
      jmp 2f			"  no: not a digit
      tad om10			" yes: subtract 10
      sma			" negative?
      jmp 2f			"  no: not a digit
      lacq
      jms getnum
      jmp spcai
2:
      law
      dac errf
      jmp getspec i
spcac:
      cla			" offset of AC in userdata
      jmp 1f
spcmq:
      lac d1			" offset of MQ in userdata
      jmp 1f
spcic:
      lac d10			" offset of PC in userdata
      jmp 1f
spcai:
      lac value			" get index number
      tad dm6			" get offset in userdata

1:
      tad o10000		" add offset in core file of userdata
      dac value
      lac d1
      dac reloc
      jmp i getspec
   "
getsym:0
   lmq
   law symbuf
   dac symbufp
   dzm symbuf
   -8
   dac symcnt
   dzm nchar1
   lac nopcom
   dac skipt
   skp
storech:
   lmq
   lac i symbufp
   and o177000
   sna
   jmp 1f
   omq
   dac i symbufp
   isz symbufp
   dzm i symbufp
   jmp 2f
1:
   llss 27
   dac i symbufp
   "
2:
   isz symcnt
   skp
   jmp endsym
skipt:
   nop " or jmp fill
   jms rch
   sad o76
   skp
   jmp 1f
   lac symbuf
   lrss 9
   and o177
   dac value
   dzm reloc
   jmp i getsym
1:
   sad o56			" '.'
   jmp storech
   dac nchar1
   tad om60
   spa
   jmp fill			" not a digit
   tad m10
   spa
   jmp 2f			" is a digit
   lac nchar1
   tad om141
   spa
   jmp 1f			" not a lc letter
   tad om32
   spa
   jmp 2f			" lc letter
1:
   lac nchar1
   tad om101
   spa
   jmp fill			" not a uc letter
   tad om32
   sma
   jmp fill			" not a uc letter
2:				" here with letter or digit
   lac nchar1
   dzm nchar1
   jmp storech
   "
fill:
   lac trafill
   dac skipt
   lac o40
   jmp storech
   "
endsym:
   lac symbuf
   sad o56040			" '. '?
   jmp dotsym			"  yes
   law symbuf
   dac nsearch
   jms nlsearch
   jmp undef
   dzm nsearch
   jmp i getsym
   "
dotsym:
   lac curdot
   dac value
   lac dotrel
   dac reloc
   jmp i getsym
   "
undef:
   dzm nsearch
   law
   dac errf
   jmp i getsym
   "
getnum:0
   dzm reloc
   dzm value
num1:
   tad om60
   lmq
   lac value
   alss 3
   omq
   dac value
   jms rch
   dac nchar1
   sad o162
   jmp nrel
   tad om60
   spa
   jmp i getnum
   tad om10
   sma
   jmp i getnum
   lac nchar1
   dzm nchar1
   jmp num1
nrel:
   dzm nchar1
   lac d1
   dac reloc
   jmp i getnum
   "
oprand:0
   lac rator
   sad o53
   jmp opplus
   sad o40
   jmp opor
   sad o55
   jmp opminus
opplus:
   lac value
   tad curval
   dac curval
1:
   lac curreloc
   tad reloc
   dac curreloc
   jmp retop
   "
opor:
   lac value
   lmq
   lac curval
   omq
   dac curval
   jmp 1b
   "
opminus:
   lac curval
   cma
   tad value
   cma
   dac curval
   lac reloc
   cma
   tad d1
   tad curreloc
   dac curreloc
retop:
   lac o40
   dac rator
   dac opfound
   jmp i oprand
   "
prasc:0
   jms wchar
   jmp i prasc
   "
prsym:0				" print symbolic instruction
   dac word
   dzm relflg
   dzm relocflg
   dzm nsearch
   and o760000
   sad o760000
   jmp plaw
   sad o20000			" cal i == "sys"
   jmp pcal
   and o740000
   sad o640000
   jmp peae
   sad o740000
   jmp popr
   sad o700000
   jmp piot
   sna
   jmp poct
   jms nlsearch
   jmp poct
   jms wrname
   lac o40
   jms wchar
   lac word
   and o20000			" check indirect bit
   sna
   jmp 1f			" no indirect
   lac o151040			" 'i '
   jms wchar
   lac word
   xor o20000			" clear indirect bit
   dac word
1:
symadr:
   lac d1
   dac relflg
   dac relocflg
   lac word
   and o17777
   tad mrelocv
   sma
   jmp 1f
   tad relocval
   dzm relocflg
1:
pradr:
   dac addr
   jms nlsearch
   jmp octala
pr1:
   dzm relflg
   jms wrname
   lac value
   sad addr
   jmp i prsym
   cma
   tad d1
   tad addr
   sma
   jmp 1f
   cma
   tad d1
   dac addr
   lac o55
   jms wchar
   jmp 2f
1:
   dac addr
   lac o53
   jms wchar
2:
   lac addr
   jms octw
   1
   jmp i prsym
   "
plaw:				" print LAW instruction
   lac d1
   dac relocflg
   lac word
   and o17777
   tad mrelocv
   sma
   jmp 1f
   tad relocval
   dzm relocflg
1:
   dac addr
   law
   dac relflg
   lac addr
   jms nlsearch
   jmp poct
   dac symindex
   law laws
   jms wrname
   lac o40
   jms wchar
3:
   lac symindex
   jmp pr1
2:0
9:0
   "
peae:				" print EAE instruction
   lac word
   jms nlsearch
   jmp 1f
   jms wrname
   jmp i prsym
1:
   lac word
   and o777700
   jms nlsearch
   jmp 1f
   jms wrname
   lac o40
   jms wchar
   lac word
   and o77
   jms octw
   1
   jmp i prsym
1:
   law eaes
   dac addr
   jmp nfnd
   "
popr:				" print OPR instruction
   law oprs
   jmp 1f
piot:				" print IOT instruction
   law iots
1:
   dac addr
   lac word
   jms nlsearch
   jmp 1f
   jms wrname
   jmp i prsym
1:
nfnd:
   lac addr
   jms wrname
octala:
   dzm relflg
   lac o40
   jms wchar
   lac word
   and o37777
   lmq
   lac relocflg
   sna
   jmp 1f
   lacq
   tad mrelocv
   lmq
1:
   lacq
   jms octw
   1
   lac relocflg
   sna
   jmp i prsym
   lac o162
   jms wchar
   dzm relocflg
   jmp i prsym
   "
poct:
   lac word
   jms octw
   6
   jmp i prsym
   "
pcal:
   lac word
   sna
   jmp poct
   and o17777
   jms nlsearch
   jmp 1f
   dac addr
   law syss
   jms wrname
   lac o40
   jms wchar
   lac addr
   jms wrname
   jmp i prsym
   "
1:
   lac word
   and o20000
   sza
   jmp poct
   jmp symadr
   "
wrname:0
   tad dm1
   dac 10
   -4
   dac 3f
1:
   lac i 10
   dac 2f
   lmq
   cla
   llss 9
   sad o40
   jmp i wrname
   jms wchar
   lac 2f
   and o177
   sad o40
   jmp i wrname
   jms wchar
   isz 3f
   jmp 1b
   jmp i wrname
2:0
3:0
   "
nlsearch:0
   dac match
   lac brack
   dac best
   dzm minp
1:
   lac nlbufp
   tad dm6
   dac cnlp
nloop:
   lac cnlp
   tad d6
   dac cnlp
   lmq
   cma
   tad nlsize
   spa
   jmp nlend
   lac nsearch
   sza
   jmp testn
   lacq
   tad d3
   dac np
   lac i np
   sna
   jmp nloop
   isz np
   lac i np
   dac treloc
   sad relocflg
   skp
   jmp nloop
   isz np
   lac i np
   dac tvalue
   sad match
   jmp nlok
   lac relocflg
   sna
   jmp nloop
   lac relflg
   sna
   jmp nloop
   -1
   tad tvalue
   cma
   tad match
   spa
   jmp nloop
   dac 2f
   tad mbrack
   sma
   jmp nloop
   lac best
   cma
   tad d1
   tad 2f
   sma
   jmp nloop
   lac 2f
   dac best
   lac tvalue
   dac value
   lac treloc
   dac reloc
   lac cnlp
   dac minp
   jmp nloop
   "
2:0
   "
testn:
   lacq
   dac minp
   -4
   dac value
   lac match
   dac inbuf
1:
   lac i minp
   sad i inbuf
   skp
   jmp nloop
   isz minp
   isz inbuf
   isz value
   jmp 1b
   lac i minp
   dac treloc
   isz minp
   lac i minp
   dac tvalue
   jmp nlok
   "
nlend:
   lac relflg
   sna
   jmp i nlsearch
   lac dotrel
   sad relocflg
   skp
   jmp 1f
   -1
   tad curdot
   cma
   tad match
   spa
   jmp 2f
   cma
   tad d1
2:
   tad brack
   spa
   jmp 1f
   tad mbrack
   tad best
   spa
   jmp 1f
   lac curdot
   dac value
   lac dotrel
   dac reloc
   law o56040
   dac minp
1:
   lac minp
   sza
   isz nlsearch
   jmp i nlsearch
nlok:
   lac tvalue
   dac value
   lac treloc
   dac reloc
   lac cnlp
   isz nlsearch
   jmp i nlsearch
   "
nlerr:
   law
   dac errf
   jmp i nlsearch
   "
rch:0
   lac nchar1
   dzm nchar1
   sza
   jmp i rch
1:
   lac nchar
   dzm nchar
   sza
   jmp i rch
   cla
   sys read; char; 1
   lac char
   and o177
   dac nchar
   lac char
   lrss 9
   sna
   jmp 1b
   jmp i rch
   "
wchar:0
   dac char
   lac d1
   sys write; char; 1
   jmp i wchar
   "
octw: 0
   isz octw
   lmq
   cla cll
   llss 3
   alss 6
   llss 3
   tad o60060
   dac obuf
   cla
   llss 3
   alss 6
   llss 3
   tad o60060
   dac obuf+1
   cla
   llss 3
   alss 6
   llss 3
   tad o60060
   dac obuf+2
   lac d1
   sys write; obuf; 3
   jmp i octw
m10: -10
   "
   "
o54:054
d6:6
o52012:052012			" "*\n"
d5:5
d9:9
d12:12
d8:8
d3:3
o177:0177
o136: 0136
o45: 045
sysflag: 0
char:0
d2:2
o162012:0162012			" r\n
mrelocv:-010000
relocval:010000
nwords:0
errf:0
rator:0
d1:1
errmes:077012			" ?\n
o12:012
curval:0
curreloc:0
curdot:0
reldot:
dotrel:0
value:0
reloc:0
o77:077
o57:057
o72:072
o50:050
type:proct
o162:0162
nl:012
om100:-0100
"d2:2				" duplicate!
symbuf: .=.+5
inbuf:.=.+64
o100:0100
opfound:0
wcore:0
symindex:0
rcore:0
o56:056
om60:-060
om10:-010
"o56:056			" duplicate!
om141:-0141
o141: 0141
o044: 044
o151: 0151
o161: 0161
om101: -0101
om32:-032
o40:040
o55:055
o53:053
symbufp:0
symcnt:0
nopcom:nop
dm6: -6
d10: 10
trafill: jmp fill
nchar1:0
nchar:0
o177000:0177000
o56040:056040				" ". "
nsearch:0
word:0
relflg:0
relocflg:0
o740000:0740000
o640000:0640000
o700000:0700000
o17777:017777
o20000:020000
o10000: 010000
o151040:0151040				" "i "
eaes:0145141;0145040			" "eae "
laws: 0154141;0167040			" "law "
oprs:0157160;0162040			" "opr "
iots:0151157;0164040			" "iot "
syss:0163171;0163040			" "sys "
corename:0143157;0162145;040040;040040	" "core"
nlname:0156056;0157165;0164040;040040	" "n.out"
dotdot: <..>; 040040; 040040; 040040	" (relocation base symbol)
addr:0
o37777:037777
dm1:-1
match:0
rwdflg:0
nlbufp:nlbuff
nlsize:0
dm6:-6
cnlp:0
o377777:0377777
minp:0
d4:4
np:0
nlcnt:0
obuf:.=.+3
o60060:060060
o75:075
best: 0
o60000:060000
comflg:0
nlbufp:nlbuff
brack: 30
mbrack: -30
o777700:0777700
o41:041
o42:042
o760000:0760000
o40000:040000
tvalue: 0
treloc: 0
o151:0151
o47: 047
o52:052
indflg: 0
o74:074
o76:076
nlbufp: nlbuff
   nlbuff = .
