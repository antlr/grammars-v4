" bc

   jmp start
rinit:
   jms initio
   jmp .+1 i
initio: 0
   lac inter-1
   dac fetch
   jmp rinit
   jms inter
inter: 0
   las
   and o17
   sza
   jms trace
   lac pc i
   dac instr
   lac pc
   and o10000
   sna
   jmp badpc
   lac sp
   and o17700
   sad o17700
   jmp badsp
   and o10000
   sna
   jmp badsp
   lac instr
   sad insasg
   skp
   jmp inter i
   -4
   tad sp
   dac t1
   lac t1 i
   and o10000
   sna
   jmp badasgn
   lac instr
   jmp inter i

trace: 0
   and d1
   sza
   jms dtrace
   las
   and d2
   sza
   jms ddisp
   las
   and d4
   sza
   jms histog
   las
   and d8
   sza
   jmp stop
   jmp trace i

dtrace: 0
   lac pc
   dac 8
   lac 8 i
   dac instr
   lac 8
   jms octal; -4
   law 040
   jms putc
   lac instr
   cll; lrs 14
   tad lacop
   dac .+1
   lac ..
   jms putc
   law 040
   jms putc
   lac instr
   jms octal; -4
   
   lac instr
   sad inslitr
   skp
   jmp 1f
   law 040
   jms putc
   lac 8 i
   jms octal; -6
1:
   law 012
   jms putc
   jms flush
   jmp dtrace i

ddisp: 0
   jms dspinit
   lac dspbp
   dac 8
   lac dp
   sad olddp
   skp
   jms dspblk
   lac pc
   jms dspnt
   lac sp
   jms dspnt
   lac lastv
   jms dspnt
   -1
   dac B i
   lac pbs i
   sza
   jmp .-2
   jmp ddisp i

dspblk: 0
   lac dspbuf
   dac 8
   lac dp
   dac t1
   dzm t2
1:
   lac t1
   sna
   jmp 1f
   lac o216000 " dx -20
   dac 8 i
   lac t2
   tad o20
   dac t2
   lac t1
   tad d1
   dac t3
   lac t3 i
   jms dspnt
   lac t1 i
   dac t1
   jmp 1b
1:
   lac o160020 " sx 20
   tad t2
   dac dspbuf i
   dac 8 i
   lac 8
   dac dspbp
   jmp dspblk i

dspnt: 0
   and o7777
   lrss 2
   xor o164000 " sy 0
   dac 8 i
   lac o17010
   dac 8 i
   jmp dspnt i

dspinit: 0
   -1
   tad dspinit
   dac dspinit
   -300
   tad lastv
   dac lastv
   dac dspbuf
   -1
   dac dspinit i
   dac dspbuf i
   dzm olddp
   lac dspbuf
   sys capt
   law 13
   sys sysloc
   dac pbs
   jmp dspinit i

histog: 0
   jms hisinit
   lac pc
   lrs 6
   and o77
   tad histbuf
   dac t1
   isz t1 i
   jmp histog i
   jmp .

hisinit: 0
   -1
   tad hisinit
   dac hisinit
   -1
   dac hisinit i
   -64
   dac t1
   tad lastv
   dac lastv
   dac histbuf
   tad dm1
   dac 8
1:
   dsm 8 i
   isz t1
   jmp 1b
   jmp hisinit i

histbuf: 0
olddp: 0
dspbuf: 0
dspbp: 0
instr: 0
obs: 0
inslitr: n 5
insasg: b 1
o17: 017
d8: 8
o77: 077
o10000: 010000
d5: 5
o60: 060
o7777: 07777
o216000: 0216000
o160020: 0160020
o20: 020
o164000: 0164000
o17010: 017010
o17700: 017700
d2: 2

lacop: lac .
   a>;b>;c>;f>;n>;s>;t>;u>;x>;y>

badpc:
   jms flush
   lac d1
   sys write; mpc; mpcs
   jmp stop
badsp:
   jms flush
   lac d1
   sys write; msp; msps
   jmp stop
badasgn:
   jms flush
   lac d1
   sys write; mas; mass
   jmp stop
mpc:
   012;<pc>;012
mpcs = .-mpc
msp:
   012;<sp>;012
msps = .-msp
mas:
   012;<as>;012
mass = .-mas

octal: 0
   lmq
   lac d5
   tad octal i
   cma
   dac 2f
   sna
   jmp 3f
1:
   llss 3
   isz 2f
   jmp 1b
3:
   lac octal i
   dac 2f
   lacq
   dac 2f+1
1:
   lac 2f+1
   lmq
   ecla llss 3
   tad o60
   jms putc
   lac 2f+1
   alss 3
   dac 2f+1
   isz 2f
   jmp 1b
   isz octal
   jmp octal i
2: 0;0
