"** 01-s1.pdf page 14
" s3

searchu: 0
   lac searchu i
   dac 9f+t+1
   -mnproc
   dac 9f+t
   law ulist-1
   dac 8
1:
   lac 8 i
   dac lu
   lac 8 i
   dac lu+1
   lac 8 i
   dac lu+2
   lac 8 i
   dac lu+3
   jms 9f+t+1 i
   isz 9f+t
   jmp 1b
   isz searchu
   jmp searchu i
t = t+2

lookfor: 0
   jms searchu; 1f
   isz lookfor
   isz lookfor
   jmp lookfor i
1: 0
   lac lu
   rtl; rtl; and o7
   sad lookfor i
   skp
   jmp 1b i
   -3
   tad 8
   and o17777
   isz lookfor
   jmp lookfor i

.fork:
   jms lookfor; 0 " not-used
      skp
      jms error
   dac 9f+t
   isz uniqpid
   lac uniqpid
   dac u.ac
   law sysexit
   dac u.swapret
   lac o200000
   tad u.ulistp i
   dac u.ulistp i
   jms dskswap; 07000
   lac 9f+t
   dac u.ulistp
   lac o100000
   xor u.ulistp i
   dac u.ulistp i
   lac u.pid
"** 01-s1.pdf page 15
   dac u.ac
   lac uniqpid
   dac u.pid
   isz 9f+t
   dac 9f+t i
   isz u.rq+8
   dzm u.intflg
   jmp sysexit
t= t+1

badcal:
   clon
   -1
   dac 7
.save:
   lac d1
   jms iget
   cla
   jms iwrite; 4096; 4096
   jms iwrite; userdata; 64
   jms iput

.exit:
   lac u.dspbuf
   sna
   jmp .+3
   law dspbuf
   jms movdsp
   jms awake
   lac u.ulistp i
   and o77777
   dac u.ulistp i
   isz u.ulistp
   dzm u.ulistp i
   jms swap

.rmes:
   jms awake
   lac o100000
   tad u.ulistp i
   dac u.ulistp i
   law 2
   tad u.ulistp
   dac 9f+t
   -1
   dac 9f+t i
   jms swap
   law 2
   tad u.ulistp
   dac 9f+t
   lac 9f+t i
   cma
   dac u.ac
   dzm 9f+t i
   isz 9f+t
   lac 9f+t i
   dac u.mq
   dzm 9f+t i
   jmp sysexit
t = t+1

"** 01-s1.pdf page 16
.smes:
   lac u.ac
   sna spa
   jms error
   jms searchu; 1f
   law 2
   tad u.ulistp
   dac 9f+t
   dzm 9f+t i
   jms error
1: 0
   lac lu+1
   sad u.ac
   skp
   jmp 1b i
   lac lu+2
   sad dm1
   jmp 1f
   lac o100000
   tad u.ulistp i
   dac u.ulistp i
   law 2
   tad u.ulistp
   dac 9f+t
   lac u.ac
   dac 9f+t i
   jms swap
   law 2
   tad u.ulistp
   dac 9f+t
   dzm 9f+t i
   jmp .smes
1:
   -3
   tad 8
   dac 9f+t
   lac o700000
   tad 9f+t i
   dac 9f+t i
   isz 9f+t
   isz 9f+t
   lac u.pid
   cma
   dac 9f+t i
   isz 9f+t
   lac u.mq
   dac 9f+t i
   jmp okexit
t = t+1

awake: 0
   jms searchu; 1f
   jmp awake i
1: 0
   lac u.pid
   sad lu+2
   skp
   jmp 1b i
   -3
   tad 8
   dac 9f+t
"** 01-s1.pdf page 17
   lac o700000
   tad 9f+t i
   dac 9f+t i
   jmp 1b i
t = t+1

swr:
sww:
   jmp .-4 i
   .halt; rttyi; rkbdi; rppti; .halt
   .halt; wttyo; wdspo; wppto

.halt: jms halt

rttyi:
   jms chkint1
   lac d1
   jms getchar
      jmp 1f
   and o177
   jms betwen; o101; o132
      skp
   tad o40
   alss 9
   jmp passone
1:
   jms sleep; sfiles+0
   jms swap
   jmp rttyi

wttyo:
   jms chkint1
   jms forall
   sna
   jmp fallr
   lmq
   lac sfiles+1
   spa
   jmp 1f
   xor o400000
   dac sfiles+1
   lacq
   tls
   sad o12
   jms putcr
   jmp fallr
1:
   lacq
   dac char
   lac d2	"** written: d6 ttyout
   jms putchar
      skp
   jmp fallr
   jms sleep; sfiles+1
   jms swap
   jmp wttyo

rkbdi:
   jms chkint1
   lac d3
   jms getchar
"** 01-s1.pdf page 18
      jmp 3f
   lmq
   and o155
   sad o55
   jmp 1f
   lacq
   and o137
   sad o134
   skp
   jmp 2f
1:
   lacq
   xor o20
   lmq
2:
   lacq
   dac u.limit
1:
   jms chkint1
   lac u.limit
   jms dspput
      jmp 1f
   jms sleep; sfiles+6
   jms swap
   jmp 1b
1:
   lac u.limit
   alss 9
   jmp passone
3:
   jms sleep; sfiles+2
   jms swap
   jmp rkbdi

wdspo:
   jms chkint1
   jms forall
   jms dspput
      jmp fallr
   jms sleep; sfiles+6
   jms swap
   jmp wdspo


rppti:
   lac d4
   jms getchar
      jmp .+3
   alss 9
   jmp passone
   lac sfiles+3
   sma
   rsa
1:
   jms sleep; sfiles+3
   jms swap
   jmp rppti
"** 01-s1.pdf page 19

wppto:
   jms forall
   sna
   jmp fallr
   lmq
   lac sfiles+4
   spa
   jmp 1f
   xor o400000
   dac sfiles+4
   lacq
   psa
   jmp fallr
1:
   lacq
   dac char
   lac d5
   jms putchar
      skp
   jmp fallr
   jms sleep; sfiles+4
   jms swap
   jmp wppto

passone:
   sad o4000
   jmp okexit
   dac u.base i
   lac d1
   dac u.ac
   jmp sysexit

error: 0
   -1
   dac u.ac
   jmp sysexit

chkint1: 0
   dzm .insys
   jms chkint
      skp
   jmp .save
   -1
   dac .insys
   jmp chkint1 i
