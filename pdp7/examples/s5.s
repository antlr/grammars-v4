"** 01-s1.pdf page 28
" s5

dskswap: 0
   cll; als 3
   dac 9f+t
   jms dsktrans; -64; userdata; 9f+t; dskswap
   lac 9f+t
   tad o20
   dac 9f+t
   jms dsktrans; -4096; 4096; 9f+t; dskswap
   isz dskswap
   jmp dskswap i
t = t+1

access: 0
   lac i.flags
   lmq
   lac u.uid
   spa
   jmp access i
   sad i.uid
   lrs 2
   lacq
   and mode
   sza
   jmp access i
   jms error

fassign: 0
   -10
   dac 9f+t
1:
   lac 9f+t
   tad d10
   jms fget
      jms halt " will not happen
   lac f.flags
   sma
   jmp 1f
   isz 9f+t
   jmp 1b
   jmp fassign i
1:
   lac mode
   xor o400000
   dac f.flags
   lac ii
   dac f.i
   lac 9f+t
   tad d10
   dac u.ac
   dzm f.badd
   jms fput
   isz fassign
   jmp fassign i
t = t+1

fget: 0
   jms betwen; d0; d9
      jmp fget i
   cll; mul; 3
   lacq
"** 01-s1.pdf page 29

   tad ofilesp
   dac 9f+t
   dac .+2
   jms copy; ..; fnode; 3
   isz fget
   jmp fget i

fput: 0
   lac 9f+t
   dac .+3
   jms copy; fnode; ..; 3
   jmp fput i
t = t+1

forall: 0
   lac u.base
   sad u.limit
   jmp 1f
   lac u.base
   ral
   lac u.base i
   snl
   lrs 9
   and o777
   jmp forall i
fallr:
   lac u.base
   add o400000
   dac u.base
   jmp forall+1
1:
   lac u.count
   dac u.ac
   jmp sysexit

sleep: 0
   law ulist-1
   dac 8
   lac o200000
   lmq
1:
   lac u.ulistp i
   sad 8 i
   jmp 1f
   isz 8
   isz 8
   isz 8
   cla; lrs 1
   jmp 1b
1:
   tad o100000
   dac u.ulistp i
   lac sleep i
   dac 9f+t
   lac 9f+t i
   omq
   dac 9f+t i
   isz sleep
   jmp sleep i
t = t+1

"** 01-s1.pdf page 30

dslot: 0
   dzm di
   skp
1:
   isz di
   lac di
   jms dget
   lac d.i
   sza
   jmp 1b
   jmp dslot i

icreat: 0
   dac 9f+t
   jms dslot
   lac o20
   dac ii
1:
   isz ii
   lac ii
   jms iget
   lac i.flags
   spa
   jmp 1b
   lac ii
   dac d.i
   jms copy; name; d.name; 4
   isz s.uniq
   lac s.uniq
   dac d.uniq
   dac i.uniq
   lac 9f+t
   xor o400000
   dac i.flags
   lac u.uid
   dac i.uid
   -1
   dac i.nlks
   dzm i.size
   jms copyz; i.dskps; 7
   jms iput
   jms dput
   jmp icreat i
t = t+1

dspput: 0
   and o177
   sna
   jmp i dspput
   sad o14
   jmp 1f
   lmq
   sad o12
   jms dspnl
   lac dsploc i
   sad o400000
   jmp dspleft
   omq
   dac dsploc i
   isz dsploc
   jmp i dspput

"** 01-s1.pdf page 31

1:
   jms dspinit
   jmp dspput i

dspleft:
   lac dsploc
   sad edspbuf
   jmp 1f
   dac 8
   lac o400000
   dac 8 i
   cla; llss 18+7
   dac dsploc i
   jmp dspput i

dspnl: 0
   lac dsplno
   sad d33
   jmp 1f
   isz dsplno
   jmp dspnl i
1:
   lac o2000
   wbl
   isz dspput
   jmp dspput i

dspinit: 0
   lac dspbufp3
   dac dsploc
   lac o400000
   dac dspbuf+3
   dzm dsplno
   jmp dspinit i

movdsp: 0
   iof
   cdf
   dac dspbufp
   -1
   dac .dspb
   ion
   jmp movdsp i

arg: 0
   lac u.rq+8 i
   isz u.rq+8
   jmp arg i

argname: 0
   jms arg
   dac .+2
   jms copy; ..; name; 4
   lac u.cdir
   jms namei; name
      jms error
   jmp argname i

seektell: 0
   jms arg
   dac u.base

"** 01-s1.pdf page 32
   jms arg
   dac u.limit
   jms finac
   lac u.limit
   sna
   jmp seektell i
   sad d1
   jmp .+3
   lac i.size
   jmp seektell i
   lac f.badd
   jmp seektell i

isown: 0
   jms argname
   jms iget
   lac u.uid
   sma
   sad i.uid
   skp
   jms error
   jmp isown i

