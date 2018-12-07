" dskio

dskrd0: 0
   dzm side
   jms dskio; 02000
   jmp i dskrd0

dskwr0: 0
   dzm side
   jms dskio; 03000
   jmp i dskwr0

dskrd1: 0
   lmq
   lac o200000
   dac side
   lacq
   jms dskio; 02000
   jmp i dskrd1

dskwr1: 0
   lmq
   lac o200000
   dac side
   lacq
   jms dskio; 03000
   jmp i dskwr1


dskio: 0
   cll; idiv; 80
   dac 2f
   lacq
   idiv; 10
   dac 3f
   lls 22
   xor 3f
   als 8
   dac 3f
   lac 2f
   idiv; 10
   dac 2f
   lls 22
   xor 2f
   xor 3f
   xor side
   dac 2f
   -10
   dac 3f
1:
   dscs
   -640
   dslw
   lac dskbufp
   dslm
   lac 2f
   dsld
   lac dskio i
   dsls
   dssf
   jmp .-1
   dsrs
   sma
   jmp 1f
   isz 3f
   jmp 1b
   hlt
1:
   isz dskio
   jmp i dskio
2: 0
3: 0

o200000: 0200000
dskbufp: dskbuf

side: .=.+1
dskbuf: .=.+640
