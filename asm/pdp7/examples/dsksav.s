" dsksav

   iof
   hlt
   dzm track
   -640
   dac c1
1:
   lac track
   jms dskrd1

   lac track
   jms dskwr0

   lac track
   tad d10
   dac track
   isz c1
   jmp 1b

   hlt
   sys exit

track: 0
c1: 0
d10: 10
