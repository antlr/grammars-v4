" paper tape bootstrap: reads system from track 180
" (could read from any track 180-189 based on switches?)
" phil budne 3/24/2016

" must be output as a "rim" tape for SIMH to boot!!!

" load at normal (user) address of 010000
   iof				" interrupts off
   caf				" reset cpu
   dscs				" clear disk status

" code from maksys (altered to read instead of write):
   -3072; dslw
   cla; dslm
   lac track; alss 8; xor o300000; dsld
   lac o2000; dsls
   dssf; jmp .-1
   dsrs; spa; hlt

   -1024; dslw
   lac d3072; dslm
   lac track; alss 8; xor o300110; dsld
   lac o2000; dsls
   dssf; jmp .-1
   dsrs; spa; hlt

   jmp 0100

o2000: 02000
d3072: 3072
o300000: 0300000
o300110: 0300110
track: 0		" get 0-9 from switches!!!
