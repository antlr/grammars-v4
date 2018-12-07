"** 01-s1.pdf page 53
" s9 -- cold boot

. = coldentry+4

" zero i-list

   dzm ii			" Set ii to zero
   jms copyz; dskbuf; 64	" Zero the 64-word dskbuf
1:
   lac ii			" Zero blocks 0 to 709
   jms dskio; 07000
   isz ii			" Move up to next block number
   -710
   tad ii
   sza
   jmp 1b			" Loop back until we reach block 710
				" i.e. we don't write block 710

" free rest of disk

1:
   lac ii			" Free blocks 710 up to 6399
   jms free
   isz ii
   -6400
   tad ii
   sza
   jmp 1b			" Loop back until we reach block 6400

" read in tapes

   dzm ii
1:
   dzm sum
   jms getw " count
   sza
   jmp .+3
   hlt
   jmp 1b " 0 count means pause
   dac xx
   isz ii
   lac ii
   jms iget
   jms copyz; inode; 12
   jms getw " flags
   dac i.flags
   -1
   dac i.uid
   jms getw " number links
   dac i.nlks
   -2
   tad xx
   dac i.size
   lac ii
   dac i.uniq
   law 4096-1
   dac 8
   -1
   tad i.size
   cma
   sna
   jmp 3f
   dac xx

"** 01-s1.pdf page 54
2:
   jms getw
   dac 8 i
   isz xx
   jmp 2b
3:
   lac sum
   dac xx
   jms getw " checksum
   sad xx
   skp
   jms halt
   lac i.size
   dac .+4
   cla
   jms iwrite; 4096; ..
   jms iput
   cla
   jms dskio; 07000	"** writing on listing
   jmp 1b

getw: 0
   jms getc
   alss 12
   lmq
   jms getc
   alss 6
   omq
   lmq
   jms getc
   omq
   lmq
   add sum
   dac sum
   lacq
   jmp getw i

getc: 0
   iof
   rsa
   rsf
   jmp .-1
   rrb
   sna
   jmp getc+1
   and o77
   ion
   jmp getc i
xx: 0
sum:  0
