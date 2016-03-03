"** 01-s1.pdf page 34
" s6

itrunc: 0
   -7
   dac 9f+t
   lac idskpp
   dac 9f+t+1
1:
   lac 9f+t+1 i
   sna
   jmp 4f
   lac i.flags
   and o200000
   sna
   jmp 3f
   -64
   dac 9f+t+2
   lac dskbufp
   dac 9f+t+3
2:
   lac 9f+t+1 i
   jms dskrd
   lac 9f+t+3 i
   sza
   jms free
   isz 9f+t+3
   isz 9f+t+2
   jmp 2b
3:
   lac 9f+t+1 i
   jms free
   dzm 9f+t+1 i
4:
   isz 9f+t+1
   isz 9f+t
   jmp 1b
   lac i.flags
   and o577777
   dac i.flags
   jmp itrunc i
t = t+4

namei: 0
   jms iget
   -1
   tad namei i
   dac 9f+t+1
   isz namei
   lac i.flags
   and o20
   sna
   jmp namei i
   -8
   tad i.size
   cma
   lrss 3
   dac 9f+t
   sna
   jmp namei i
   dzm di
1:
   lac di

"** 01-s1.pdf page 35

   jms dget
   lac d.i
   sna
   jmp 2f
   lac 9f+t+1
   dac 8
   lac d.name
   sad 8 i
   skp
   jmp 2f
   lac d.name+1
   sad 8 i
   skp
   jmp 2f
   lac d.name+2
   sad 8 i
   skp
   jmp 2f
   lac d.name+3
   sad 8 i
   skp
   jmp 2f
   lac d.i
   isz namei
   jmp namei i
2:
   isz di
   isz 9f+t
   jmp 1b
   jmp namei i
t = t+2

iget: 0
   dac ii
   cll; idiv; 5
   dac 9f+t
   lacq
   tad d2
   dac 9f+t+1
   jms dskrd
   lac 9f+t
   cll; mul; 12
   lacq
   tad dskbufp
   dac 9f+t
   dac .+2
   jms copy; ..; inode; 12
   jmp iget i

iput: 0
   lac 9f+t+1
   jms dskrd
   law inode-1
   dac 8
   -1
   tad 9f+t
   dac 9
   -12
   dac 9f+t+2
1:
   lac 8 i

"** 01-s1.pdf page 36

   sad 9 i
   skp
   jmp 2f
   isz 9f+t+2
   jmp 1b
   jmp iput i
2:
   -1
   tad 8
   dac 8
   -1
   tad 9
   dac 9
1:
   lac 8 i
   dac 9 i
   isz 9f+t+2
   jmp 1b
   lac 9f+t+1
   jms dskwr
   jmp iput i
t = t+3

dget: 0
   dac di
   alss 3
   dac 9f+t
   jms pget
   dac 9f+t+1
   jms dskrd
   lac 9f+t
   and o77
   tad dskbufp
   dac 9f+t+2
   dac .+2
   jms copy; ..; dnode; 8
   lac 9f+t
   tad d8
   jms betwen; d0; i.size
      skp
   jmp dget i
   jms dacisize
   dzm d.i
   jmp dget i

dput: 0
   lac 9f+t+1
   jms dskrd
   lac 9f+t+2
   dac .+3
   jms copy; dnode; ..; 8
   lac 9f+t+1
   jms dskwr
   jmp dput i

t = t+3

pget: 0
   lrss 6
   dac 9f+t
   lac i.flags

"** 01-s1.pdf page 37

   and o200000
   sza
   jmp 2f
   lac 9f+t
   jms betwen; d0; d6
      jmp 1f
   tad idskpp
   dac 9f+t
   lac 9f+t i
   sna
   jms alloc
   dac 9f+t i
   jmp pget i
1:
   jms alloc
   dac 9f+t+1
   jms copy; i.dskps; dskbuf; 7
   jms copyz; dskbuf+7; 64-7
   lac 9f+t+1
   jms dskwr
   lac 9f+t+1
   dac i.dskps
   jms copyz; i.dskps+1; 6
   lac i.flags
   xor o200000
   dac i.flags
2:
   lac 9f+t
   lrss 6
   jms betwen; d0; d6
      jms halt " file too big
   tad idskpp
   dac 9f+t+1
   lac 9f+t+1 i
   sna
   jms alloc
   dac 9f+t+1 i
   dac 9f+t+2
   jms dskrd
   lac 9f+t
   and o77
   tad dskbufp
   dac 9f+t+1
   lac 9f+t+1 i
   sza
   jmp pget i
   jms alloc
   dac 9f+t
   lac 9f+t+2
   jms dskrd
   lac 9f+t
   dac 9f+t+1 i
   lac 9f+t+2
   jms dskwr
   lac 9f+t
   jmp pget i
t = t+3

iwrite: 0
   dac 9f+t
   lac iwrite

"** 01-s1.pdf page 38

   dac iread
   lac cskp
   dac iwrite
   jmp 1f

iread: 0
   dac 9f+t
   lac cnop
   dac iwrite
1:
   -1
   tad iread i
   dac 10
   dac 11
   isz iread
   lac iread i
   dac 9f+t+1
   isz iread
   lac o70000
   xct iwrite
   lac i.size
   cma
   tad 9f+t
   cma
   jms betwen; d0; 9f+t+1
      lac 9f+t+1
   dac 9f+t+2
   cma
   tad d1
   sna
   jmp iread i
   dac 9f+t+1
1:
   lac 9f+t
   jms pget
   dac 9f+t+3
   jms dskrd
   lac 9f+t
   and o77
   tad dskbufp
   tad dm1
   xct iwrite
   jmp .+3
   dac 10
cskp:
   skp
   dac 11
2:
   lac 11 i
   dac 10 i
   isz 9f+t
   isz 9f+t+1
   jmp 3f
      xct iwrite
      jmp 4f
      lac 9f+t
      jms betwen; d0; i.size
         dac i.size
      lac 9f+t+3
      jms dskwr
   4:
"** 01-s1.pdf page 38
      lac 9f+t+2
      jmp iread i
3:
   lac 9f+t
   and o77
   sza
   jmp 2b
   xct iwrite
   jmp 1b
   lac 9f+t+3
   jms dskwr
   jmp 1b
t = t+4

finac: 0
   lac u.ac
   jms fget
      jms error
   lac f.flags
   sma
   jms error
   lac f.i
   jms iget
   jmp finac i

dacisize: 0
   dac i.size
   jms iput
   lac i.size
   jmp dacisize i
