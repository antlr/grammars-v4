" ds

   lac 017777 i
   sad d8
   skp
   sys exit
   lac 017777
   tad d5
   dac .+3
   law 017
   sys creat; ..
   dac fo
   law 017
   sys creat; scrname
   spa; jms error
   dac fso
   sys open; scrname; 0
   spa; jms error
   dac fsi
   sys chdir; dd
   spa; jms error
   lac d1
   sys write; pass1; 1
   law fsobuf
   dac fsopt
   dzm nfiles
   law fbuf
   dac filp
   dzm ndirs
   law dbuf
   dac dirp
   dzm fsloc
   sys open; dotdot; 0
   spa; jms error
   dac fd
   jms readdir; dotdot
   law statbuf
   sys status; dotdot; dotdot
   spa; jms error
   lac statbuf+12 " i index
   dac dirp i
   isz dirp
   -1
   tad nfiles
   cma
   dac ddfiles
   law fbuf
   dac ddfilp

loop:
   -1
   tad ndirs
   cma
   dac c1
   law dbuf
   dac i1

1:
   isz i1
   lac i1 i
   sad ddfilp i
   jmp 2f
   isz i1
   isz c1
   jmp 1b

   lac ddfilp
   tad i1
   dac i1
   lac i1 i
   dac .+3
   lac fsi
   sys seek; ..; 0
   lac fsi
   sys read; scrname; 4
   law statbuf
   sys status; dotdot; scrname
   spa; jms error
   lac statbuf+0 " flags
   and o20
   sna
   jmp 2f
   sys open; scrname; 0
   spa; jms error
   dac fd
   jms readdir; scrname
   lac ddfilp i
   dac dirp i
   isz dirp

2:
   isz ddfilp
   isz ddfilp
   isz ddfiles
   jmp loop

" output phase

   lac fso
   sys write; fsobuf; 64
   lac d1
   sys write; pass2; 2
   -500
   dac c1

1:
   law dbuf+2
   dac i1
   dzm fflg
   law fbuf
   dac i2
   r1
   tad nfiles
   cma
   dac c2

2:
   lac c1
   tad d501
   sad i2 i
   skp
   jmp 3f
   -1
   tad i1
   dac i3
   iac i3 i
   dac c3
   law fbuf
   dac i3
0:
   lac i3 i
   sad c3
   jmp 0f
   isz i3
   isz i3
   jmp 0b
0:
   lac i3
   tad d1
   dac c3
   lac c3 i
   dac .+3
   lac fsi
   sys seek; ..; 0
   lac fsi
   sys read; scrname; 4
   lac i2
   tad d1
   dac c3
   lac c3 i
   dac .+3
   lac fsi
   sys seek; ..; 0
   lac fsi
   sys read; dd; 4
   lac fflg
   sza
   jmp 0f

   lac nlinkt
   sad nlinka
   skp
   jms fishy
   dzm nlinka
   law 012
   jms putc
   law statbuf
   sys status; scrname; dd
   spa; jms error
   -1
   tad statbuf+9
   cma
   dac nlinkt
   -1
   dac fflg
   jms longout
   law 012
   jms putc
0:
   isz nlinka
   jms putname; scrname
   jms putname; dd
   law 012
   jms putc

3:
   isz i2
   isz i2
   lac i2
   sad i1 i
   skp
   jmp .+3
   isz i1
   isz i1
   isz c2
   jmp 2b

   isz c1
   jmp 1b
   lac nlinkt
   sad nlinka
   skp
   jms fishy

   sys chdir; system
   jmp done

fishy: 0
   jms asters
   jms asters
   law 012
   jms putc
   lac d1
   sys write; 1f; 1
   jmp fishy i
1: 052012

nlinka: 0
nlinkt: 0

asters: 0
   -10
   dac c
1:
   law 052
   jms putc
   isz c
   jmp 1b
   jmp asters i

longout: 0
   lac statbuf+12 " i
   jms octal; -3
   lac statbuf+0 " flags
   jms octal; -2
   lac statbuf+8 " uid
   jms octal; -2
   -1
   tad statbuf+9 " nlinks
   cma
   jms octal; -2
   lac statbuf+10
   jms octal; -5
   jmp longout i

readdir: 0
   law 012
   jms putc
   law 012
   jms putc
   jms asters
   lac readdir i
   dac 5f
   dac .+2
   jms putname; ..
   jms asters
   law 012
   jms putc
   law 012
   jms putc
   isz readdir
   isz ndirs
   lac filp
   dac dirp i
   isz dirp
0:
   jms copyz; buf; 64
   lac fd
   sys read; buf; 64
   spa; jms error
   sna
   jmp 4f
   -8
   dac c1
   law buf
   dac i1
1:
   lac i1 i
   sna
   jmp 3f

   isz nfiles
   dac filp i
   isz filp
   lac fsloc
   dac filp i
   tad d4
   dac fsloc
   isz filp
   lac i1
   tad d1
   dac .+4
   law statbuf
   sys status; 5:..; ..
   spa; jms error
   jms longout
   lac i1
   tad d1
   dac .+2
   jms putname; ..
   law 012
   jms putc
   lac i1
   dac 8
   lac 8 i
   dac fsopt i
   isz fsopt
   lac 8 i
   dac fsopt i
   isz fsopt
   lac 8 i
   dac fsopt i
   isz fsopt
   lac 8 i
   dac fsopt i
   isz fsopt
   law fsobuf+64
   sad fsopt
   skp
   jmp 3f
   lac fso
   sys write; fsobuf; 64
   law fsobuf
   dac fsopt

3:
   law 8
   tad i1
   dac i1
   isz c1
   jmp 1b
   jmp 0b
4:
   lac fd
   sys close
   jmp readdir i

putname: 0
   -1
   tad putname i
   dac 8
   -4
   dac c
1:
   lac 8 i
   lrss 9
   jms putc
   llss 9
   jms putc
   isz c
   jmp 1b
   isz putname
   jmp putname i

octal: 0
   lmq
   lac d5
   tad octal i
   cma
   dac c
1:
   llss 3
   isz c
   jmp 1b
   lac octal i
   dac c
1:
   ecla llss 3
   tad o60
   jms putc
   isz c
   jmp 1b
   law 040
   jms putc
   isz octal
   jmp octal i

error: 0
   -1
   tad error
   hlt
   sys save

copyz: 0
   -1
   tad copyz i
   dac 8
   isz copyz
   -1
   tad copyz i
   cma
   dac 2f
   isz copyz
1:
   dzm 8 i
   isz 2f
   jmp 1b
   jmp copyz i
2: 0

done:
   lac noc
   sna
   sys exit
   and d1
   sna cla
   jmp 1f
   jms putc
   jmp done
1:
   lac noc
   rcr
   dac 1f
   lac fo
   sys write; obuf; 1;..
   sys exit

putc: 0
   and o177
   dac 2f+1
   lac opt
   dac 2f
   add o400000
   dac opt
   spa
   jmp 1f
   lac 2f i
   xor 2f+1
   jmp 3f
1:
   lac 2f+1
   alss 9
3:
   dac 2f i
   isz noc
   lac noc
   sad d128
   skp
   jmp putc i
   lac fo
   sys write; obuf; 64
   lac iopt
   dac opt
   dzm noc
   jmp putc i
2: 0;0
opt: obuf
iopt: obuf
noc: 0
fo: 1

d1: 1
o177: 0177
o400000: 0400000
d128: 128
d4: 4
d5: 5
d8: 8
o60: 060
o20: 020
d501: 501

dd:
   <dd>; 040040; 040040; 040040
dotdot:
   056056; 040040; 040040; 040040
system:
   <sy>;<st>;<em>; 040040
scrname:
   <*s>;<rc>;040040;040040
pass2:
   <i
pass1:
   <i 012

fso: .=.+1
fsi: .=.+1
fsloc: .=.+1
nfiles: .=.+1
fflg: .=.+1
buf: .=.+64
obuf: .=.+64
fd: .=.+1
filp: .=.+1
ddfilp: .=.+1
ddfiles: .=.+1
statbuf: .=.+13
c: .=.+1
i1: .=.+1
i2: .=.+1
i3: .=.+1
c1: .=.+1
c2: .=.+1
c3: .=.+1
ndirs: .=.+1
dirp: .=.+1
fsopt: .=.+1
fsobuf: .=.+64
dbuf: .=.+100
fbuf:
   