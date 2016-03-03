   " dmabs

   lac o17
   sys creat; punout
   spa
   sys save
   dac fo
   lac 017777
   tad d1
   dac name
   jms space
   100

loop:
   dzm oldsum
   lac initcmd
   dac comand
   lac i 017777
   sad d4
   jmp stop
   tad dm4
   dac i 017777
   lac name
   tad d4
   dac name

dump1:
   lac comand
   xor dactra
   dac tracmd

dump2:
   sys open; name: 0; 0
   spa
   jmp opnerr
   dac fi
   -bootsiz
   dac c1
   law boot-1
   dac 8
1:
   lac i 8
   jms put
   isz c1
   jmp 1b
   lac bootcmd
   lrs 12
   jms put1
   lac bootcmd
   lrs 6
   jms put1
   lac bootcmd
   and o77
   xor o300
   jms put2

   jms space
   3

dump3:
   -1
   tad bufp
   dac 8
   -64
   dac c1
1:
   dzm i 8
   isz c1
   jmp 1b
   lac fi
   sys read; bufp: buf; 64
   sna
   jmp done
   dac count
   -1
   tad bufp
   dac 8
   -64
   dac c1
   cla
1:
   add i 8
   isz c1
   jmp 1b
   sna
   jmp dump4
   dac newsum
   lac comand
   jms put
   lac count
   jms put
   lac oldsum
   add comand
   add count
   jms put
   lac newsum
   dac oldsum
   jms space
   3
   -1
   tad bufp
   dac 8
   -1
   tad count
   cma
   dac c1
1:
   lac i 8
   jms put
   isz c1
   jmp 1b
   jms space
   10

dump4:
   lac comand
   tad count
   dac comand
   jmp dump3

done:
   lac tracmd
   jms put
   cla
   jms put
   lac oldsum
   add tracmd
   jms put
   jms space
   20
   lac fi
   sys close
   jmp loop

stop:
   cla
   jms put
   jms space
   100
   sys exit

space: 0
   -1
   tad i space
   cma
   dac c1
   isz space
1:
   lac o400
   jms put2
   isz c1
   jmp 1b
   jmp i space

put: 0
   dac 1f
   lrs 12
   jms put1
   lac 1f
   lrs 6
   jms put1
   lac 1f
   jms put1
   jmp i put
1:0

put1:0
   and o77
   xor o200
   jms put2
   jmp i put1

put2: 0
   dac 1f
   lac fo
   sys write; 1f; 1
   jmp i put2
1: 0

boot:
   org = 017740
2:
   jms get1-boot+org
   dac cmd-boot+org
   jms get1-boot+org
   cma
   dac cnt-boot+org
   jms get2-boot+org
   xor sum-boot+org
   dzm sum-boot+org
   cla cll sza
   hlt
   isz cnt-boot+org
1:
   jms get1-boot+org
cmd: 0
   isz cmd-boot+org
   isz cnt-boot+org
   jmp 1b-boot+org
   jmp 2b-boot+org
get1: 0
   jms get2-boot+org
   dac get2-boot+org
   add sum-boot+org
   dac sum-boot+org
   lac get2-boot+org
   jmp i get1-boot+org
get2: 0
   iot 0144
1:
   iot 0101
   jmp 1b-boot+org
   iot 0112
   jmp i get2-boot+org
sum: 0
   cnt = sum+1
   bootsiz = .-boot
bootcmd: jmp org

opnerr:
   lac name
   dac 1f
   lac d1
   sys write; 1: 0; 4
   lac d1
   sys write; mes; 2
   jmp loop
mes:
   040;077012

comand: 0
tracmd: 0
   d1: 1
.17777: 017777
o77: 077
o200: 0200
o300: 0300
d4: 4
d64: 64
dm4: -4
o400: 0400
punout: <pp>;<to>;<ut>;040040
o17: 017

fi: 0
fo: 0
count: 0
oldsum: 0
newsum: 0
daccmd: dac
dactra: dac jmp
initcmd: dac 0
c1: 0
buf:

iot = 0700000