" bi

start:
   jms initio
   -1
   tad .main
   dac pc

fetch:
   lac pc i
   lmq
   and o17777
   dac addr
   cla; lls 4    " XXX  replaced for now:  ecla lls 4
   tad .+3
   dac .+1
   jmp .. i
   jmp . i
   autop; binop; consop; ifop; etcop; setop; traop
   unaop; extop; aryop


ifop:
   -2
   tad sp
   dac sp
   lac sp i
   dac t1
   lac t1 i
   sza
   jmp fetch
   -1
   tad addr i
   dac pc
   jmp fetch

autop:
   lac addr
   tad dp
   dac sp i
   isz sp
   isz sp
   jmp fetch

binop:
   -2
   tad sp
   dac sp
   tad dm1
   dac t4
   tad dm1
   dac t3
   lac t3 i
   dac t1
   lac sp i
   dac t2
   lac t4
   dac t3 i
   lac addr
   tad .+3
   dac .+1
   jmp .. i
   jmp . i
   basg; bor; band; beq; bne; ble; blt; bge; bgt; brsh; blsh
   badd; bmin; bmod; bmul; bdiv

basg:
   lac t2 i
   dac t1 i
   dac t4 i
   jmp fetch

bor:
   lac t1 i
   lmq
   lac t2 i
   omq
   dac t4 i
   jmp fetch

band:
   lac t1 i
   and t2 i
   dac t4 i
   jmp fetch

beq:
   lac t1 i
   xor t2 i
   sna cla
   lac d1
   dac t4 i
   jmp fetch

bne:
   lac t1 i
   xor t2 i
   sza
   lac d1
   dac t4 i
   jmp fetch

ble:
   lac t2 i
   cma
   tad t1 i
   spa cla
   lac d1
   dac t4 i
   jmp fetch

blt:
   lac t1 i
   cma
   tad t2 i
   sma cla
   lac d1
   dac t4 i
   jmp fetch

bge:
   lac t1 i
   cma
   tad t2 i
   spa cla
   lac d1
   dac t4 i
   jmp fetch

bgt:
   lac t2 i
   cma
   tad t1 i
   sma cla
   lac d1
   dac t4 i
   jmp fetch

brsh:
blsh:
   hlt

badd:
   lac t1 i
   tad t2 i
   dac t4 i
   jmp fetch

bmin:
   lac t1 i
   cma
   tad t2 i
   cma
   dac t4 i
   jmp fetch

bmod:
   lac t2 i
   dac .+4
   lac t1 i
   cll; idiv; ..
   dac t4 i
   jmp fetch

bmul:
   lac t2 i
   dac .+4
   lac t1 i
   cll; mul; ..
   lacq
   dac t4 i
   jmp fetch

bdiv:
   lac t2 i
   dac .+4
   lac t1 i
   cll; idiv; ..
   lacq
   dac t4 i
   jmp fetch

consop:
   lac sp
   tad d1
   dac sp i
   isz sp
   lac addr
   dac sp i
   isz sp
   jmp fetch

etcop:
   lac addr
   tad .+3
   dac .+1
   jmp .. i
   jmp . i
   mcall; mark; call; vector; litrl; goto; retrn; escp

mcall:
   -2
   tad sp
   dac t1
   lac t1 i
   dac t2
   -1
   tad t2 i
   lmq
   lac dp
   dac t1 i
   lac t1
   dac dp
   isz t1
   lac pc
   dac t1 i
   lacq
   dac pc
   jmp fetch

mark:
   -1
   tad sp
   dac t2
   tad dm1
   dac t1
   lac t1 i
   dac t3
   lac t3 i
   dac t2 i
   lac ap
   dac t1 i
   lac t1
   dac ap
   jmp fetch

call:
   lac ap
   tad d1
   dac 8
   dac 9
1:
   lac 8 i
   dac t1
   lac t1 i
   dac 9 i
   isz 8
   -1
   tad sp
   sad 8
   skp
   jmp 1b
   lac ap i
   lmq
   lac dp
   dac ap i
   lac ap
   dac dp
   isz ap
   -1
   tad ap i
   dac t1
   lac pc
   dac ap i
   lacq
   dac ap
   lac t1
   dac pc
   jmp fetch

vector:
   -2
   tad sp
   dac sp
   tad dm2
   dac t1
   lac sp i
   dac t2
   lac t1 i
   dac t3
   lac t3 i
   tad t2 i
   dac t1 i
   jmp fetch

litrl:
   lac sp
   tad d1
   dac sp i
   isz sp
   lac pc i
   dac sp i
   isz sp
   jmp fetch

goto:
   -2
   tad sp
   dac sp
   lac sp i
   dac t1
   -1
   tad t1 i
   dac pc
   jmp fetch
   
retrn:
   -2
   tad sp
   dac sp
   lac sp i
   dac t1
   lac t1 i
   lmq
   lac dp
   dac sp
   dac t1
   lac sp i
   sna
   jmp stop
   dac dp
   isz sp
   lac sp
   dac t1 i
   lac sp i
   dac pc
   lacq
   dac sp i
   isz sp
   jmp fetch

escp:
   law 2
   tad pc
   dac t1
   jmp t1 i

setop:
   lac addr
   tad dp
   dac sp
   jmp fetch

traop:
   -1
   tad addr
   dac pc
   jmp fetch

unaop:
   -1
   tad sp
   dac t3
   tad dm1
   dac t2
   lac t2 i
   dac t1
   lac t3
   dac t2 i
   lac addr
   tad .+3
   dac .+1
   jmp .. i
   jmp . i
   uadr; umin; uind; unot

uadr:
   lac t1
   dac t3 i
   jmp fetch

umin:
   -1
   tad t1 i
   cma
   dac t3 i
   jmp fetch

uind:
   lac t1 i
   dac t2 i
   jmp fetch

unot:
   lac t1 i
   sna cla
   lac d1
   dac t3 i
   jmp fetch

extop:
   lac addr
   dac sp i
   isz sp
   isz sp
   jmp fetch

aryop:
   lac addr
   tad dp
   dac t1
   tad d1
   dac t1 i
   jmp fetch

a = 040000
b = a+a
c = b+a
f = c+a
n = f+a
s = n+a
t = s+a
u = t+a
x = u+a
y = x+a

d1: 1
dm1: -1
dm2: -2
o17777: 017777

t1: 0
t2: 0
t3: 0
t4: 0
addr: 0

pc = 017

sp: stack
dp: stack
ap: stack
stack: 0
