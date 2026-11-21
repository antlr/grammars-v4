        name    'DIOMOD'
        title   'Direct CP/M Calls From PL/I-80'
;
;***********************************************************
;*                                                         *
;*      cp/m calls from pl/i for direct i/o                *
;*                                                         *
;***********************************************************
        public  memptr  ;return pointer to base of free mem
        public  memsiz  ;return size of memory in bytes
        public  memwds  ;return size of memory in words
        public  dfcb0   ;return address of default fcb 0
        public  dfcb1   ;return address of default fcb 1
        public  dbuff   ;return address of default buffer
        public  reboot  ;system reboot (#0)
        public  rdcon   ;read console character (#1)
        public  wrcon   ;write console character(#2)
        public  rdrdr   ;read reader character (#3)
        public  wrpun   ;write punch character (#4)
        public  wrlst   ;write list character (#5)
        public  coninp  ;direct console input (#6a)
        public  conout  ;direct console output (#6b)
        public  rdstat  ;read console status (#6c)
        public  getio   ;get io byte (#8)
        public  setio   ;set i/o byte (#9)
        public  wrstr   ;write string (#10)
        public  rdbuf   ;read console buffer (#10)
        public  break   ;get console status (#11)
        public  vers    ;get version number (#12)
        public  reset   ;reset disk system (#13)
        public  select  ;select disk (#14)
        public  open    ;open file (#15)
        public  close   ;close file (#16)
        public  sear    ;search for file (#17)
        public  searn   ;search for next (#18)
        public  delete  ;delete file (#19)
        public  rdseq   ;read file sequential mode (#20)
        public  wrseq   ;write file sequential mode (#21)
        public  make    ;create file (#22)
        public  rename  ;rename file (#23)
        public  logvec  ;return login vector (#24)
        public  curdsk  ;return current disk number (#25)
        public  setdma  ;set DMA address (#26)
        public  allvec  ;return address of alloc vector (#27)
        public  wpdisk  ;write protect disk (#28)
        public  rovec   ;return read/only vector (#29)
        public  filatt  ;set file attributes (#30)
        public  getdpb  ;get base of disk parm block (#31)
        public  getusr  ;get user code (#32a)
        public  setusr  ;set user code (#32b)
        public  rdran   ;read random (#33)
        public  wrran   ;write random (#34)
        public  filsiz  ;random file size (#35)
        public  setrec  ;set random record pos (#36)
        public  resdrv  ;reset drive (#37)
        public  wrranz  ;write random, zero fill (#40)
        public  sgscb   ;set/get System Control Block byte/word
;
;
        extrn   ?begin  ;beginning of free list
        extrn   ?boot   ;system reboot entry point
        extrn   ?bdos   ;bdos entry point
        extrn   ?dfcb0  ;default fcb 0
        extrn   ?dfcb1  ;default fcb 1
        extrn   ?dbuff  ;default buffer
;
;***********************************************************
;*                                                         *
;*        equates for interface to cp/m bdos               *
;*                                                         *
;***********************************************************
cr      equ     0dh     ;carriage return
lf      equ     0ah     ;line feed
eof     equ     1ah     ;end of file
;
readc   equ     1       ;read character from console
writc   equ     2       ;write console character
rdrf    equ     3       ;reader input
punf    equ     4       ;punch output
listf   equ     5       ;list output function
diof    equ     6       ;direct i/o, version 2.0
getiof  equ     7       ;get i/o byte
setiof  equ     8       ;set i/o byte
printf  equ     9       ;print string function
rdconf  equ     10      ;read console buffer
statf   equ     11      ;return console status
versf   equ     12      ;get version number
resetf  equ     13      ;system reset
seldf   equ     14      ;select disk function
openf   equ     15      ;open file function
closef  equ     16      ;close file
serchf  equ     17      ;search for file
serchn  equ     18      ;search next
deletf  equ     19      ;delete file
readf   equ     20      ;read next record
writf   equ     21      ;write next record
makef   equ     22      ;make file
renamf  equ     23      ;rename file
loginf  equ     24      ;get login vector
cdiskf  equ     25      ;get current disk number
setdmf  equ     26      ;set dma function
getalf  equ     27      ;get allocation base
wrprof  equ     28      ;write protect disk
getrof  equ     29      ;get r/o vector
setatf  equ     30      ;set file attributes
getdpf  equ     31      ;get disk parameter block
userf   equ     32      ;set/get user code
rdranf  equ     33      ;read random
wrranf  equ     34      ;write random
filszf  equ     35      ;compute file size
setrcf  equ     36      ;set random record position
rsdrvf  equ     37      ;reset drive function
wrrnzf  equ     40      ;write random zero fill
scbf    equ     49      ;set/get SCB
;
;       utility functions
;***********************************************************
;*                                                         *
;*       general purpose routines used upon entry          *
;*                                                         *
;***********************************************************
;
getp1:  ;get single byte parameter to register e
        mov     e,m             ;low (addr)
        inx     h
        mov     d,m             ;high(addr)
        xchg                    ;hl = .char
        mov     e,m             ;to register e
        ret
;
getp2:  ;get single word value to DE
getp2i: ;(equivalent to getp2)
        call    getp1
        inx     h
        mov     d,m             ;get high byte as well
        ret
;
getver: ;get cp/m or mp/m version number
        push    h               ;save possible data adr
        mvi     c,versf
        call    ?bdos
        pop     h               ;recall data addr
        ret
;
chkv20: ;check for version 2.0 or greater
        call    getver
        cpi     20
        rnc                     ;return if > 2.0
;       error message and stop
        jmp     vererr          ;version error
;
chkv22: ;check for version 2.2 or greater
        call    getver
        cpi     22h
        rnc                     ;return if >= 2.2
vererr:
        ;version error, report and terminate
        lxi     d,vermsg
        mvi     c,printf
        call    ?bdos           ;write message
        jmp     ?boot           ;and reboot
vermsg: db      cr,lf,'Later CP/M or MP/M Version Required$'
;
;***********************************************************
;*                                                         *
;***********************************************************
memptr: ;return pointer to base of free storage
        lhld    ?begin
        ret
;
;***********************************************************
;*                                                         *
;***********************************************************
memsiz: ;return size of free memory in bytes
        lhld    ?bdos+1         ;base of bdos
        xchg                    ;de = .bdos
        lhld    ?begin          ;beginning of free storage
        mov     a,e             ;low(.bdos)
        sub     l               ;-low(begin)
        mov     l,a             ;back to l
        mov     a,d             ;high(.bdos)
        sbb     h
        mov     h,a             ;hl = mem size remaining
        ret
;
;***********************************************************
;*                                                         *
;***********************************************************
memwds: ;return size of free memory in words
        call    memsiz          ;hl = size in bytes
        mov     a,h             ;high(size)
        ora     a               ;cy = 0
        rar                     ;cy = ls bit
        mov     h,a             ;back to h
        mov     a,l             ;low(size)
        rar                     ;include ls bit
        mov     l,a             ;back to l
        ret                     ;with wds in hl
;
;***********************************************************
;*                                                         *
;***********************************************************
dfcb0:  ;return address of default fcb 0
        lxi     h,?dfcb0
        ret
;
;***********************************************************
;*                                                         *
;***********************************************************
dfcb1:  ;return address of default fcb 1
        lxi     h,?dfcb1
        ret
;
;***********************************************************
;*                                                         *
;***********************************************************
dbuff:  ;return address of default buffer
        lxi     h,?dbuff
        ret
;
;***********************************************************
;*                                                         *
;***********************************************************
reboot: ;system reboot (#0)
        jmp     ?boot
;
;***********************************************************
;*                                                         *
;***********************************************************
rdcon:  ;read console character (#1)
        ;return character value to stack
        mvi     c,readc
        jmp     chrin           ;common code to read char
;
;***********************************************************
;*                                                         *
;***********************************************************
wrcon:  ;write console character(#2)
        ;1->char(1)
        mvi     c,writc         ;console write function
        jmp     chrout          ;to write the character
;
;***********************************************************
;*                                                         *
;***********************************************************
rdrdr:  ;read reader character (#3)
        mvi     c,rdrf          ;reader function
chrin:
        ;common code for character input
        call    ?bdos           ;value returned to A
        pop     h               ;return address
        push    psw             ;character to stack
        inx     sp              ;delete flags
        mvi     a,1             ;character length is 1
        pchl                    ;back to calling routine
;
;***********************************************************
;*                                                         *
;***********************************************************
wrpun:  ;write punch character (#4)
        ;1->char(1)
        mvi     c,punf          ;punch output function
        jmp     chrout          ;common code to write chr
;
;***********************************************************
;*                                                         *
;***********************************************************
wrlst:  ;write list character (#5)
        ;1->char(1)
        mvi     c,listf         ;list output function
chrout:
        ;common code to write character
        ;1-> character to write
        call    getp1           ;output char to register e
        jmp     ?bdos           ;to write and return
;
;***********************************************************
;*                                                         *
;***********************************************************
coninp: ;perform console input, char returned in stack
        lxi     h,chrstr        ;return address
        push    h               ;to stack for return
        lhld    ?boot+1         ;base of bios jmp vector
        lxi     d,2*3           ;offset to jmp conin
        dad     d
        pchl                    ;return to chrstr
;
chrstr: ;create character string, length 1
        pop     h               ;recall return address
        push    psw             ;save character
        inx     sp              ;delete psw
        mvi     a,1             ;string length is 1
        pchl                    ;return to caller
;
;***********************************************************
;*                                                         *
;***********************************************************
conout: ;direct console output
        ;1->char(1)
        call    getp1           ;get parameter
        mov     c,e             ;character to c
        lhld    ?boot+1         ;base of bios jmp
        lxi     d,3*3           ;console output offset
        dad     d               ;hl = .jmp conout
        pchl                    ;return through handler
;
;***********************************************************
;*                                                         *
;***********************************************************
rdstat: ;direct console status read
        lxi     h,rdsret        ;read status return
        push    h               ;return to rdsret
        lhld    ?boot+1         ;base of jmp vector
        lxi     d,1*3           ;offset to .jmp const
        dad     d               ;hl = .jmp const
        pchl
;
;***********************************************************
;*                                                         *
;***********************************************************
getio:  ;get io byte (#8)
        mvi     c,getiof
        jmp     ?bdos           ;value returned to A
;
;***********************************************************
;*                                                         *
;***********************************************************
setio:  ;set i/o byte (#9)
        ;1->i/o byte
        call    getp1           ;new i/o byte to E
        mvi     c,setiof
        jmp     ?bdos           ;return through bdos
;
;***********************************************************
;*                                                         *
;***********************************************************
wrstr:  ;write string (#10)
        ;1->addr(string)
        call    getp2           ;get parameter value to DE
        mvi     c,printf        ;print string function
        jmp     ?bdos           ;return through bdos
;
;***********************************************************
;*                                                         *
;***********************************************************
rdbuf:  ;read console buffer (#10)
        ;1->addr(buff)
        call    getp2i          ;DE = .buff
        mvi     c,rdconf        ;read console function
        jmp     ?bdos           ;return through bdos
;
;***********************************************************
;*                                                         *
;***********************************************************
break:  ;get console status (#11)
        mvi     c,statf
        call    ?bdos           ;return through bdos
;
rdsret: ;return clean true value
        ora     a               ;zero?
        rz                      ;return if so
        mvi     a,0ffh          ;clean true value
        ret
;
;***********************************************************
;*                                                         *
;***********************************************************
vers:   ;get version number (#12)
        mvi     c,versf
        jmp     ?bdos           ;return through bdos
;
;***********************************************************
;*                                                         *
;***********************************************************
reset:  ;reset disk system (#13)
        mvi     c,resetf
        jmp     ?bdos
;
;***********************************************************
;*                                                         *
;***********************************************************
select: ;select disk (#14)
        ;1->fixed(7) drive number
        call    getp1           ;disk number to E
        mvi     c,seldf
        jmp     ?bdos           ;return through bdos
;***********************************************************
;*                                                         *
;***********************************************************
open:   ;open file (#15)
        ;1-> addr(fcb)
        call    getp2i          ;fcb address to de
        mvi     c,openf
        jmp     ?bdos           ;return through bdos
;
;***********************************************************
;*                                                         *
;***********************************************************
close:  ;close file (#16)
        ;1-> addr(fcb)
        call    getp2i          ;.fcb to DE
        mvi     c,closef
        jmp     ?bdos           ;return through bdos
;
;***********************************************************
;*                                                         *
;***********************************************************
sear:   ;search for file (#17)
        ;1-> addr(fcb)
        call    getp2i          ;.fcb to DE
        mvi     c,serchf
        jmp     ?bdos
;
;***********************************************************
;*                                                         *
;***********************************************************
searn:  ;search for next (#18)
        mvi     c,serchn        ;search next function
        jmp     ?bdos           ;return through bdos
;
;***********************************************************
;*                                                         *
;***********************************************************
delete: ;delete file (#19)
        ;1-> addr(fcb)
        call    getp2i          ;.fcb to DE
        mvi     c,deletf
        jmp     ?bdos           ;return through bdos
;
;***********************************************************
;*                                                         *
;***********************************************************
rdseq:  ;read file sequential mode (#20)
        ;1-> addr(fcb)
        call    getp2i          ;.fcb to DE
        mvi     c,readf
        jmp     ?bdos           ;return through bdos
;
;***********************************************************
;*                                                         *
;***********************************************************
wrseq:  ;write file sequential mode (#21)
        ;1-> addr(fcb)
        call    getp2i          ;.fcb to DE
        mvi     c,writf
        jmp     ?bdos           ;return through bdos
;
;***********************************************************
;*                                                         *
;***********************************************************
make:   ;create file (#22)
        ;1-> addr(fcb)
        call    getp2i          ;.fcb to DE
        mvi     c,makef
        jmp     ?bdos           ;return through bdos
;
;***********************************************************
;*                                                         *
;***********************************************************
rename: ;rename file (#23)
        ;1-> addr(fcb)
        call    getp2i          ;.fcb to DE
        mvi     c,renamf
        jmp     ?bdos           ;return through bdos
;
;***********************************************************
;*                                                         *
;***********************************************************
logvec: ;return login vector (#24)
        mvi     c,loginf
        jmp     ?bdos           ;return through BDOS
;
;***********************************************************
;*                                                         *
;***********************************************************
curdsk: ;return current disk number (#25)
        mvi     c,cdiskf
        jmp     ?bdos           ;return value in A
;
;***********************************************************
;*                                                         *
;***********************************************************
setdma: ;set DMA address (#26)
        ;1-> pointer (dma address)
        call    getp2           ;dma address to DE
        mvi     c,setdmf
        jmp     ?bdos           ;return through bdos
;
;***********************************************************
;*                                                         *
;***********************************************************
allvec: ;return address of allocation vector (#27)
        mvi     c,getalf
        jmp     ?bdos           ;return through bdos
;
;***********************************************************
;*                                                         *
;***********************************************************
wpdisk: ;write protect disk (#28)
        call    chkv20          ;must be 2.0 or greater
        mvi     c,wrprof
        jmp     ?bdos
;
;***********************************************************
;*                                                         *
;***********************************************************
rovec:  ;return read/only vector (#29)
        call    chkv20          ;must be 2.0 or greater
        mvi     c,getrof
        jmp     ?bdos           ;value returned in HL
;
;***********************************************************
;*                                                         *
;***********************************************************
filatt: ;set file attributes (#30)
        ;1-> addr(fcb)
        call    chkv20          ;must be 2.0 or greater
        call    getp2i          ;.fcb to DE
        mvi     c,setatf
        jmp     ?bdos
;
;***********************************************************
;*                                                         *
;***********************************************************
getdpb: ;get base of current disk parm block (#31)
        call    chkv20          ;check for 2.0 or greater
        mvi     c,getdpf
        jmp     ?bdos           ;addr returned in HL
;
;***********************************************************
;*                                                         *
;***********************************************************
getusr: ;get user code to register A
        call    chkv20          ;check for 2.0 or greater
        mvi     e,0ffh          ;to get user code
        mvi     c,userf
        jmp     ?bdos
;
;***********************************************************
;*                                                         *
;***********************************************************
setusr: ;set user code
        call    chkv20          ;check for 2.0 or greater
        call    getp1           ;code to E
        mvi     c,userf
        jmp     ?bdos
;
;***********************************************************
;*                                                         *
;***********************************************************
rdran:  ;read random (#33)
        ;1-> addr(fcb)
        call    chkv20          ;check for 2.0 or greater
        call    getp2i          ;.fcb to DE
        mvi     c,rdranf
        jmp     ?bdos           ;return through bdos
;
;***********************************************************
;*                                                         *
;***********************************************************
wrran:  ;write random (#34)
        ;1-> addr(fcb)
        call    chkv20          ;check for 2.0 or greater
        call    getp2i          ;.fcb to DE
        mvi     c,wrranf
        jmp     ?bdos           ;return through bdos
;
;***********************************************************
;*                                                         *
;***********************************************************
filsiz: ;compute file size (#35)
        call    chkv20          ;must be 2.0 or greater
        call    getp2           ;.fcb to DE
        mvi     c,filszf
        jmp     ?bdos           ;return through bdos
;
;***********************************************************
;*                                                         *
;***********************************************************
setrec: ;set random record position (#36)
        call    chkv20          ;must be 2.0 or greater
        call    getp2           ;.fcb to DE
        mvi     c,setrcf
        jmp     ?bdos           ;return through bdos
;
;***********************************************************
;*                                                         *
;***********************************************************
resdrv: ;reset drive function (#37)
        ;1->drive vector - bit(16)
        call    chkv22          ;must be 2.2 or greater
        call    getp2           ;drive reset vector to DE
        mvi     c,rsdrvf
        jmp     ?bdos           ;return through bdos
;
;***********************************************************
;*                                                         *
;***********************************************************
wrranz: ;write random, zero fill function
        ;1-> addr(fcb)
        call    chkv22  ;must be 2.2 or greater
        call    getp2i          ;.fcb to DE
        mvi     c,wrrnzf
        jmp     ?bdos
;
;***********************************************************
;*                                                         *
;***********************************************************
sgscb: ;set/get SCB byte/word
       ;1-> addr(SCB structure)
       call     getp2
       mvi      c,scbf
       jmp      ?bdos
;
;***********************************************************
;*                                                         *
;***********************************************************
        end
