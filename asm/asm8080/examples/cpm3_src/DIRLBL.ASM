;Function 100 RSX (set/create directory label
;       Only for Non banked systems
;
;       Procedure:
;               1. If this BDOS call ~= f100 then go to NEXT
;               2. select the current disk for BIOS calls
;               3. search for current label
;               4. if no label then do
;                  a. find first empty dir slot
;                  b. if no empties then return error
;                  c. create dir label from user FCB in DE
;                  d. call update SFCB
;                  e. return
;               5. if password protected then ok = password()
;               6. if ~ok then return error
;               7. update label from user info
;               8. call update SFCB
;               9. return
;
;                                       P. Balma

;
;                       RSX     PREFIX
;
serial:         db      0,0,0,0,0,0
jmp1:           jmp     ftest
NEXTj:          db      0c3h            ; next RSX or BDOS
NEXTa:          db      0,0             ; next address
prev:           dw      0               ; where from
remove:         db      0ffh            ; remove RSX at warm start
nbank:          db      0FFh            ; non banked RSX 
rsxname:        db      'DIRLBL  '
space:          dw      0
patch:          db      0
;
;
ftest:
        push a                          ;save user regs
        mov a,c
        cpi 64h                         ;compare BDOS func 100
        jz func100
        pop a                           ;some other BDOS call
goto$next:
        lhld NEXTa                      ; go to next and don't return
        pchl

        ; Set directory label
        ; de -> .fcb
        ;       drive location
        ;       name & type fields user's discretion
        ;       extent field definition
        ;               bit 1 (80h): enable passwords on drive
        ;               bit 2 (40h): enable file access         
        ;               bit 3 (20h): enable file update stamping
        ;               bit 4 (10h): enable file create stamping
        ;               bit 8 (01h): assign new password to dir lbl

func100:
        pop a
        lxi h,0 ! dad sp ! shld ret$stack       ; save user stack
        lxi sp,loc$stack

        xchg ! shld info ! xchg
        mvi c,19h ! call goto$next ! sta curdsk ; get current disk

        mvi c,1dh ! call goto$next              ; is drive R/O ?
        lda curdsk ! mov c,a ! call hlrotr
        mov a,l ! ani 01h ! jnz read$only

        lhld info ! call getexta ! push a       ; if user tries to set time
        ani 0111$0000b ! sta set$time           ; stamps and no SFCB's...error
        mov a,m ! ani 7fh ! mov m,a             ; mask off password bit
        ani 1 ! sta newpass                     ; but label can have password

        mvi c,69h ! push d ! lxi d,stamp        ; get time for possible
        call goto$next ! pop d                  ; update later

        mvi c,31h ! lxi d,SCBPB ! call goto$next; get BDOS current dma
        shld curdma

        lda curdsk ! call dsksel                ; BIOS select and sets
                                                ; disk parameters
                                                ; Does dir lbl exist on drive?
        call search                             ; return if found or
        push h ! mvi b,0                        ; successfully made
        lxi d,20h ! lda nfcbs ! mov c,a         ; Are there SFCB's in directory
 main0: dad d ! mov a,m ! cpi 21h ! jz main1
        inr b ! lda i ! inr a ! sta i ! cmp c
        jnz main0

        lda set$time ! ora a ! jnz no$SFCB      ; no, but user wants to set
                                                ; time stamp
        sta SFCB                                ; SFCB = false

 main1: shld SFCB$addr ! mov a,b ! sta j ! lhld info
        xchg ! pop h ! push h ! inx h           ; HL => dir FCB, DE => user FCB
        inx d ! mvi c,0ch                       ; prepare to move DE to HL
        call move ! lda newpass                 ; find out if new password ?
        ora a 
        cnz scramble                            ; scramble user pass & put in
                                                ; dFCB

        lda SFCB ! inr a ! jnz mainx1           ; any SFCB's


 main2:                                         ; update time & date stamp
        lda j ! mov b,a ! mvi a,2               ; j = FCB position from  SFCB
        sub b                                   ; in 4 FCB sector (0,1,2), thus
                                                ;  FCBx - 2
                                                ;  FCBy - 1
                                                ;  FCBz - 0
                                                ;  SFCB
                                                ; So, 2-j gives FCB offset in
                                                ; SFCB

        mvi b,0 ! mov c,a ! lhld SFCB$addr
        inx h ! lxi d,0ah ! inr c
mainx0: dcr c ! jz mainx1
        dad d ! jmp mainx0

mainx1: pop d ! push d ! push h                 ; HL => dFCB
        xchg ! lxi d,18h ! dad d                ; HL => dfcb(24) (TS field)
        xchg ! pop h ! push d                   ; of DIR LABEL
                                                ; HL => Time/stamp pos in SFCB
        lda NEW ! inr a ! jnz st0               ; did we create a new DL?
        call stamper ! jmp st1                  ; yes

 st0:   lxi d,4 ! dad d                         ; update time stamp
        pop d ! push h ! xchg ! lxi d,4         ; DFCB position
        dad d ! xchg ! pop h ! push d
 st1:   call stamper
        pop h

mainr: pop h ! call getexta ! ori 1 ! mov m,a  ; set lsb extent
        call write$dir
        xra a ! lxi h,0 !jmp goback             ; no SFCB, so finished


no$SFCB:
        mvi a,0ffh ! lxi h,0ffh ! jmp goback

read$only:
        mvi a,0ffh ! lxi h,02ffh 

goback: push h ! lhld aDIRBCB ! mvi m,0ffh      ; tell BDOS not to use buffer
                                                ; contents
        push a

        mvi c,0dh ! call goto$next              ; BDOS reset
        lda curdsk ! mov e,a ! mvi c,0eh
        call goto$next
        lda curdsk ! call seldsk                ; restore BDOS environment
        pop a ! pop d
        lhld ret$stack ! sphl                   ; restore user stack
        xchg                                    ; move error return to h
        ret


dsksel:                                 ; select disk and get parameters

        call seldsk                             ; Bios select disk
        call gethl                              ; DE = XLT addr
        shld XLT ! xchg
        lxi b,0ah ! dad b                       ; HL = addr DPB
        call gethl
        shld aDPB ! xchg
        lxi b,4 ! dad b                         ; HL = addr DIR BCB
        call gethl ! shld aDIRBCB
        lxi b,0ah ! dad b                       ; Hl => DIR buffer
        shld bufptr                             ; use BDOS buffer for
                                                ; BIOS reads & writes
                                                ; must jam FF into it to
                                                ; signal don't use when done
        lhld aDPB
        call gethl                              ; get [HL]
        shld spt ! xchg
        inx h! inx h! inx h ! inx h! inx h!     ; HL => dirmax
        call gethl ! shld dirmax ! xchg
        inx h ! inx h !
        call gethl ! shld checkv ! xchg
        call gethl ! shld offset ! xchg
                                                ; HL => phys shift
        call gethl ! xchg                       ; E = physhf, D = phymsk
        inr d ! mov a,d                         ; phys mask+1 = # 128 byte rcd
                                                ; phymsk * 4 = nfcbs/rcd
        ora a ! ral ! ora a ! ral               ; clear carry & shift phymsk
        sta nfcbs

        lhld spt                                ; spt = spt/phymsk
        mov c,e ! call hlrotr                   ; => spt = shl(spt,physhf)
        shld spt
        ret

search:                                         ; search dir for pattern in 
                                                ; info of length in c
        xra a ! sta sect ! sta empty
        lxi h,0 ! shld dcnt

        lhld bufptr ! mov b,h ! mov c,l         ; set BIOS dma
        call setdma

 src0:  call read$dir
        cpi 0 ! jnz oops                        ; if A ~= 0 then BIOS error

        mvi b,0 ! lda nfcbs ! mov c,a           ; BC always = nfcbs

        lhld bufptr ! lxi d,20h                 ; start of buffer and FCB
        xra a                                   ; do i = 0 to nfcbs - 1
 src1:  sta i ! mov a,m                         ; user #
        cpi 20h ! jnz src2                      ; dir label mark 

        push h ! lxi d,10h ! dad d ! mov a,m    ; found label, move to DM to
        ora a ! pop h ! rz                      ; check if label is pass prot
        push h ! cpi 20h ! pop h ! jnz checkpass
        ret

 src2:  lda empty ! inr a ! jz src3             ; record first sect with empty
        mov a,m
        cpi 0e5h ! jnz src3 ! lda sect          ; save sector #
        sta savsect ! mvi a,0ffh ! sta empty    ; set empty found = true 
 src3:  dad d                                   ; position to next FCB
        lda i ! inr a                           ; while i < nfcbs
        cmp c ! jnz src1

        lhld dirmax ! xchg ! lhld dcnt          ; while (dcnt < dirmax) &
                                                ; dir label not found 
        dad b ! shld dcnt ! call subdh          ; is dcnt <= dirmax ?
        jc not$found                            ; no
        lda sect ! inr a ! sta sect ! jmp src0

oops:   mvi a,0ffh ! lxi h,1ffh
        pop b ! jmp goback                      ; return perm. error

not$found:                                      ; must make a label

        lda empty ! inr a ! jnz no$space        ; if empty = false...
        lda savsect ! sta sect
        call read$dir                           ; get sector
        lhld bufptr ! lxi d,20h ! mvi c,0       ; C = FCB offset in buffer
 nf0:   mov a,m ! cpi 0e5h ! jz nf1
        dad d ! inr c !jmp nf0                  ; know that empty occurs here
                                                ; so don't need bounds test
 nf1:   mvi m,20h ! mov a,c ! sta i
        mvi a,0 ! push h ! mvi c,32             ; clear fcb to spaces
 nf2:   inx h ! dcr c ! jz nf3
        mov m,a ! jmp nf2
 nf3:   pop h 
        mvi a,0ffh ! sta NEW
        ret                                     ; HL => dir FCB

no$space: mvi a,0ffh ! lxi h,0ffh ! pop b ! jmp goback

check$pass:                     ; Dir is password protected, check dma for
                                ; proper password

        push h                          ; save addr dir FCB
        lxi d,0dh ! dad d ! mov c,m     ; get XOR sum in S1, C = S1
        lxi d,0ah ! dad d               ; position to last char in label pass
        mvi b,8                         ; # chars in pass
        xchg ! lhld curdma ! xchg       ; DE => user pass, HL => label pass

 cp0:   mov a,m ! xra c ! push b        ; HL = XOR(HL,C)
        mov c,a ! ldax d ! cmp c        ; compare user and label passwords
        jnz wrong$pass
        pop b ! inx d ! dcx h ! dcr b
        jnz cp0

        xchg ! shld curdma              ; curdma => 2nd pass in field if there
        pop h                           ; restore dir FCB addr
        mvi a,0ffh ! sta oldpass
        ret

wrong$pass:
        mvi a,0ffh ! lxi h,07ffh ! pop b ! pop b
        jmp goback

scramble:                               ; encrypt password at curdma
                                        ; 1. sum each char of pass.
                                        ; 2. XOR each char with sum
                                        ; 3. reverse order of encrypted pass

        lxi b,8 ! lhld curdma                   ;checkpass sets to 2nd pos if
        lda oldpass ! inr a ! jz scr0           ;old pass else must move dma
        dad b ! shld curdma
                                                ; B = sum, C = max size of pass
 scr0:  mov a,m ! add b ! mov b,a ! dcr c
        inx h ! jnz scr0


        pop d ! pop h ! push d                  ; H => dFCB, D was return 
        lxi d,0dh ! dad d ! mov m,b             ; S1 = sum
        lxi d,0ah ! dad d                       ; position to last char in pass
        mvi c,8 ! xchg ! lhld curdma
 scr1:  mov a,m ! xra b ! xchg ! mov m,a        ; XOR(char) => dFCB
        xchg ! inx h ! dcx d ! dcr c ! jnz scr1

        ret


read$dir:                               ; read directory into bufptr

        call track
        call sector
        call rdsec
        ret

writedir:                               ; write directory from bufptr
        lda sect
        call track
        call sector
        call wrsec
        ret

track:                                  ; set the track for the BIOS call

        lhld spt ! call intdiv                  ; E = integer(sect/spt)
        lhld offset ! dad d ! xchg ! call settrk
        ret

sector:                                 ; set the sector for the BIOS
        lda sect
        lhld spt ! call intdiv          ; get mod(sect,spt)
        mov a,c ! sub l                 ; D = x * spt such that D > sect
                                        ; D - spt = least x*spt s.t. D < sect
        mov c,a ! lda sect ! sub c      ; a => remainder of sect/spt
        mvi b,0 ! mov c,a ! lhld XLT    ; BC = logical sector #, DE = translate
        xchg ! call sectrn              ; table address
        xchg ! call setsec              ; BC = physical sector #
        ret


intdiv:                                 ; compute the integer division of A/L

        mvi c,0 ! lxi d,0
 int0:  push a                          ; compute the additive sum of L such
        mov a,l ! add c ! mov c,a       ; that C = E*L where C = 1,2,3,...
        pop a

        cmp C ! inr e ! jnc int0        ; if A < E*L then return E - 1
        dcr e
        ret

getexta:
                        ; Get current extent field address to hl
        lxi d,0ch ! dad d               ; hl=.fcb(extnum)
        mov a,m
        ret

move:                   ; Move data length of length c from source de to
                        ; destination given by hl

        inr c                                   ; in case it is zero
        move0:
                dcr c! rz                       ; more to move
                ldax d! mov m,a                 ; one byte moved
                inx d! inx h                    ; to next byte
                jmp move0

gethl:                                  ; get the word pointed at by HL
        mov e,m ! inx h ! mov d,m ! inx h
        xchg ! ret

subdh:                          ; HL = DE - HL

        ora a                                   ; clear carry
        mov a,e ! sub l ! mov l,a
        mov a,d ! sbb h ! mov h,a
        ret

hlrotr:
                                        ; rotate HL right by amount c
        inr c                                   ; in case zero
 hlr:   dcr c! rz                               ; return when zero
        mov a,h! ora a! rar! mov h,a            ; high byte
        mov a,l! rar! mov l,a                   ; low byte
        jmp hlr

stamper:                                ; move time stamp into SFCB & FCB
        lda SFCB ! inr a                ; no SFCB, update DL only
        cz stmp ! pop b ! pop d ! push h ! xchg
        push b ! call stmp ! pop b ! xchg ! pop h ! push d
        push b
        ret
stmp:   lxi d,stamp ! mvi c,4 ! call move
        ret

;**********************************************************************

curdsk:         db      0
set$time:       db      0
oldpass:        db      0
newpass:        db      0
pass$prot       db      0
sect:           db      0
empty:          db      0
stamp:          ds      4
NEW:            db      0
nfcbs:          db      0
i:              db      0
j:              db      0
SFCB:           db      0ffh
savsect:        db      0

SFCB$addr:      dw      0
info:           dw      0
checkv          dw      0
offset:         dw      0
XLT:            dw      0
bufptr:         dw      0
spt:            dw      0
dcnt:           dw      0
curdma:         dw      0
aDIRBCB         dw      0
aDPB:           dw      0
dFCB:           dw      0
dirmax:         dw      0

SCBPB:
Soff:           db      3ch
Sset:           db      0
Svalue:         dw      0

;
;***********************************************************
;*                                                         *
;*      bios calls from for track, sector io               *
;*                                                         *
;***********************************************************
;***********************************************************
;*                                                         *
;*        equates for interface to cp/m bios               *
;*                                                         *
;***********************************************************
;
;
base    equ     0
wboot   equ     base+1h ;warm boot entry point stored here
sdsk    equ     18h     ;bios select disk entry point
strk    equ     1bh     ;bios set track entry point
ssec    equ     1eh     ;bios set sector entry point
stdma   equ     21h
read    equ     24h     ;bios read sector entry point
write   equ     27h     ;bios write sector entry point
stran   equ     2dh     ;bios sector translation entry point
;
;***********************************************************
;*                                                         *
;***********************************************************
seldsk: ;select drive number 0-15, in C
        ;1-> drive no.
        ;returns-> pointer to translate table in HL
        mov c,a         ;c = drive no.
        lxi d,sdsk
        jmp gobios
;
;***********************************************************
;*                                                         *
;***********************************************************
settrk: ;set track number 0-76, 0-65535 in BC
        ;1-> track no.
        mov b,d
        mov c,e         ;bc = track no.
        lxi d,strk
        jmp gobios
;
;***********************************************************
;*                                                         *
;***********************************************************
setsec: ;set sector number 1 - sectors per track
        ;1-> sector no.
        mov b,d
        mov c,e         ;bc = sector no.
        lxi d,ssec
        jmp gobios
;
;***********************************************************
;*                                                         *
;***********************************************************
rdsec:  ;read current sector into sector at dma addr
        ;returns in A register: 0 if no errors 
        ;                       1 non-recoverable error
        lxi d,read
        jmp gobios
;***********************************************************
;*                                                         *
;***********************************************************
wrsec:  ;writes contents of sector at dma addr to current sector
        ;returns in A register: 0 errors occured
        ;                       1 non-recoverable error
        lxi d,write
        jmp gobios
;
;***********************************************************
;*                                                         *
;***********************************************************
sectrn: ;translate sector number
        ;1-> logical sector number (fixed(15))
        ;2-> pointer to translate table
        ;returns-> physical sector number
        push d
        lxi d,stran
        lhld wboot
        dad d           ;hl = sectran entry point
        pop d
        pchl
;
;
setdma:                         ; set dma
        ; 1 -> BC = dma address

        lxi d,stdma
        jmp gobios
;
;
;***********************************************************
;***********************************************************
;***********************************************************
;*                                                         *
;*       compute offset from warm boot and jump to bios    *
;*                                                         *
;***********************************************************
;
;
gobios: ;jump to bios entry point
        ;de ->  offset from warm boot entry point
        lhld    wboot
        dad     d
        lxi     d,0
        pchl
;

ret$stack:      dw      0
                ds      32
loc$stack:
end

