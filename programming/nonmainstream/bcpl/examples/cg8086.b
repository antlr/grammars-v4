/* This is a BCPL Codegenerator for the 8086, based on code
   supplied by Graham Adams, Rutherford Laboratories in 1982.

   It is being modified by Martin Richards to run under the
   BCPL Cintcode System as a cross compiler.

Current status: (6 March 1998)
   It compiles but does not run (for many reasons).
   It requires a lot more work.....
   It may be of no use to anyone.....

Usage: To compile cg8086:             c bp cg8086
       To create a small OCODE file:  c bp primes
       To generate 8086 code:         cg8086
*/

SECTION "CG8086"
 
GET "libhdr"
 
GLOBAL {
profiling:464
progsize:493;       op:494;             tempv:495
tempt:496;          brefv:497;          brefp:498
procstk:499;        procstkp:200;       reg_k:201
reg_n:202;          labv:203;           dp:204
paramnumber:205;    stv:206;            stvp:207
initdatalists:208;  cgerror:209;        rdn:210
rdl:211;            incode:212;         return_set:213
maxgn:214;          maxlab:215;         maxssp:216
initstack:217;      code:218;           cgname:219
scan:220;           even:221;           outputsection:222
checkparam:223;     rdgn:224;           nextparam:225
                    arg1:227;           arg2:228
pendingop:229;      ssp:230;            stack:231
store:232;          storet:233;         loadt:244
cgstring:245;       cgstind:246;        cgrv:247
cgpendingop:248;    regset:249;         genbranch:250
cgjump:251;         getvalue:252;       genj:253
cglab:254;          cgentry:255;        cgsave:256
codeb:257;          cgapply:258;        cgreturn:259
pwabo:260;          movetor:261;        cgglobcall:262
cgswitch:263;       cgbyteap:264;       cgglobal:265
cgdata:266;                             movetoanyr:268
genrs:269;          checkbrefs:270;     gend:271
genrd:272;          genkd:273;          lookinfreeregs:274
condbrfn:275;       nextrset:276;       cgcmp:277
lose1:278;          cgplus:279;         cgminus:280
numberis:281;       genshift:282;       freereg:283
genmuldiv:284;      genf:285;           cglogorandneqv:286
inset:287;          cgshiftk:288;       genmov:289
forgetall:290;      movetoindexr:291;   movetoindexrsh:292
lookinregs:293;     setr:294;           setinfo:295
isfree:296;         nextr:297;          regusedby:298
nextrforceset:299;  consb2:300;         forgetvars:301
storein:302;        genbytexchg:303;    genclearbyte:304
cgaddk:305;         genincdec:306;      cgdiadwithr:307
cgstatics:308;      genbrefjumps:309;   setlab:310
indcode:311;        tranr:312;          casek:313
casel:314;          brlabref:315;       addr_m:316
addr_v:317;         codekd:318;         coderd:319
coders:320;         coded:321;          remem:322
forget:323;         formsaddr:324;      formdaddr:325
codedisp:326;       formaddr:327;       checkspace:328
smallnum:329;                           labref:331
refliste:332;       dliste:333;         reflist:334
dlist:335;          addabo:336;
objword:338
                    iseven:342;         getvbyte:343
                    putvbyte:345;       insertcount:346
genint:347
dsize:349

// Added by MR (6/3/98)
ocodefile     : 500
codefile      : 501
cgworksize    : 502
stkchking     : 503
callcounting  : 504
naming        : 505
debugging     : 506
cglisting     : 507
verstream     : 508
getbyte       : 509
putbyte       : 510
aptovec       : 511
codestream    : 512
ocodestream   : 513
writewcount   : 514
ordch         : 515
listchan      : 516
savespacesize : 517
err_p         : 518
err_l         : 519
dboutput      : 520
listcnt       : 521
listat        : 522
rc            : 523
stringrb      : 524
listgen       : 525
listl         : 526
sf            : 527
listlab       : 528
datvec        : 529
askii         : 530

       }
 
MANIFEST {
 // BITS OF OPCODES
 // JUMPS
f_jge = #B01111101
f_jg  = #B01111111
f_jle = #B01111110
f_jl  = #B01111100
f_je  = #B01110100
f_jne = #B01110101
f_jmp = #B11101011
f_b   = f_jmp
f_ji  = #B100    // INDIRECT JUMP
f_jis = #B101    //    "      "   INTERSEGMENT
f_calli = #B010  // INDIRECT CALL
f_cis   = #B011  //    "      "   INTERSEGMENT
 // MONADIC OPERATORS REPRESENTED BY 11 BIT QUANTITIES
f_neg = #B11110111011
f_not = #B11110111010
 // SHIFT OPERATORS REP. BY 11 BIT QUANTITIES
f_shl = #B11010001100
f_shr = #B11010001101
f_sar = #B11010001111
 // NOP I.E. XCHG AX,AX
f_nop = #B10010000
 // MOVES
f_movimmtrm = #B11000111
f_movrtrm = #B10001001
f_movimmtr = #B10111000
f_lea = #B10001101
 // CONVERT WORD IN AX TO DOUBLEWORD IN AX AND DX FOR DIVISION
f_cwd = #B10011001
 // MULTIPLY AND DIVIDE
f_imul = #B101
f_idiv = #B111
 // DIADICS
f_xor = #B110
f_or  = #B001
f_and = #B100
f_add = #B000
f_sub = #B101
f_cmp = #B111
 //
f_inc = #B000
f_dec = #B001
 // BYTE OPERATIONS
f_xchgb = #B10000110
f_clrb  = #B00110000   // THIS IS XOR REG,REG
 // SOFTWARE INTERRUPT WITH TYPE GIVEN
f_int   = #B11001101
 //
 // REGISTERS
r0 = 0; r1 = 1; r2 = 2; r3 = 3; r4 = 4; r5 = 5; r6 = 6; r7 = 7
r_bx = r0
r_ax = r1
r_cx = r2
r_dx = r3
r_si = r4
r_bp = r5
r_sp = r6
r_di = r7
 // SETS
v_xx  = #B11111111
v_r0  = #B00000001
v_r1  = #B00000010
v_ri  = #B00010001
v_ax  = #B00000010
v_r80 = #B00001111
v_xmuldiv = #B00010101
 //
 // TYPES
k_lab = 3
k_numb = 4
k_lvloc = 5
k_lvglob = 6
k_lvlab = 7
k_reg = 8
k_mloc = 9
k_mlab = 10
k_mglob = 11
k_none = 12
k_roff = 14
k_loc = r_bp + k_roff
k_glob = r_di + k_roff
k_xbx = r_bx + k_roff
k_xsi = r_si + k_roff
 // M/C TYPES
m_loc  = #B110
m_glob = #B101
m_bx   = #B111
m_si   = #B100
m_bp   = #B110
m_di   = #B101
m_lab  = 1
m_reg  = 2
m_imm  = 3
 // GLOBALS USED
gn_stop       = 2
gn_ipdump     = 8
 //
 // INTERRUPT TYPES
i_stkcheck    = 33
 //
 // OBJECT MODULE TYPES
t_hunk   = 1000
t_end    = 1002
t_relocb = 1005
 //
secword = 12345

// OCODE keywords.
s_true=4; s_false=5; s_rv=8; s_fnap=10
s_mult=11; s_div=12; s_rem=13
s_plus=14; s_minus=15; s_query=16; s_neg=17; s_abs=19
s_eq=20; s_ne=21; s_ls=22; s_gr=23; s_le=24; s_ge=25
s_not=30; s_lshift=31; s_rshift=32; s_logand=33
s_logor=34; s_eqv=35; s_neqv=36
s_lf=39; s_lp=40; s_lg=41; s_ln=42; s_lstr=43
s_ll=44; s_llp=45; s_llg=46; s_lll=47
s_needs=48; s_section=49
s_rtap=51; s_goto=52; s_finish=68
s_switchon=70; s_global=76; s_sp=80; s_sg=81; s_sl=82; s_stind=83
s_jump=85; s_jt=86; s_jf=87; s_endfor=88
s_lab=90; s_stack=91; s_store=92; s_rstack=93; s_entry=94
s_save=95; s_fnrn=96; s_rtrn=97; s_res=98
s_datalab=100; s_itemn=102; s_endproc=103; s_none=111
s_getbyte=120; s_putbyte=121

s_return=67
s_local=77; s_label=78
s_iteml=101
 
s_debug=109; s_none=111

 //
 // SELECTORS
h1 = 0; h2 = 1; h3 = 2; h4 = 3; h5 = 4; h6 = 5
}
 
 
 
 /* Improvements May 1982, removal of redundant branches, more use of
    8080 half registers.
 
    Bug fixed, 6th August 1982, comparison MANIFEST=MANIFEST generated
    bad code.
 */
 
/* Moved to 4090, November 1982. Changed reading of parms, date and
   allocation of work space.
*/
 
 /* THIS IS THE BCPL CODE GENERATOR FOR THE INTEL 8086 MICROPROCESSOR,
    (FOR THE  1M (MAX) VERSION OF TRIPOS).
    IT HAS BEEN ADAPTED FROM THAT FOR THE PDP11
    ( COPYRIGHT TRIPOS RESEARCH GROUP, CAMBRIDGE ).
    THIS VERSION ASSUMES ES=DS=SS.
    EACH SECTION STARTS IN A NEW CODE SEGMENT SETTING.
    THE 8086 REGISTERS ARE USED AS FOLLOWS:-
    . ES, DS AND SS ARE NOT CHANGED, CS CHANGES WITH SECTION
    . IP IS THE INSTRUCTION POINTER WHICH IS CHANGED ONLY IMPLICITLY
      BY NORMAL EXECUTION OF INSTRUCTIONS
    . SP IS THE MACHINE STACK POINTER USED BY CALL,RET,PUSH,POP ETC
    . BP IS THE BCPL STACK FRAME BASE POINTER
    . DI HOLDS THE ADDRESS OF THE GLOBAL VECTOR
    . THE OTHER 5 REGISTERS ARE USED FOR TEMPORARY STORAGE OF
      DATA ITEMS. FOR MANY OPERATIONS ANY OF THESE REGISTERS MAY BE
      USED, BUT FOR OTHERS OPERANDS ARE RESTRICTED TO ONE OR TWO
      POSSIBLE REGISTERS.
    . R0 = R_BX = REGISTER BX GENERAL, INDEX, FUNCTION AND VALOF RESULT, ARG1
      R1 = R_AX = REGISTER AX GENERAL, MULTIPLICAND, LS PRODUCT,
                              LS DIVIDEND, ARG2
      R2 = R_CX = REGISTER CX GENERAL, SHIFT COUNTER, ARG3
      R3 = R_DX = REGISTER DX GENERAL, REMAINDER, MS DIVIDEND, SIZE OF
                  CALLING ROUTINE'S FRAME
      R4 = R_SI = REGISTER SI GENERAL, INDEX, ADDRESS OF CALLED ROUTINE'S
                  DESCRIPTOR
      R5 = R_BP = REGISTER BP
      R6 = R_SP = REGISTER SP
      R7 = R_DI = REGISTER DI
                                                            */
 
 /* THE FOLLOWING STREAMS ARE USED FOR THE CROSS-COMPILER VERSION
     SYSPRINT (OUT)      HEADING, DEBUGGING AND ERROR INFO
     OCODE    (IN)       OCODE FROM PHASE 1
     CODE     (OUT)      A REPRESENTATION OF THE COMPILED M/C CODE
 */
 
 
 
 /* THE CROSS VERSION CROSS-COMPILES ON 360/370/303X IBM MACHINES   */
 /* (UNDER K. MOODY'S IMPLEMENTATION OF BCPL)                  */
 
//SECTION "CG1"
//GET "CGHDR"

//******************** Fiddle --- Added by MR 

LET getbyte(s,i) = s%i

LET putbyte(s,i,ch) BE s%i := ch

//******************** End of fiddle

LET start() = VALOF
{  LET v = 0
 //*<3032
    LET sysp = output()
    //LET vda = VEC 11
    LET vda = "   06-MAR-1998"
    LET parms = VEC 10
 
    writes("Cg8086 (6 Mar 98)*n") // Add by MR
    //gettime(vda, 24)
    
    ocodefile := "OCODE"
    codefile := "CODE"
    cgworksize := 5000
    datvec := getvec(14)

    FOR i = 1 TO 11 DO putbyte(datvec,i,askii(getbyte(vda,i+3)))
    putbyte(datvec,3,askii('-'))
    putbyte(datvec,7,askii('-'))
    putbyte(datvec,5,getbyte(datvec,5) | #X20)
    putbyte(datvec,6,getbyte(datvec,6) | #X20)  // TO LOWER CASE
    putbyte(datvec,8,getbyte(datvec,10))
    putbyte(datvec,9,getbyte(datvec,11))        // GET RID OF '19'
    profiling := FALSE
    stkchking := FALSE
    callcounting := FALSE
    naming := TRUE
    debugging := FALSE
    cglisting := FALSE
    IF sysp=0 THEN
    { //WRITETOLOG("NO PRINT STREAM SELECTED - CG ABORTED")
       RESULTIS 16
    }
    selectoutput(sysp)
    verstream := sysp
 
/*
    { // get parms from a file
       LET ps = findinput("PARMS")
       IF ps=0 DO
       { writes("Can't find PARMS file.*N")
          RESULTIS 20
       }
 
       selectinput(ps)
 
       { LET i = 0
          LET ch = rdch()
 
          UNTIL ch=endstreamch | ch='*N' DO
          { i := i+1
             IF i>20 DO { writes("Too many parms!*N"); stop(30) }
             parms%i := ch
             ch := rdch()
          }
 
          parms%0 := i
          endread()
       }
 
    }
*/
    parms := "L W30000" // Added by MR

    FOR i = 1 TO getbyte(parms, 0) DO
     SWITCHON getbyte(parms, i) INTO
     {  DEFAULT:  LOOP
         CASE 'P': profiling := TRUE
         CASE 'K': callcounting := TRUE; LOOP
         CASE 'N': naming := FALSE; LOOP
         CASE 'C': stkchking := TRUE; LOOP
         CASE 'D': debugging := TRUE; LOOP
         CASE 'L': cglisting := TRUE; LOOP
         CASE 'W': cgworksize := 0
                   { LET ch = ?
                      UNLESS i+1<=getbyte(parms, 0) DO BREAK
                      ch := getbyte(parms, i+1)
                      UNLESS '0'<=ch<='9' DO BREAK
                      cgworksize := cgworksize*10 + ch - '0'
                      i := i+1
                   } REPEAT
     }
 /*3032>*/
 
    err_p, err_l := level(), fail
    writes("RAL 8086 cg*N")
 //*<3032
    codestream := findoutput(codefile)
    IF codestream=0 DO cgerror("CAN'T OPEN %S", TRUE, codefile)
 /*3032>*/
    ocodestream := findinput(ocodefile)
    IF ocodestream=0 DO cgerror("CAN'T OPEN %S", TRUE, ocodefile)
    selectinput(ocodestream)
    v := getvec(cgworksize)
    IF v=0 DO cgerror("CAN'T GET WORKSPACE OF %N", TRUE, cgworksize)
    IF cgworksize<500 DO cgerror("GIVE MORE WORKSPACE TO CG", TRUE)
    progsize := 0
 //*<3032
    writewcount := 0
     listchan := TRUE
     IF cglisting DO
     { ordch := rdch
        rdch := myrdch
     }
 /*3032>*/
l:  op := rdn()
    IF op=0 GOTO x
    FOR i = 0 TO cgworksize DO v!i := 0
    tempv, tempt := v, v+150
    brefv, brefp := tempt, tempt
    procstk, procstkp := brefv+129, 0
    reg_k := procstk+20
    reg_n := reg_k+5
    labv := reg_n+5
    dp := v+cgworksize
    paramnumber := (dp-labv)/10 + 10
    stv := labv+paramnumber
    FOR p = labv TO stv-1 DO !p := -1
    forgetall()    // INIT. REGISTERS
    stvp := 0
    initdatalists()
    incode := FALSE
    maxgn := 0
    maxlab := 0
    maxssp := 0
    return_set := 0
    initstack(savespacesize)
    code(0, 0)  // FIRST WORD WILL HOLD NO. OF WORDS OF CODE COMPILED
    TEST op=s_section THEN
    { cgname(s_section, rdn())
       op := rdn()
    }
    OR cgname(s_section, 0)
    scan()
    even()
    UNLESS maxgn=0 DO outputsection()
    progsize := progsize + stvp
    GOTO l
 
x:  writef("PROGRAM SIZE %N BYTES*N", progsize)
fail: UNLESS v=0 DO freevec(v)
    UNLESS ocodestream=0 DO endread()
    ocodestream := 0
 //*<3032
    UNLESS rc=0 RESULTIS rc
 /*3032>*/
}
 
 
 
 //*<3032
AND myrdch() = VALOF
{   LET ch = ordch()
 
     IF listchan DO
     {   writes("*N** ")
          listcnt := 0
          listchan := FALSE
     }
 
     IF ch=endstreamch RESULTIS 0
 
     wrch(ch='*N'->'*S',ch)
     listcnt := listcnt + 1
     IF listcnt>60 DO listchan := TRUE
     RESULTIS ch
}
 /*3032>*/
 
AND rdn() = VALOF
    { LET a, sign = 0, FALSE
       LET ch = ?
 
       ch := rdch() REPEATWHILE
          ch='*S' | ch='*N' | ch='L'
 
       IF ch=endstreamch RESULTIS 0
 
       IF ch='-' DO { sign := TRUE
                       ch := rdch()  }
 
       WHILE '0'<=ch<='9' DO { a := 10*a + ch - '0'
                                ch := rdch()  }
 
       IF sign DO a := -a
       RESULTIS a
    }
 
// READ IN AN OCODE LABEL
AND rdl() = VALOF
    { LET l = rdn()
       IF maxlab<l DO
       { maxlab := l
          checkparam() }
       RESULTIS l  }
 
// READ IN A GLOBAL NUMBER
AND rdgn() = VALOF
    { LET g = rdn()
       IF maxgn<g DO maxgn := g
       RESULTIS g  }
 
 
 
// GENERATE NEXT LABEL PARAMETER
AND nextparam() = VALOF
    { paramnumber := paramnumber-1
       checkparam()
       RESULTIS paramnumber  }
 
 
 
AND checkparam() BE
       IF maxlab>=paramnumber DO
       { cgerror("TOO MANY LABELS -*
                  * INCREASE WORKSPACE", TRUE) }
 
 
 
AND cgerror(n,f,a) BE
    { writes("*NERROR. ")
       writef(n,a)
       newline()
       rc := 10
       IF f DO
       { rc := 20
          longjump(err_p, err_l) }
    }
 

 
 

//SECTION "CG2"
 
//GET "CGHDR"
 
// INITIALISE THE SIMULATED STACK (SS)
LET initstack(n) BE
    { arg2, arg1 := tempv, tempv+3
       ssp := n
       pendingop := s_none
       h1!arg2, h2!arg2, h3!arg2 := k_loc, ssp-2, ssp-2
       h1!arg1, h2!arg1, h3!arg1 := k_loc, ssp-1, ssp-1
       IF maxssp<ssp DO maxssp := ssp  }
 
 
 
// MOVE SIMULATED STACK (SS) POINTER TO N
AND stack(n) BE
{ IF maxssp<n DO maxssp := n
    IF n>ssp+savespacesize DO // I wonder how this works
        { store(0,ssp-1)
           initstack(n)
           RETURN   }
 
    WHILE n>ssp DO loadt(k_loc, ssp)
 
    UNTIL n=ssp DO
    { IF arg2=tempv DO
       { TEST n=ssp-1
            THEN { ssp := n
                    h1!arg1,h2!arg1 := h1!arg2,h2!arg2
                    h3!arg1 := ssp-1
                    h1!arg2,h2!arg2 := k_loc,ssp-2
                    h3!arg2 := ssp-2
                 }
            ELSE initstack(n)
          RETURN
       }
 
       arg1, arg2 := arg1-3, arg2-3
       ssp := ssp-1
    }
}
 
 
 
 
 
// STORE ALL SS ITEMS FROM A TO B IN THEIR TRUE
// LOCATIONS ON THE STACK
AND store(a,b) BE FOR p = tempv TO arg1 BY 3 DO
    { LET s = h3!p
       IF s>b RETURN
       IF s>=a DO storet(p)  }
 
 
 
 //*<3032
 // THE LINE BELOW WILL BE REMOVED FOR THE TRIPOS VERSION
 AND testflags(n) = FALSE
 /*3032>*/
 
 
 
AND scan() BE
 
{  IF testflags(1) DO cgerror("BREAK", TRUE)
 
    SWITCHON op INTO
 
 {   DEFAULT:     cgerror("BAD OP %N", FALSE, op)
                   ENDCASE
 
      CASE 0:      RETURN
 
 //*<3032
      CASE s_debug:debugging := NOT debugging
                   ENDCASE
 /*3032>*/
 
      CASE s_lp:   loadt(k_loc, rdn()); ENDCASE
      CASE s_lg:   loadt(k_glob, rdgn()); ENDCASE
      CASE s_ll:   loadt(k_lab, rdl()); ENDCASE
      CASE s_ln:   loadt(k_numb, rdn()); ENDCASE
 
      CASE s_lstr: cgstring(rdn()); ENDCASE
 
      CASE s_true: loadt(k_numb, -1); ENDCASE
      CASE s_false:loadt(k_numb, 0); ENDCASE
 
      CASE s_llp:  loadt(k_lvloc, rdn()); ENDCASE
      CASE s_llg:  loadt(k_lvglob, rdgn()); ENDCASE
      CASE s_lll:  loadt(k_lvlab, rdl()); ENDCASE
 
      CASE s_sp:   storein(k_loc, rdn()); ENDCASE
      CASE s_sg:   storein(k_glob, rdgn()); ENDCASE
      CASE s_sl:   storein(k_lab, rdl()); ENDCASE
 
      CASE s_stind:cgstind(); ENDCASE
 
      CASE s_rv:   cgrv(); ENDCASE
 
      CASE s_mult:CASE s_div:CASE s_rem:
      CASE s_plus:CASE s_minus:
      CASE s_eq: CASE s_ne:
      CASE s_ls:CASE s_gr:CASE s_le:CASE s_ge:
      CASE s_lshift:CASE s_rshift:
      CASE s_logand:CASE s_logor:CASE s_eqv:CASE s_neqv:
      CASE s_not:CASE s_neg:CASE s_abs:
                   cgpendingop(regset(op))
                   pendingop := op
                   ENDCASE
 
      CASE s_jump: cgpendingop(v_xx)
                   store(0, ssp-1)
                   genbranch(f_jmp, rdl())
                   ENDCASE
 
      CASE s_endfor:
                   cgpendingop(v_xx)
                   pendingop := s_le
      CASE s_jt:   cgjump(TRUE, rdl())
                   ENDCASE
 
      CASE s_jf:   cgjump(FALSE, rdl())
                   ENDCASE
 
      CASE s_goto: cgpendingop(v_xx)
                   store(0, ssp-2)
                   getvalue(arg1)
                   IF h1!arg1=k_numb THEN
                      h2!arg1 := h2!arg1 * 2
                   movetoindexr(arg1)
                      // CS same as now so can do JI (instead of JIS)
                   genj(f_ji,h2!arg1=r_bx->k_xbx, k_xsi, 0)
                   stack(ssp-1)
                   ENDCASE
 
      CASE s_lab:  cgpendingop(v_xx)
                   store(0, ssp-1)
                   cglab(rdl())
                   ENDCASE
 
      CASE s_query:cgpendingop(v_xx)
                   stack(ssp+1)
                   ENDCASE
 
      CASE s_stack:cgpendingop(v_xx)
                   stack(rdn())
                   ENDCASE
 
      CASE s_store:cgpendingop(v_xx)
                   store(0, ssp-1)
                   ENDCASE
 
      CASE s_entry:
                cgentry(rdn(), rdl())
                ENDCASE
 
      CASE s_save: cgsave(rdn())
                   IF stkchking DO
                   { IF procstkp>=20 DO
                         cgerror("PROC STACK OVF", TRUE)
                      procstk!procstkp := maxssp
                      genint(i_stkcheck)
                      procstk!(procstkp+1) := stvp
 //*<3032
                      listat(s_itemn, 0)
 /*3032>*/
                      code(0,0) // MAXSSP OF CALLED ROUTINE IS PLACED HERE
                      maxssp := ssp }
                   procstkp := procstkp+2
                   ENDCASE
 
      CASE s_fnap:
      CASE s_rtap: cgapply(op, rdn())
                   ENDCASE
 
      CASE s_rtrn:
      CASE s_fnrn: cgreturn(op)
                   ENDCASE
 
      CASE s_endproc:
                   rdn()
                   procstkp := procstkp-2
                   IF stkchking  DO
                   { pwabo(stv, procstk!(procstkp+1), maxssp)
                      maxssp := procstk!procstkp }
                   ENDCASE
 
      CASE s_res:  cgpendingop(v_r0)
                   store(0, ssp-2)
                   movetor(arg1,r0)
                   genbranch(f_jmp, rdl())
                   stack(ssp-1)
                   ENDCASE
 
      CASE s_rstack:
                   initstack(rdn())
                   loadt(k_reg, r0)
                   ENDCASE
 
      CASE s_finish:
                   loadt(k_numb, 0)
                   loadt(k_numb, 0)
                   cgglobcall(gn_stop)
                   ENDCASE
 
      CASE s_switchon:
                   cgswitch(rdn())
                   ENDCASE
 
      CASE s_getbyte:
      CASE s_putbyte:
                   cgbyteap(op)
                   ENDCASE
 
      CASE s_global:
                   cgglobal(rdn())
                   RETURN
 
      CASE s_datalab:
      CASE s_iteml:cgdata(op, rdl()); ENDCASE
      CASE s_itemn:cgdata(op, rdn()); ENDCASE
 }
 
 //*<3032
    IF debugging DO dboutput()
 /*3032>*/
    op := rdn()
 
} REPEAT
 

 
//SECTION "CG3"
 
//GET "CGHDR"
 
// COMPILES CODE TO DEAL WITH ANY PENDING OP
LET cgpendingop(set) BE
 
{  LET r = -1
    LET sw = FALSE
    LET pendop = pendingop
    LET num1 = h1!arg1=k_numb
    LET kk = h2!arg1
    LET rand1,rand2 = arg1,arg2
 
    pendingop := s_none
 
    SWITCHON pendop INTO
    {   CASE s_abs:
                 r := movetoanyr(arg1,set)
                 genrs(f_or, r, k_reg, r)
                 checkbrefs(6)
 //*<3032
                 listl("JGE $+4")
 /*3032>*/
                 codeb(f_jge)
                 codeb(2)
         CASE s_neg:
                 sw := TRUE
         CASE s_not:
                 r := movetoanyr(arg1, set)
                 gend(sw->f_neg,f_not, k_reg, r)
         CASE s_none:
                 RETURN
    }
 
    getvalue(arg1)
    getvalue(arg2)
 
    IF h1!arg2=k_numb | h1!arg1=k_reg |
          lookinfreeregs(arg1)>=0 DO
       // SWOP OPERANDS FOR SYMETRIC OPS
       rand1,rand2 := arg2,arg1
 
    SWITCHON pendop INTO
 
    {   DEFAULT:cgerror("BAD PNDOP %N",FALSE,pendop)
                 RETURN
 
         CASE s_eq: CASE s_ne:
         CASE s_ls: CASE s_gr:
         CASE s_le: CASE s_ge:
                 // COMPARISONS ARE ARG2 <OP> ARG1
              { LET f = condbrfn(pendop)
                 r := nextrset(-1,set)
                 genrs(f_xor, r, k_reg, r)
                 f := cgcmp(FALSE,f,r)   // EXCLUDE R
                 checkbrefs(6)
 //*<3032
                 listl("%S $+4", sf(f))
 /*3032>*/
                 codeb(f)
                 codeb(2)
                 gend(f_not, k_reg, r)
                 ENDCASE }
 
         CASE s_eqv:
                 sw := TRUE
         CASE s_neqv:
                 r := movetoanyr(arg2,set)
                 cglogorandneqv(f_xor, arg1, k_reg, r)
                 IF sw DO gend(f_not, k_reg, r)
                 ENDCASE
 
         CASE s_plus:
                 IF num1 & h1!arg2=k_numb DO
                 { lose1(k_numb, kk+h2!arg2)
                    RETURN  }
 
                 r := movetoanyr(rand2,set)
                 cgplus(rand1,k_reg,r)
                 ENDCASE
 
         CASE s_minus:
                 r := movetoanyr(arg2,set)
                 cgminus(arg1,k_reg,r)
                 ENDCASE
 
         CASE s_mult:
                 IF numberis(2,rand1) | numberis(4,rand1) DO
                 { r := movetoanyr(rand2,set)
                    FOR i = 1 TO (h2!rand1)/2 DO
                     genshift(f_shl, 1, k_reg, r)
                    ENDCASE  }
 
                 movetor(rand2, r_ax)
                 freereg(r_dx, rand1)
                 IF h1!rand1=k_numb DO
                 { LET r = lookinregs(k_numb, h2!rand1)
                    IF r<0 DO r := nextrforceset(r_ax, v_xmuldiv)
                    movetor(rand1, r)
                 }
                 forget(k_reg, r_dx)
                 genmuldiv(f_imul, rand1)
                 r := r_ax
                 ENDCASE
 
         CASE s_div:
                 sw := TRUE
         CASE s_rem:
                 freereg(r_dx, -1)
                 movetor(arg2, r_ax)
                 forget(k_reg, r_dx)
                 genf(f_cwd)
                 IF h1!arg1=k_numb DO
                 { LET r = lookinregs(k_numb, h2!arg1)
                    IF r<0 DO r := nextrforceset(r_ax, v_xmuldiv)
                    movetor(arg1, r)
                 }
                 genmuldiv(f_idiv, arg1)
                 r := sw -> r_ax, r_dx
                 ENDCASE
 
          CASE s_logor:
                 sw := TRUE
          CASE s_logand:
                 r := movetoanyr(rand2,set)
                 cglogorandneqv(sw->f_or,f_and,rand1,k_reg,r)
                 ENDCASE
 
          CASE s_lshift:
                 sw := TRUE
          CASE s_rshift:
                 IF num1 DO
                    IF kk=1 | kk=2 | kk=8 DO
                    { r := movetoanyr(arg2,set)
                       UNLESS kk=8 & ~inset(v_r80, r) DO
                       { cgshiftk(sw,kk,k_reg,r)
                          ENDCASE
                       }
                    }
 
                       movetor(arg1, r_cx)    // SHIFT COUNTER
                       TEST h1!arg2=k_reg DO r := h2!arg2
                        OR { r := lookinfreeregs(h1!arg2, h2!arg2)
                              TEST r>=0 DO h1!arg2, h2!arg2 := k_reg, r
                                        OR { r := nextrset(r_cx, set)
                                              movetor(arg2, r)
                                           }
                            }
                       genshift(sw -> f_shl, f_shr,0,k_reg,r)
                       ENDCASE
 
    }
 
    lose1(k_reg, r)
}
 
 
 
 
 
// GET A SET FOR AN OPERATOR
AND regset(p) = VALOF
  SWITCHON p INTO
  { DEFAULT:     RESULTIS v_xx
     CASE s_mult:
     CASE s_div:
     CASE s_rem:  RESULTIS v_ax
     CASE s_rv:
     CASE s_putbyte:
     CASE s_getbyte:
                  RESULTIS v_ri
  }
 
// COMPILES A GLOBAL CALL FOR OUT OF
// LINE FUNCTIONS
AND cgglobcall(gn) BE
    { cgpendingop(v_r1)
       store(0,ssp-3)
       movetor(arg2, r0)
       movetor(arg1, r1)
       stack(ssp-2)
       freereg(r_dx,-1)
       genmov(k_numb, 2*ssp, k_reg, r_dx)
       genmov(k_glob, gn, k_reg, r_si)
       genj(f_cis, k_xsi, 0)
       forgetall()
    }
 
 
 
AND numberis(n,a) =
       h1!a=k_numb & h2!a=n
 

 
//SECTION "CG4"
 
//GET "CGHDR"
 
// MAKE ANY LVALUES ADDRESSABLE - IE GET THEM
// INTO A REGISTER
LET getvalue(a) BE
       IF h1!a=k_lvloc | h1!a=k_lvglob | h1!a=k_lvlab DO
          movetoindexr(a)
 
 
 
// MOVE A SS ITEM INTO AN INDEX REGISTER AND SHIFT
// IT LEFT FOR USE WITH CGRV
AND movetoindexrsh(a) = VALOF
    { LET k,n,r = h1!a,h2!a,?
       LET km = k=k_loc -> k_mloc,
                k=k_lab -> k_mlab,
                k=k_glob -> k_mglob, k_none
       UNLESS km=k_none DO
       { r := lookinregs(km,n)
          IF r>=0 DO UNLESS (setr(r) & v_ri)=0 RESULTIS r }
      r := movetoindexr(a)
       genshift(f_shl, 1, k_reg, r)
       setinfo(r,km,n)
       RESULTIS r
    }
 
 
 
// MOVE A SS ITEM INTO ANY REGISTER
// PREFERABLY IN SET S
AND movetoanyr(a,s) = VALOF
    { LET k,n,r = h1!a,h2!a,?
       IF k=k_reg RESULTIS n
       r := lookinfreeregs(k,n)
       IF r>=0 DO
       { h1!a,h2!a := k_reg,r
          RESULTIS r }
       RESULTIS movetor(a,nextrset(-1,s))
    }
 
 
 
// CHANGE R TO BIT STRING SET FORMAT
AND setr(r) =  1<<r
 
// MOVE ARGUMENT TO AN INDEX REGISTER (BX OR SI)
AND movetoindexr(a) = VALOF
 {  LET k,n = h1!a, h2!a
     LET t1, t2 = -1, -1
 
     IF k=k_reg & (n=r_si | n=r_bx) RESULTIS n
 
     IF reg_k!r_si=k & reg_n!r_si=n & isfree(r_si) DO t1 := r_si
 
     IF reg_k!r_bx=k & reg_n!r_bx=n & isfree(r_bx) DO t2 := r_bx
 
     UNLESS t1=-1 & t2=-1 DO { LET t= t2=-1 -> t1, t2
                                 h1!a, h2!a := k_reg, t
                                 RESULTIS t
                              }
 
     RESULTIS movetor(a, (isfree(r_bx)->r_bx,
               isfree(r_si)->r_si, r_bx))
 }
 
 
 
// MOVE A SS ITEM INTO A GIVEN  REGISTER
AND movetor(a,r) = VALOF
    { freereg(r,a)
       genmov(h1!a,h2!a,k_reg,r)
       h1!a,h2!a := k_reg,r
       RESULTIS r
    }
 
 
 
// LOOK FOR THE VALUE OF AN ITEM (K,N) IN THE
// REGISTERS; THE REGISTER WILL NOT BE MODIFIED
AND lookinregs(k,n) = VALOF
    { FOR r=r0 TO r4 DO
          IF reg_k!r=k & reg_n!r=n RESULTIS r
       RESULTIS -1
    }
 
 
 
// LOOK FOR THE VALUE OF AN ITEM (K,N) IN THE
// FREE REGISTERS; THE REGISTER MAY BE MODIFIED
AND lookinfreeregs(k,n) = VALOF
    { FOR r=r0 TO r4 DO
          IF reg_k!r=k & reg_n!r=n & isfree(r) RESULTIS r
       RESULTIS -1
    }
 
 
 
// ALLOCATE THE NEXT REGISTER (EXCEPT X);
// FREE IT IF REQUIRED
AND nextr(x) = VALOF
    { FOR r=r0 TO r4 DO
          UNLESS r=x DO
             IF reg_k!r=k_none & isfree(r) RESULTIS r
       FOR r=r0 TO r4 DO
          UNLESS r=x DO
             IF isfree(r) RESULTIS r
       FOR t=tempv TO arg1 BY 3 DO
       { LET r=regusedby(t)
          UNLESS r=x IF r>=0 DO
          { freereg(r,0)
             RESULTIS r }
       }
    }
 
 
 
//TRY AND ALLOCATE THE NEXT REGISTER FROM THE GIVEN SET
AND nextrset(x, e) = VALOF
  { FOR r = r0 TO r4 DO
      UNLESS r=x DO
       IF reg_k!r=k_none & isfree(r) & inset(e, r) RESULTIS r
      FOR r = r0 TO r4 DO
      UNLESS r=x DO
       IF isfree(r) & inset(e, r) RESULTIS r
     RESULTIS nextr(x)
  }
 
 
 
 
 
AND nextrforceset(x, e) = VALOF
   { FOR r = r0 TO r4 DO
      UNLESS r=x DO
       IF reg_k!r=k_none & isfree(r) & inset(e, r) RESULTIS r
      FOR r = r0 TO r4 DO
      UNLESS r=x DO
       IF isfree(r) & inset(e, r) RESULTIS r
 
      IF (setr(x) NEQV e)=0 DO cgerror("SET MEMBER %N EXCLUDED", TRUE, x)
      IF e=0 DO cgerror("EMPTY SET", TRUE)
 
      FOR t=tempv TO arg1 BY 3 DO
      { LET r = regusedby(t)
         UNLESS r=x IF r>=0 & inset(e, r) DO
         { freereg(r, 0)
            RESULTIS r
         }
      }
      FOR r = r0 TO r4 DO
       UNLESS r=x DO
          IF inset(e,r) DO
            { freereg(r, 0)
               RESULTIS r
            }
      cgerror("FALL OUT NEXTRFSET %N", TRUE, e)
   }
 
 
 
AND inset(set, r) = ((set>>r)&1)=1
 
 
 
// FIND WHICH REGISTER, IF ANY, IS USED BY
// AN SS ITEM
AND regusedby(a) = VALOF
    { LET k=h1!a
       IF k=k_reg RESULTIS h2!a
       IF k=k_xbx | k=k_xsi RESULTIS k-k_roff
       RESULTIS -1  }
 
 
 
AND isfree(r) = VALOF
    { FOR t=tempv TO arg1 BY 3 DO
          IF regusedby(t)=r RESULTIS FALSE
       RESULTIS TRUE
    }
 
 
 
// FREE REGISTER R BY STORING THE VALUES OF
// ALL SS ITEMS (EXCEPT X) THAT DEPEND UPON IT
AND freereg(r,x) BE
       FOR t=tempv TO arg1 BY 3 DO
          UNLESS t=x DO IF regusedby(t)=r DO
             storet(t)
 
 
 
// STORE THE VALUE OF AN SS ITEM IN ITS TRUE
// STACK LOCATION
AND storet(a) BE UNLESS h1!a=k_loc & h2!a=h3!a DO // CHECK REDUNDANT
    { getvalue(a)
       UNLESS h1!a=k_reg | h1!a=k_numb DO
         movetoanyr(a, v_xx)
       genmov(h1!a,h2!a,k_loc,h3!a)
       h1!a := k_loc
       h2!a := h3!a  }
 
 
 
// LOAD AN ITEM (K,N) ONTO THE SS
AND loadt(k, n) BE
    { cgpendingop(v_xx)
       arg2 := arg1
       arg1 := arg1 + 3
       IF h3+arg1-tempt>=0 DO
          cgerror("SIM STACK OVF", TRUE)
       h1!arg1,h2!arg1,h3!arg1 := k,n,ssp
       ssp := ssp + 1
       IF maxssp<ssp DO maxssp := ssp
    }
 
 
 
// REPLACE THE TOP TWO SS ITEMS BY (K,N)
AND lose1(k, n) BE
    { ssp := ssp - 1
       TEST arg2=tempv
       THEN { h1!arg2,h2!arg2 := k_loc,ssp-2
               h3!arg2 := ssp-2 }
       ELSE { arg1 := arg2
               arg2 := arg2-3 }
       h1!arg1, h2!arg1, h3!arg1 := k,n,ssp-1
    }
 
 
 
AND cgbyteap(op) BE
{  cgpendingop(v_xx)
 { LET s = movetoindexrsh(arg2)
    LET i = h2!arg1
    LET byt_m, byt_v = ?, ?
    LET rst = ?
    UNLESS h1!arg1=k_numb DO
    { freereg(s,arg2)
       getvalue(arg1)
       cgplus(arg1,k_reg,s)
       i := 0
    }
    formaddr(s+k_roff, i)
    byt_m, byt_v := addr_m, addr_v
    rst := s=r_bx->"BX","SI"
    TEST op=s_getbyte
    THEN { LET r = nextrforceset(s,v_r80)
            LET tn = tranr(r)
            genrs(f_xor, r, k_reg, r)
            checkbrefs(5)
 //*<3032
            listl("MOV %S,%N(%S)", stringrb(tn), i, rst)
 /*3032>*/
            codeb((f_movrtrm & #376) | 2)
            codeb(consb2(tn, byt_m, byt_v))
            codedisp(byt_m, byt_v)
            lose1(k_reg,r)
         }
   OR { LET r, tn = ?, ?
 
         TEST arg2=tempv DO
         { r := nextrforceset(s, v_r80)
            tn := tranr(r)
            genmov(k_loc, ssp-3, k_reg, r)
         }
         OR
         { LET arg3 = arg2-3
            TEST h1!arg3=k_reg & inset(v_r80, h2!arg3) DO
              r := h2!arg3
            OR
              r := nextrforceset(s, v_r80)
            tn := tranr(r)
            movetor(arg3, r)
         }
 
          checkbrefs(5)
 //*<3032
          listl("MOV %N(%S),%S", i, rst, stringrb(tn))
 /*3032>*/
          codeb(f_movrtrm & #376)
          codeb(consb2(tn, byt_m, byt_v))
          codedisp(byt_m, byt_v)
          forgetvars()
          stack(ssp-3)
       }
   }
 }
 
 
 
AND cgstind() BE
    { cgrv()
       UNLESS h1!arg2=k_reg | h1!arg2=k_numb DO
       { LET r = nextr((h2!arg1)-k_roff)
          movetor(arg2, r)
       }
       genmov(h1!arg2,h2!arg2,h1!arg1,h2!arg1)
       forgetvars()
       stack(ssp-2)
    }
 
 
 
// STORE THE TOP ITEM OF THE SS IN (K,N)
AND storein(k, n) BE
 
{  LET b = (h1!arg1=k & h2!arg1=n) -> 1,
            (h1!arg2=k & h2!arg2=n) -> 2, 0
    LET arg = b=2 -> arg1,arg2
    LET num = b=2 & h1!arg=k_numb
    LET kk = h2!arg
    LET sw = FALSE
    LET pendop = pendingop
 
    IF b=0 GOTO gencase
 
    pendingop := s_none
    SWITCHON pendop INTO
 
    {  DEFAULT:
        gencase: pendingop := pendop
                 cgpendingop(v_xx)
 
        CASE s_none:
                 getvalue(arg1)
                 UNLESS h1!arg1=k_reg | (h1!arg1=k_numb & h2!arg1~=0) DO
                   movetoanyr(arg1, v_xx)
                 genmov(h1!arg1,h2!arg1,k,n)
                 stack(ssp-1)
                 RETURN
 
        CASE s_neg:
                 sw := TRUE
        CASE s_not:
                 UNLESS b=1 GOTO gencase
                 gend(sw -> f_neg, f_not, k, n)
                 stack(ssp-1)
                 RETURN
 
        CASE s_plus:
                 getvalue(arg)
                 cgplus(arg, k, n)
                 ENDCASE
 
        CASE s_minus:
                 getvalue(arg)
                 cgminus(arg, k, n)
                 IF b=1 DO gend(f_neg,k, n)
                 ENDCASE
 
        CASE s_logor:
                 sw := TRUE
        CASE s_logand:
                 getvalue(arg)
                 cglogorandneqv(sw->f_or,f_and, arg, k, n)
                 ENDCASE
 
        CASE s_eqv:
                 sw := TRUE
        CASE s_neqv:
                 getvalue(arg)
                 cglogorandneqv(f_xor,arg, k, n)
                 IF sw DO gend(f_not, k, n)
                 ENDCASE
 
        CASE s_mult:
                 IF h1!arg=k_numb DO
                    IF kk=2 | kk=4 DO
                    { FOR i = 1 TO kk/2 DO
                        genshift(f_shl,1,k,n)
                       ENDCASE
                    }
                 GOTO gencase
 
        CASE s_lshift:
                 sw := TRUE
        CASE s_rshift:
                 UNLESS b=2 GOTO gencase
                 IF num DO
                    IF 0<=kk<=3 DO
                    { FOR i = 1 TO kk DO
                        genshift(sw->f_shl,f_shr,1,k,n)
                       ENDCASE
                    }
 
                 movetor(arg1, r_cx)
                 genshift(sw->f_shl,f_shr,0,k,n)
                 ENDCASE
    }
    stack(ssp-2)
}
 

 
//SECTION "CG5"
 
//GET "CGHDR"
 
LET cgrv() BE
 
{  LET r = 0
 
    IF pendingop=s_minus & h1!arg1=k_numb DO
             pendingop, h2!arg1 := s_plus, -h2!arg1
 
    TEST pendingop=s_plus &
          (h1!arg1=k_numb | h1!arg2=k_numb)
 
    THEN { LET arg = arg2
            LET n = h2!arg1
            IF h1!arg2=k_numb DO arg,n := arg1,h2!arg2
            pendingop := s_none
            r := movetoindexrsh(arg)
            lose1(r+k_roff,2*n) }
 
    ELSE { cgpendingop(v_ri)
            r := movetoindexrsh(arg1)
            h1!arg1, h2!arg1 := r+k_roff, 0 }
}
 
 
 
AND cgshiftk(sw,kk,k,n) BE
{  IF kk=8 DO  // TYPE SHOULD BE K_REG IN THE 8080 SET (V_R80)
    { TEST sw DO { genbytexchg(n); genclearbyte(n) }
               OR { genclearbyte(n); genbytexchg(n) }
       RETURN
    }
    genshift(sw -> f_shl, f_shr, 1, k, n)
    IF kk=2 DO genshift(sw -> f_shl, f_shr, 1, k, n)
}
 
 
 
AND cgaddk(kk, k, n) BE UNLESS kk=0 DO
{  IF kk=1  DO { genincdec(f_inc, k, n); RETURN  }
    IF kk=-1 DO { genincdec(f_dec, k, n); RETURN  }
    genkd(f_add, kk, k, n)
}
 
 
 
AND cgplus(a,k,n) BE TEST h1!a=k_numb
    THEN cgaddk(h2!a, k, n)
    OR cgdiadwithr(f_add, a, k, n)
 
 
 
AND cgminus(a,k,n) BE TEST h1!a=k_numb
    THEN cgaddk(-h2!a, k, n)
    OR cgdiadwithr(f_sub, a, k, n)
 
 
 
AND cglogorandneqv(f, a, k, n) BE
   TEST h1!a=k_numb THEN genkd(f, h2!a, k, n)
   OR cgdiadwithr(f, a, k, n)
 
 
 
AND cgglobal(n) BE
{
    even()
 //*<3032
    listat(s_itemn, 0)
 /*3032>*/
    code(0, 0)
    FOR i = 1 TO n DO
    { LET g = rdgn()
       LET l = rdl()
       LET dl = nextparam()
 //*<3032
       listat(s_itemn, g)
 /*3032>*/
       code(g, 0)
 //*<3032
       listat(s_iteml,dl)
 /*3032>*/
       code(0, -dl)          // GLOBIN DOES RELOCATION
       cgdata(s_datalab, dl) // LABEL OF DESCRIPTOR
       cgdata(s_global, l)   //  THE DESCRIPTOR ITSELF
    }
 //*<3032
    listat(s_itemn, maxgn)
 /*3032>*/
    code(maxgn, 0)
    cgstatics()    // ITEMS IN DATA SEGMENT
}
 
 
 
AND cgentry(n,l) BE
{  genbrefjumps(50,0)
    cgname(s_entry,n)
    setlab(l)
    incode := TRUE
    genrs(f_add, r_bp, k_reg, r_dx)  // ADJUST FRAME
 //*<3032
    listl("MOV 4(BP),DX")
 /*3032>*/
    codeb(#X89); codeb(#X56); codeb(4) // MOV 4(BP),DX
            // POP RETURN ADDRESS INTO STACK FRAME
 //*<3032
    listl("POP (BP)")
 /*3032>*/
    codeb(#X8F); codeb(#X46); codeb(0)  // POP (BP)
 //*<3032
    listl("POP 2(BP)")
 /*3032>*/
    codeb(#X8F); codeb(#X46); codeb(2)  // POP 2(BP)
    IF naming DO
    { //*<3032
       listl("MOV 6(BP),SI")
       /*3032>*/
       codeb(#X89); codeb(#X76); codeb(6)
    }
    IF callcounting DO insertcount()
    forgetall()
}
 
 
 
 
 
AND cgdiadwithr(f, a, k, n) BE
    { LET r = k=k_reg -> n,
               h1!a=k_reg -> h2!a, -1
       IF r=-1 DO r := movetoanyr(a, v_xx)
       TEST k=k_reg DO genrs(f, r, h1!a, h2!a)
                    OR genrd(f, r, k, n)
    }
 
 
 
AND indcode(r) = VALOF
   { LET ir = tranr(r)!TABLE -1,-1,-1,7,-1,6,4,5
      IF ir=-1 DO cgerror("REG %N NOT INDXBLE",FALSE,r)
      RESULTIS ir
   }
 
 
 
 
 
AND tranr(r) = VALOF
   { UNLESS 0<=r<=7 DO cgerror("INVALID REGISTER %N", TRUE, r)
      RESULTIS r!TABLE 3,0,1,2,6,5,4,7
   }
 
AND cgsave(n) BE
  {  LET nrgs = n-savespacesize
      FOR i = 0 TO nrgs>=3->2,nrgs-1 DO
      { genmov(k_reg, i, k_loc, i+savespacesize)
         setinfo(i, k_loc, i+savespacesize)
      }
      initstack(n)
  }
 
 
 
// FUNCTION OR ROUTINE CALL
AND cgapply(op,k) BE
 
{  LET sr0 = k+savespacesize
    LET sr2 = k+savespacesize+2
    LET l = ?
 
    cgpendingop(v_xx)
 
    // STORE ARGS 4,5,...
    store(sr2+1, ssp-2)
 
    // NOW DEAL WITH NON-ARGS
    FOR t = tempv TO arg2 BY 3 DO
        { IF h3!t>=k BREAK
           IF regusedby(t)>=0 DO storet(t)  }
 
    getvalue(arg1)
 
    // MOVE ARGS 1-3 TO ARG REGISTERS
    FOR t = arg2 TO tempv BY -3 DO
        { LET s = h3!t
           LET r = s-sr0
           IF s<sr0 BREAK
           IF s<=sr2 & isfree(r) DO movetor(t,r)  }
    FOR t = arg2 TO tempv BY -3 DO
        { LET s = h3!t
           LET r = s-sr0
           IF s<sr0 BREAK
           IF s<=sr2 DO movetor(t,r)  }
 
    // DEAL WITH ARGS NOT IN SS
    FOR s = sr0 TO sr2 DO
    { LET r = s-sr0
       IF s>=h3!tempv BREAK
       freereg(r,0)
       genmov(k_loc, s, k_reg, r)  }
 
    freereg(r_dx, 0)
 
    genmov(k_numb, 2*k, k_reg, r_dx)
    IF h1!arg1=k_numb DO h2!arg1 := h2!arg1*2
 
    freereg(r_si, 0)         // SI USED TO HOLD DESCRIPTOR ADDRESS
    genmov(h1!arg1, h2!arg1, k_reg, r_si)
    genj(f_cis, k_xsi, 0)
 
    forgetall()
    stack(k)
    IF op=s_fnap DO loadt(k_reg,r0)
}
 
 
 
AND cgreturn(op) BE
 
{  cgpendingop(v_r0)
    IF op=s_fnrn DO
    { movetor(arg1,r0)
       stack(ssp-1)  }
                 // GENERATE ONLY 1 RETURN SEQUENCE
                 // PER SECTION
   TEST return_set=0 DO
   { return_set := nextparam()
      incode := TRUE     // BE SAFE
      checkbrefs(7)
      setlab(return_set)
 //*<3032
       listl("MOV SI,4(BP)")
 /*3032>*/
       codeb(#X8B);codeb(#X76);codeb(4) // MOV SI,4(BP)
       genrs(f_sub, r_bp, k_reg, r_si) // OLD FRAME POINTER
 //*<3032
       listl("JIS (BP,SI)")
 /*3032>*/
       codeb(#XFF); codeb(#X2A)        // JIS (BP,SI)
   }
   OR
   { checkbrefs(3)
      genbranch(f_jmp, return_set)
   }
    initstack(ssp)
}
 
 
 
// USED FOR OCODE OPERATORS JT AND JF
AND cgjump(b,l) BE
{  LET f = condbrfn(pendingop)
    IF f=0 DO
    { cgpendingop(v_xx)
       loadt(k_numb,0)
       f := f_jne }
    pendingop := s_none
    store(0,ssp-3)
    getvalue(arg1)
    getvalue(arg2)
    f := cgcmp(b,f,-1)
    genbranch(f,l)
    stack(ssp-2)
    IF profiling DO insertcount()
}
 
 
 
AND cgcmpf(f) = VALOF SWITCHON f INTO
    { CASE f_jl : RESULTIS f_jg
       CASE f_jge: RESULTIS f_jle
       CASE f_jle: RESULTIS f_jge
       CASE f_jg : RESULTIS f_jl
       DEFAULT   : RESULTIS f
    }
 
 
 
    // XR IS A REGISTER WHICH MUST NOT BE USED!
AND cgcmp(b,f,xr) = VALOF
{  TEST numberis(0,arg1) & h1!arg2=k_reg
    THEN genrs(f_or,h2!arg2, k_reg,h2!arg2)
    ELSE { TEST numberis(0,arg2) & h1!arg1=k_reg THEN
              { genrs(f_or,h2!arg1,k_reg,h2!arg1)
                 f := cgcmpf(f)
              }
            OR
            { TEST h1!arg1=k_numb & h1!arg2~=k_numb THEN
                  genkd(f_cmp, h2!arg1, h1!arg2, h2!arg2)
               OR
               { TEST h1!arg2=k_numb & h1!arg1~=k_numb THEN
                  { genkd(f_cmp, h2!arg2, h1!arg1, h2!arg1)
                     f := cgcmpf(f)
                  }
                OR { UNLESS h1!arg1=k_reg | h1!arg2=k_reg DO
                        movetor(arg2, nextr(xr))
                      cgdiadwithr(f_cmp, arg1, h1!arg2, h2!arg2)
                   }
               }
            }
         }
            RESULTIS b -> f, f NEQV 1
}
 


//SECTION "CG6"
 
//GET "CGHDR"
 
// COMPILES CODE FOR SWITCHON
// N = NO. OF CASES
// D = DEFAULT LABEL
LET cgswitch(n) BE
    {  LET d = rdl()
        LET v = getvec(2*n+1)
        IF v=0 DO cgerror("RUN OUT OF STORE",TRUE)
        casek, casel := v, v+n
 
        // READ AND SORT (K,L) PAIRS
        FOR i = 1 TO n DO
          { LET a = rdn()
             LET l = rdl()
             LET j = i-1
             UNTIL j=0 DO
               { IF a > casek!j BREAK
                  casek!(j+1) := casek!j
                  casel!(j+1) := casel!j
                  j := j - 1  }
             casek!(j+1), casel!(j+1) := a, l  }
 
        cgpendingop(v_r0)
        store(0, ssp-2)
        movetor(arg1,r0)
        stack(ssp-1)
 
        UNLESS n=0 DO
           // CARE WITH OVERFLOW !
           TEST 2*n-6 > casek!n/2-casek!1/2
 
                   THEN lswitch(1, n, d)
 
                   OR { bswitch(1, n, d)
 
                         genbranch(f_jmp, d)  }
 
           freevec(v)
    }
 
 
 
// BINARY SWITCH
AND bswitch(p, q, d) BE TEST q-p>6
 
      THEN { LET m = nextparam()
              LET t = (p+q)/2
              genrs(f_cmp, r0, k_numb, casek!t)
              genbranch(f_jge,m)
              bswitch(p, t-1, d)
              genbranch(f_jmp,d)
              genbrefjumps(50,0)
              setlab(m)
              incode := TRUE
              genbranch(f_je,casel!t)
              bswitch(t+1, q, d)  }
 
      ELSE FOR i = p TO q DO
              { genrs(f_cmp,r0,k_numb,casek!i)
                 genbranch(f_je,casel!i) }
 
 
 
 
 
// LABEL VECTOR SWITCH
AND lswitch(p,q,d) BE
    {  LET l = nextparam()
        genrs(f_sub,r0,k_numb,casek!p)
        genbranch(f_jl,d)
        genrs(f_cmp,r0,k_numb,casek!q-casek!p)
        genbranch(f_jg,d)
        genshift(f_shl, 1, k_reg, r0)
        checkbrefs(6)
        codeb(#X2E) // CODE SEGMENT OVERRIDE PREFIX
 //*<3032
        listl("JI label %N(BX)", l)
 /*3032>*/
        code(#123777, 0)           // JI L(BX)
        code(0, l)
        incode := FALSE
        genbrefjumps(((casek!q-casek!p)*2+50),0)
        setlab(l)
        FOR k=casek!p TO casek!q TEST casek!p=k
            THEN {
 //*<3032
                    listat(s_iteml, casel!p)
 /*3032>*/
                    code(0, casel!p)
                    p := p+1 }
            ELSE code(0, d)
    }
 
 
 
// CHECKS THAT AT LEAST N CONSECUTIVE BYTES
// MAY BE COMPILED WITHOUT ANY BRANCH REFS
// GOING OUT OF RANGE
AND checkbrefs(n) BE
  {
     UNLESS brefv=brefp |
      brefv!1+129-n-(brefp-brefv)*2>=stvp DO
        TEST incode
        THEN { LET l = nextparam()
                brlabref(l, stvp+1)
                codeb(f_b)
                codeb(0)
                genbrefjumps(n+50,0)
                setlab(l)  }
        ELSE genbrefjumps(n+50,0)
  }
 
 
 
// GENERATES JUMPS TO FILL IN ENOUGH BRANCH
// REFS TO ENSURE THAT AT LEAST N BYTES MAY
// BE COMPILED, GIVEN THAT LABEL X IS TO BE
// DEFINED AS THE NEXT LOCATION
AND genbrefjumps(n,x) BE
  { LET p = brefv
     UNTIL p=brefp |
      p!1+129-n-(brefp-brefv)*2>=stvp DO
        { IF p!0=x DO     // LEAVE REFS TO X
           { p := p+2
              LOOP }
           IF brefv!0=x DO // CHECK X STILL IN RANGE
           { UNLESS brefv!1+126>stvp DO
              { genbrefjumps(n,0)
                 RETURN }
           }
        { LET l=p!0
           setlab(l)       // TO FILL IN BRANCH REFS
           labv!l := -1    // THEN UNSET L AGAIN
 //*<3032
          IF cglisting DO writes(" !!!")
           listl("JMP L%N", l)
 /*3032>*/
           codeb(#351)                 // JMP
           code(-stvp-2, -l)   // GIVES CORRECT DISP. WHEN SECTION OUTPUT
        }
        }
  }
 
 
 
// GENERATE A LABEL REF FOR A BRANCH INSTR
AND brlabref(l, a) BE
     { brefp!0, brefp!1 := l, a
        brefp := brefp + 2  }
 
 
 
AND condbrfn(op) = VALOF SWITCHON op INTO
     { CASE s_eq:  RESULTIS f_je
        CASE s_ne:  RESULTIS f_jne
        CASE s_gr:  RESULTIS f_jg
        CASE s_le:  RESULTIS f_jle
        CASE s_ge:  RESULTIS f_jge
        CASE s_ls:  RESULTIS f_jl
        DEFAULT:    RESULTIS 0
     }
 
 
 
AND genbranch(f, l) BE IF incode DO
    {  LET a = labv!l
 
        checkbrefs(4)
 
        IF a=-1 DO         // LABEL IS UNSET
           { brlabref(l, stvp+1)
 //*<3032
              listl("%S L%N", sf(f), l)
 /*3032>*/
              codeb(f)
              codeb(0)
              IF f=f_jmp DO incode := FALSE
              RETURN  }
 
        IF stvp-a > 126 DO // BACK JUMP TOO FAR FOR BR
           { LET m = ?
              IF f=f_jmp DO
              {
 //*<3032
                 listl("JMP L%N", l)
 /*3032>*/
                 codeb(#351)               // JMP
                 code(-stvp-2, -l)
                 incode := FALSE
                 RETURN }
              f := f NEQV 1
              m := nextparam()
              genbranch(f, m)
 //*<3032
              listl("JMP L%N", l)
 /*3032>*/
              codeb(#351)                   // JMP
              code(-stvp-2, -l)
              genbrefjumps(50,m)
              setlab(m)
              RETURN  }
 
        // IT MUST BE A SHORT BACKWARD JUMP
 //*<3032
        listl("%S L%N", sf(f), l)
 /*3032>*/
        codeb(f)
        codeb(a-stvp-1)
        IF f=f_jmp DO incode := FALSE
    }
 
 
 
// GENERATE A MOV INSTR; WILL CALCULATE LVALUES
AND genmov(k1,n1,k2,n2) BE UNLESS k1=k2 & n1=n2 DO
   {  LET m1,v1 = 0,0
       LET r=lookinregs(k1,n1)
       IF r>=0 DO k1,n1 := k_reg,r
 
       UNLESS k1=k2 & n1=n2 DO
       { IF (k1=k_loc | k1=k_lab | k1=k_glob) &
             (k2=k_loc | k2=k_lab | k2=k_glob) DO
          { LET r = nextr(-1)
             genmov(k1, n1, k_reg, r)
             k1, n1 := k_reg, r
          }
 
           SWITCHON k1 INTO
           { CASE k_lvloc:
                m1, v1 := m_loc, 2*n1
                GOTO l
 
              CASE k_lvglob:
                m1, v1 := m_glob, 2*n1
                GOTO l
 
              CASE k_lvlab:
                m1, v1 := m_lab, n1
        l:      formdaddr(k2, n2)
                TEST addr_m=m_reg DO
                {
 //*<3032
                   listgen("LEA", m_reg, addr_v, m1, v1)
 /*3032>*/
                   coders(f_lea, n2, m1, v1)
                }
                OR
                { TEST m1=m_lab DO
                   {
 //*<3032
                      listgen(sf(f_movimmtrm), addr_m, addr_v, m_lab, v1)
 /*3032>*/
                      codekd(f_movimmtrm, 0, addr_m, addr_v)
                      labref(v1, stvp-2)  // FOR LABEL FILLING
                   }
                   OR
                   { LET r = m1=m_loc -> r_bp, r_di
 //*<3032
                      listgen(sf(f_movrtrm),addr_m,addr_v,m_reg,tranr(r))
 /*3032>*/
                      coderd(f_movrtrm, r, addr_m, addr_v)
 //*<3032
                      listgen("ADD",addr_m,addr_v,m_imm,v1)
 /*3032>*/
                      codekd(f_add,v1,addr_m,addr_v)
                   }
                }
 //*<3032
              listgen("SHR", addr_m, addr_v, 0)
 /*3032>*/
              coded(f_shr, addr_m, addr_v)
              ENDCASE
 
              CASE k_numb:
                formdaddr(k2, n2)
                IF n1=0 & addr_m=m_reg DO
                {
 //*<3032
                   listgen("XOR", m_reg, addr_v, m_reg, addr_v)
 /*3032>*/
                   coderd(f_xor, n2, addr_m, addr_v)
                   ENDCASE
                }
 
                IF addr_m=m_reg DO
                { checkbrefs(4)
 //*<3032
                   listgen(sf(f_movimmtr), m_reg, addr_v, m_imm, n1)
 /*3032>*/
                   codeb(f_movimmtr | tranr(n2))
                   code(n1, 0)
                   ENDCASE
                }
 
 //*<3032
                listgen(sf(f_movimmtrm),addr_m,addr_v,m_imm,n1)
 /*3032>*/
                codekd(f_movimmtrm, n1, addr_m, addr_v)
              ENDCASE
 
              DEFAULT:
                TEST k1=k_reg DO
                { formdaddr(k2, n2)
 //*<3032
                   listgen(sf(f_movrtrm),addr_m,addr_v,m_reg,tranr(n1))
 /*3032>*/
                   coderd(f_movrtrm, n1, addr_m, addr_v)
                }
                OR
                { formsaddr(k1, n1)
                   UNLESS k2=k_reg DO cgerror("K2 NOT A REG_ *
                        * IN GENMOV %N", TRUE, k2)
 //*<3032
                   listgen(sf(f_movrtrm),m_reg,tranr(n2),addr_m,addr_v)
 /*3032>*/
                   coders(f_movrtrm, n2, addr_m, addr_v)
                }
           }
        }
        remem(k1, n1, k2, n2)
    }
 
 
 
AND genshift(f, b, k, n) BE
  { //F IS ANOTHER ONE OF THOSE 11 BIT OBJECTS
     LET v = b=0 -> #20, 0
     formdaddr(k, n)
 //*<3032
     TEST v=0 THEN listgen(sf(f), addr_m, addr_v, 0)
              OR   listgen(sf(f), addr_m, addr_v, m_reg, tranr(r_cx))
 /*3032>*/
     coded(f|v, addr_m, addr_v)
  }
 
 
 
AND genrs(f,r,k,n) BE
    { formsaddr(k,n)
       IF f=f_cmp GOTO grl
       IF f=f_or & addr_m=m_reg DO
         IF addr_v=tranr(r) GOTO grl
       forget(k_reg, r)
  grl:
 //*<3032
       listgen(sf(f),m_reg,tranr(r),addr_m,addr_v)
 /*3032>*/
       coders(f,r,addr_m,addr_v)
    }
 
 
 
AND genrd(f,r,k,n) BE
    { TEST f=f_cmp DO formsaddr(k, n) OR formdaddr(k, n)
 //*<3032
       listgen(sf(f),addr_m,addr_v,m_reg,tranr(r))
 /*3032>*/
       coderd(f,r,addr_m,addr_v)
    }
 
 
 
AND gend(f,k,n) BE
    { formdaddr(k, n)
 //*<3032
       listgen(sf(f), addr_m, addr_v, 0)
 /*3032>*/
       coded(f,addr_m,addr_v)
    }
 
 
 
AND genf(f) BE IF incode DO
    { checkbrefs(2)
 //*<3032
       listl(sf(f))
 /*3032>*/
       codeb(f)
    }
 
 
 
// INTERRUPT TYPE N
AND genint(n) BE IF incode DO
{ checkbrefs(3)
 //*<3032
   listl("INT %N", n)
 /*3032>*/
   codeb(f_int)
   codeb(n)
}
 
 
 
AND genkd(f, i, k, n) BE
    { LET r = lookinregs(k_numb,i)
       TEST f=f_cmp THEN formsaddr(k,n) OR formdaddr(k,n)
       TEST r>=0 THEN
       {
 //*<3032
          listgen(sf(f),addr_m,addr_v,m_reg,tranr(r))
 /*3032>*/
          coderd(f, r, addr_m, addr_v)
       }
       OR
       {
         // Optimise operations of the form AND reg,!#FFxx
         //                                 AND reg,!#xxFF
         //                                 OR  reg,!#00yy
         //                                 OR  reg,!#yy00
         // by using the 8080 half registers is possible.
 
          IF addr_m=m_reg & inset(v_r80, invtran(addr_v)) DO
          {
             LET ms = (f=f_and & (i&#XFF00)=#XFF00) |
                      (f=f_or  & (i&#XFF00)=0)
             LET lst = (f=f_and & (i&#XFF)=#XFF) |
                      (f=f_or  & (i&#XFF)=0)
             // MS is TRUE if the ms byte is unchanged, LST if the
             // ls byte is unchanged.
 
             // Check for special case of ANDing with #XFF00 or #XFF
             // when XOR can be used.
             IF (ms & (i=#XFF00)) | (lst & (i=#XFF)) DO
             { LET vr = ms -> addr_v, addr_v | 4
 //*<3032
                LET s = stringrb(vr)
 /*3032>*/
                checkbrefs(2)
 //*<3032
                listl("XOR   %S,%S", s, s)
 /*3032>*/
                codeb(f_clrb)
                codeb(consb2(vr, m_reg, vr))
                RETURN
             }
 
             // Different code for AX or AL.
             IF addr_v=tranr(r_ax) DO
             { LET wbit = ms -> 0, 1   // can use AL if ms does not change
 //*<3032
                LET sr = ms -> "AL", "AX"
 /*3032>*/
                checkbrefs(ms->2, 3)
 //*<3032
                listl("%S  %S,", sf(f), sr)
                IF cglisting DO
                   TEST ms DO writef("!#X%X2", i) OR writef("!#X%X4", i)
 /*3032>*/
                codeb( (f<<3) | 4 | wbit )
 
                TEST ms DO codeb(i) OR code(i, 0)
                RETURN
             }
 
             IF ms | lst DO
             { LET vr = lst -> addr_v | 4, addr_v
 //*<3032
                LET s = stringrb(vr)
 /*3032>*/
                checkbrefs(3)
 //*<3032
                listl("%S  %S,", sf(f), s)
                IF cglisting DO writef("!#X%X2", ms -> i, i>>8)
 /*3032>*/
                codeb(#200)
                codeb(consb2(f, m_reg, vr))
                IF lst DO i := i>>8
                codeb(i)
                RETURN
             }
          }
 
 //*<3032
          listgen(sf(f),addr_m,addr_v,m_imm,i)
 /*3032>*/
          codekd(f, i, addr_m, addr_v)
       }
    }
 
 
 
 // Find register from code value (inverse of TRAN)
AND invtran(v) = VALOF
{ UNLESS 0<=v<=7 DO cgerror("Invalid value for INVTRAN %N", TRUE, v)
   RESULTIS v!TABLE 1,2,3,0,6,5,4,7
}
 
 
 
// GENERATE A JI, JIS OR CIS INSTRUCTION;
// ONE EXTRA LEVEL OF INDIRECTION
AND genj(f,k,n) BE
    { formsaddr(k,n)
       checkbrefs(4)
 //*<3032
       listgen(f=f_ji->"JI",f=f_jis->"JIS","CIS", addr_m, addr_v, 0)
 /*3032>*/
       codeb(#377)            // INDIRECT CONTROL TRANSFER
       codeb(consb2(f, addr_m, addr_v))
       codedisp(addr_m, addr_v)
       IF f=f_ji | f=f_jis DO incode := FALSE
    }
 
 
 
 
 
 // SWAP BYTES OF '8080' DOUBLE REGISTER
AND genbytexchg(n) BE
    { LET tn = tranr(n)
//*<3032
       LET s1, s2 = stringrb(tn), stringrb(tn+4)
/*3032>*/
       UNLESS 0 <= tn <= 3 DO
           cgerror("NOT V80 IN GBXG", TRUE)
       forget(k_reg, n)
       checkbrefs(2)
 //*<3032
       listl("XCHG %S,%S", s1, s2)
 /*3032>*/
       codeb(f_xchgb)
       codeb(consb2(tn, m_reg, tn+4))
    }


 
// CLEAR L.S. BYTE OF '8080' DOUBLE REGISTER
AND genclearbyte(n) BE
    { LET tn = tranr(n)
//*<3032
       LET s = stringrb(tn)
/*3032>*/
       UNLESS 0 <= tn <=3 DO
         cgerror("NOT V80 IN GCLB", TRUE)
       forget(k_reg, n)
       checkbrefs(2)
 //*<3032
       listl("XOR %S,%S", s, s)
 /*3032>*/
       codeb(f_clrb)
       codeb(consb2(tn, m_reg, tn))
    }

AND genincdec(f, k, n) BE
    { formdaddr(k, n)
       TEST addr_m=m_reg DO
       { checkbrefs(1)
 //*<3032
          listgen(f=f_inc->"INC","DEC", addr_m, addr_v, 0)
 /*3032>*/
          codeb(#100 | (f<<3) | addr_v)
       }
       OR
       { checkbrefs(4)
 //*<3032
          listgen(f=f_inc->"INC","DEC", addr_m, addr_v, 0)
 /*3032>*/
          codeb(#377)
          codeb(consb2(f, addr_m, addr_v))
          codedisp(addr_m, addr_v)
       }
    }
 
AND genmuldiv(f, a) BE
{ LET k, n = h1!a, h2!a
   formsaddr(k, n)
   forget(k_reg, r_ax)
   forget(k_reg, r_dx)
   checkbrefs(4)
 //*<3032
    listgen(f=f_imul->"IMUL","IDIV", addr_m, addr_v, 0)
 /*3032>*/
   codeb(#367)
   codeb(consb2(f, addr_m, addr_v))
   codedisp(addr_m, addr_v)
}
 


//SECTION "CG7"
 
//GET "CGHDR"
 
// FORMS A SOURCE ADDRESS (M,V) PAIR;
// LOOKS IN THE REGISTERS
LET formsaddr(k,n) BE
    { LET r=lookinregs(k,n)
       IF r>=0 DO
       { addr_m,addr_v := m_reg,tranr(r)
          RETURN }
       formaddr(k,n)
    }
 
// FORMS A DESTINATION ADDRESS (M,V) PAIR;
// FORGETS THE VALUE OF THE DESTINATION
AND formdaddr(k,n) BE
    { forget(k,n)
       formaddr(k,n)
    }
 
// FORMS A MACHINE ADDRESS PAIR (M,V)
// FOR USE BY A CODE- ROUTINE
AND formaddr(k,n) BE
    SWITCHON k INTO
       { CASE k_loc:
             addr_m,addr_v := m_loc,2*n
             ENDCASE
 
          CASE k_numb:
             addr_m,addr_v := m_imm,n
             ENDCASE
 
          CASE k_glob:
             addr_m,addr_v := m_glob,2*n
             ENDCASE
 
          CASE k_lab:
             addr_m,addr_v := m_lab,n
             ENDCASE
 
          CASE k_reg:
             addr_m,addr_v := m_reg,tranr(n)
             ENDCASE
 
          CASE k_xbx:
             addr_m,addr_v := m_bx,n
             ENDCASE
 
          CASE k_xsi:
             addr_m,addr_v := m_si,n
             ENDCASE
 
          DEFAULT: cgerror("BAD ADDRESS TYPE %N", TRUE, k)
 
      }
 
// CALLED BY GENMOV TO UPDATE THE CONTENTS
// OF THE REGISTERS
AND remem(k1,n1,k2,n2) BE
       TEST k2=k_reg
         THEN setinfo(n2,k1,n1)
         ELSE IF k1=k_reg & reg_k!n1=k_none DO
                 setinfo(n1,k2,n2)
  
// SETS THE INFO FOR REGISTER R TO (K,N)
AND setinfo(r,k,n) BE
    { SWITCHON k INTO
       { CASE k_reg:
             k := reg_k!n
             n := reg_n!n
             ENDCASE
 
          DEFAULT:
             k := k_none
          CASE k_loc: CASE k_glob:
          CASE k_lab:
          CASE k_mloc: CASE k_mglob:
          CASE k_mlab:
          CASE k_lvloc: CASE k_lvglob:
          CASE k_lvlab:
          CASE k_numb:
       }
       reg_k!r := k
       reg_n!r := n
    }
 
// FORGETS THE VALUE OF A REGISTER OR VARIABLE
AND forget(k,n) BE
    { SWITCHON k INTO
       { CASE k_reg:
             reg_k!n := k_none
          DEFAULT:
             RETURN
 
          CASE k_loc:
             forget(k_mloc,n)
             ENDCASE
 
          CASE k_glob:
             forget(k_mglob,n)
             ENDCASE
 
          CASE k_lab:
             forget(k_mlab,n)
 
          CASE k_mloc: CASE k_mglob: CASE k_mlab:
       }
       FOR r=r0 TO r4 DO IF reg_k!r=k & reg_n!r=n DO
             reg_k!r := k_none
    }
 
// FORGETS THE VALUES OF ALL VARIABLES; CALLED
// AFTER AN INDIRECT ASSIGNMENT
AND forgetvars() BE
    FOR r=r0 TO r4 SWITCHON reg_k!r INTO
       { CASE k_loc: CASE k_glob:
          CASE k_lab:
          CASE k_mloc: CASE k_mglob:
          CASE k_mlab:
             reg_k!r := k_none
          DEFAULT:
       }
 
// FORGETS THE CONTENTS OF ALL REGISTERS; CALLED
// AFTER LABELS, PROCEDURE CALLS
AND forgetall() BE
    FOR r=r0 TO r4 DO reg_k!r := k_none
 
 //MAKE 1 BYTE OF CODE
AND codeb(b) BE
    { putvbyte(stv, stvp, b)
      stvp := stvp + 1
      checkspace()
    }
 
 
// MAKES ONE WORD OF CODE; L INDICATES A LABEL REF
AND code(a, l) BE
{ UNLESS l=0 DO labref(l, stvp)
   pwabo(stv, stvp, a)
   stvp := stvp + 2
   checkspace()
}
 
AND coded(f, m, v) BE IF incode DO
{ checkbrefs(4)
   codeb(f>>3)    // F IS AN 11 BIT COMPOSITE
   codeb(consb2(f&7,m,v))
   codedisp(m,v)
}
 
 
AND codekd(f, i, m, v) BE IF incode DO
    { IF f=f_movimmtrm DO
       { checkbrefs(6)
          codeb(#307)
          codeb(consb2(0, m, v))
          codedisp(m, v)
          code(i, 0)
          RETURN
       }
 
       IF f=f_movimmtr DO
       { checkbrefs(3)
          codeb(#270|v)
          code(i, 0)
          RETURN
       }
 
       IF m=m_reg DO
         IF v=tranr(r_ax) DO
         { checkbrefs(3)
            codeb((f<<3)|5)
            code(i, 0)
            RETURN
         }
 
         { LET s = smallnum(i) -> 2, 0
            UNLESS f=f_add | f=f_sub | f=f_cmp DO s := 0
            TEST s=2 THEN checkbrefs(5) OR checkbrefs(6)
            codeb(#201|s)
            codeb(consb2(f, m, v))
            codedisp(m, v)
            TEST s=0 DO code(i, 0)
            OR codeb(i)
         }
    }
 
AND coders(f, r, m, v) BE IF incode DO
{  checkbrefs(4)
    TEST m=m_imm THEN
    { TEST r=r_ax THEN
       { codeb((f<<3)|5)
          code(v, 0)
       }
       OR
       { LET s = smallnum(v) -> 2,0
          UNLESS f=f_add | f=f_sub | f=f_cmp DO s := 0
          codeb(#201|s)
          codeb(consb2(f,m_reg,tranr(r)))
          TEST s=0 DO code(v, 0) OR codeb(v)
       }
    }
    OR
    { TEST f=f_movrtrm DO codeb(f_movrtrm | 2)
       OR { TEST f=f_lea DO codeb(f_lea) OR codeb((f<<3)|3) }
       codeb(consb2(tranr(r),m,v))
       codedisp(m,v)
    }
}
 
AND coderd(f, r, m, v) BE IF incode DO
{ checkbrefs(4)
   TEST f=f_movrtrm DO codeb(f_movrtrm)
   OR codeb((f<<3)|1)
   codeb(consb2(tranr(r), m, v))
   codedisp(m, v)
}
 
// INSERTS A PROFILE COUNT
AND insertcount() BE
{  LET l = nextparam()
    checkbrefs(9)
 //*<3032
    listl("INC L%N", l)
 /*3032>*/
    codeb(#X2E) // CODE SEGMENT OVERRIDE PREFIX
    codeb(#377)
    codeb(6)                          // INC L
    code(0, l)
 //*<3032
    listl("JMP $+4")
 /*3032>*/
    codeb(#353)                       // BRANCH ROUND COUNT
    codeb(2)
    setlab(l)
 //*<3032
    listat(s_itemn, 0)
 /*3032>*/
    code(0, 0)                        // WORD FOR THE COUNT
}
 
// SET THE LABEL L TO THE CURRENT LOCATION
AND setlab(l) BE
{  LET p = brefv
    UNLESS labv!l=-1 DO
      cgerror("LABEL L%N SET TWICE", FALSE, l)
     UNLESS brefp=brefv DO
       // ELIMINATE REDUNDANT BRANCHES
       IF (brefp-2)!0=l & (brefp-2)!1=stvp-1 DO
          stvp, brefp := stvp-2, brefp-2
    labv!l := stvp
 //*<3032
    listlab(l)
 /*3032>*/
    // FILL IN FORWARD BRANCH REFS
    UNTIL p-brefp>=0 DO TEST !p=l
      THEN { LET loc = p!1
              LET a = stvp - loc - 1
              IF a>127 DO
                 cgerror("BAD BR LAB L%N", FALSE, l)
              putvbyte(stv, loc, a)
              brefp := brefp - 2
              FOR q = p TO brefp-1 DO q!0 := q!2  }
      ELSE p := p+2
}
 
// COMPILE OCODE LABEL L
AND cglab(l) BE
{
    UNLESS incode DO genbrefjumps(50,l)
    setlab(l)
    incode := TRUE
    IF profiling DO insertcount()
    forgetall()
}
 
// COMPILES NAMES FOR S_ENTRY, S_SECTION
AND cgname(op,n) BE
{ LET v = VEC 17/bytesperword+1
 //*<3032
   LET astar, aspace = askii('**'), askii('*S')
 /*3032>*/
 /*<8086
   LET ASTAR, ASPACE = '**', '*S'
 /*8086>*/
    putbyte(v, 0, op=s_entry->7, 17)
    FOR i = 1 TO 9 DO putbyte(v,i+8,getbyte(datvec,i)) // DATE
    FOR i=1 TO n DO
    { LET c = rdn()
       IF i <=7 DO putbyte(v, i, c) }
    FOR i = n+1 TO 7 DO putbyte(v, i, n=0->astar, aspace)
    putbyte(v, 8, aspace)
    IF naming DO
    { IF op=s_section DO
       {
 //*<3032
          listl("SECTION")
 /*3032>*/
          code(secword, 0)
       }
 //*<3032
       listl(v)
 /*3032>*/
       FOR i = 0 TO op=s_entry->6,16 BY 2 DO
         { codeb(getbyte(v,i)); codeb(getbyte(v,i+1)) }
    }
}
 
AND cgstring(n) BE
    {  LET l,w = nextparam(),n
        loadt(k_lvlab,l)
        cgdata(s_datalab, l)
        { UNLESS n=0 DO w :=(rdn()<<8) | w
           cgdata(s_itemn, w)
           IF n<=1 BREAK
           n, w := n-2, rdn()  } REPEAT
    }

// GENERATE A LABEL REFERENCE
// L>0 => RELOCATION
AND labref(l, p) BE
{  dp := dp-3
    checkspace()
    h1!dp, h2!dp, h3!dp := 0, l, p
    !refliste := dp
    refliste := dp
}
  
AND cgdata(a, l) BE
{  dp := dp-3
    checkspace()
    h1!dp, h2!dp, h3!dp := 0, a, l
    !dliste := dp
    dliste := dp
}
 
AND cgstatics() BE
{  LET d = dlist
    LET p = ?
    even()
    { pwabo(stv, 0, stvp>>1)  //FILL IN NO. OF WORDS OF CODE
       p := stvp
       code(0, 0)              //THIS WILL HOLD NO. OF DATA WORDS
    }
    UNTIL d=0 DO
    {
       SWITCHON h2!d INTO
       { CASE s_datalab: setlab(h3!d);    ENDCASE
          CASE s_iteml:   { LET il = nextparam()
                              code(0, il)        // REFER TO DESCRIPTOR
                              //*<3032
                             listl("        DW    L%N", il)
                             /*3032>*/
                              setlab(il)
                          }
          CASE s_global:  code(0, h3!d)
                          code(0, 0)   // GETS FILLED IN WITH SEG BY LOADSEG
                           //*<3032
                           listl("        DW    L%N", h3!d)
                           listl("        DW    0    SEG")
                           /*3032>*/
                          ENDCASE
          CASE s_itemn:   code(h3!d, 0)
                          //*<3032
                          listat(s_itemn, h3!d)
                          /*3032>*/
                          ENDCASE                  }
       d := !d  }
 
      { dsize := (stvp-p)>>1
         pwabo(stv, p, dsize)    //FILL IN DATA SECTION SIZE
      }
}
 
AND initdatalists() BE
{  reflist := 0
    refliste := @reflist
    dlist := 0
    dliste := @dlist
}
 
AND checkspace() BE IF stv+stvp/2-dp>0 DO
    cgerror("PROGRAM TOO LARGE*
            * %N WORDS COMPILED",TRUE, stvp)
 
 // CONSTRUCT 2ND BYTE OF MANY INSTRUCTIONS
 // FORMAT IS (MOD. MID. DSP) ([2 3 3]) BITS
AND consb2(mid, rm, dsp) = VALOF
 { LET mod = rm=m_reg -> #300,
              rm=m_lab -> 0,
              rm=m_loc & dsp=0 -> #100,
              dsp=0 -> 0,
              smallnum(dsp) -> #100, #200
 
    RESULTIS mod + (mid<<3) +
             (rm=m_lab -> m_loc, rm=m_reg -> dsp, rm)
 }

AND codedisp(m, v) BE
 { IF m=m_lab DO { code(0, v); RETURN }
 UNLESS m=m_reg | (v=0 & m~=m_loc) DO
  TEST smallnum(v) DO codeb(v)
                   OR code(v, 0)
}
 
AND even() BE
    UNLESS iseven() DO
    { checkbrefs(1)
       IF iseven() RETURN
 //*<3032
       listl("NOP")
 /*3032>*/
       codeb(f_nop)
    }
  
AND smallnum(i) = -128 <= i <= 127
 
 // PUT WORD AT BYTE OFFSET IN VECTOR (STRANGE SIMULATION OF
 // 16 BIT WORDS! )
AND pwabo(v, ot, w) BE
    {  LET p = ot/2
        TEST (ot & 1)=0 DO
          v!p := ((w&255)<<8)|((w&#177400)>>8)
        OR
        { v!p := (v!p & #177400) | (w & 255)
           v!(p+1) := (v!(p+1) & #377) | (w & #177400)
        }
    }
 
 // ADD WORD AT BYTE OFFSET
AND addabo(v, ot, w) BE
    { LET p = ot/2
       LET t = ?
       TEST (ot & 1)=0 DO
        t := (v!p>>8) | ((v!p&255)<<8)
       OR
        t := (v!p & 255) | (v!(p+1) & #177400)
 
       pwabo(v, ot, t+w)
    }

 
//SECTION "CG8"
 
//GET "CGHDR"
 
LET outputsection() BE
{  LET rl = reflist
    LET r = 0
 
    UNTIL rl=0 DO      // FILL IN LABEL REFS
    { LET l = h2!rl
       AND a = h3!rl
       LET labval = 0
       TEST l>0
         THEN r := r+1
         ELSE l := -l
       labval := labv!l
       IF labval=-1 DO
          cgerror("LABEL L%N UNSET", FALSE, l)
       addabo(stv, a, labval)
       rl := !rl  }
    selectoutput(codestream)
 
    { objword(t_hunk)
       objword((stvp+1)>>1)
       objword(dsize)
//*<3032
       writewords(stv, (stvp+1)>>1)
/*3032>*/
/*<8086
      MWRITEWORDS(STV, (STVP+1)>>1)
/*8086>*/
       IF r>0 DO               // OUTPUT RELOCB BLOCK
       { objword(t_relocb)
          objword(r)
          rl := reflist
          UNTIL rl=0 DO
          { IF h2!rl>0 DO objword(h3!rl)
             rl := !rl }
       }
       objword(t_end)
    }
    selectoutput(verstream)
}
 
 /*<8086
AND MWRITEWORDS(V, N) BE
{
  FOR I = 0 TO N-1 DO
    V!I := ((V!I)<<8) | ((V!I)>>8)
  WRITEWORDS(V, N)
}
 /*8086>*/
 
//*<3032
AND askii(c) = c
 
AND iseven() = (stvp&1)=0
 
AND getvbyte(v, p) = VALOF
{ LET ot = p/2
   IF (p&1)=0 RESULTIS (v!ot&#177400)>>8
   RESULTIS v!ot & 255
}
 
AND putvbyte(v, p, w) BE
{ LET ot=p/2
   TEST (p&1)=0 DO v!ot := (v!ot&255) + ((w&255)<<8)
     OR v!ot := (v!ot&#177400) + (w&255)
}
 
 //*<3032
AND objword(w) BE
{ LET x = (w>>8) | ((w&255)<<8)
   writewords(@x, 1)
}
 /*3032>*/
 
/*<8086
AND OBJWORD(W) BE WRITEWORDS(@W, 1)
/*8086>*/
 
 
 
 //*<3032
AND writewords(v, n) BE
     FOR i = 0 TO n-1 DO
     { IF writewcount >= 16 DO
        { newline()
           writewcount := 0
        }
 
        writehex(v!i, 4)
        writewcount := writewcount + 1
     }
 
AND dboutput() BE
{  LET nl = "*N      "
    writef("OP=%N PNDOP=%N SSP=%N LOC=%X4*NSTACK ",
           op,pendingop,ssp,stvp)
    FOR p=arg1 TO tempv BY -3 DO
    { IF (arg1-p) REM 30 = 27 DO writes(nl)
       wrkn(h1!p,h2!p) }
    writes("*NBREFS ")
    FOR p=brefv TO brefp-2 BY 2 DO
    { IF p-brefv REM 10 = 8 DO writes(nl)
       writef("L%N %X4 ",p!0,p!1) }
    writes("*NREGS  ")
    FOR r=r0 TO r4 DO UNLESS reg_k!r=k_none DO
    { writef("R%N=",r)
       wrkn(reg_k!r,reg_n!r) }
    newline()
}
 
AND wrkn(k,n) BE
{  LET s = VALOF
       SWITCHON k INTO
       { DEFAULT: RESULTIS "?"
          CASE k_numb:   RESULTIS "N"
          CASE k_loc:    RESULTIS "P"
          CASE k_glob:   RESULTIS "G"
          CASE k_lab:    RESULTIS "L"
          CASE k_mloc:   RESULTIS "ML"
          CASE k_mglob:  RESULTIS "MG"
          CASE k_mlab:   RESULTIS "ML"
          CASE k_lvloc:  RESULTIS "@P"
          CASE k_lvglob: RESULTIS "@G"
          CASE k_lvlab:  RESULTIS "@L"
          CASE k_reg:    RESULTIS "R"
          CASE k_xbx:     RESULTIS "XBX "
          CASE k_xsi:     RESULTIS "XSI "
       }
    writef("%S%N  ",s,n)
}
 
    // LIST INSTRUCTIONS
AND listgen(f, k1, n1, k2, n2) BE IF cglisting DO
{   listchan := TRUE
     writef("*N%X4          %S", stvp, f)
     FOR i = 0 TO 6-getbyte(f, 0) DO wrch('*S')
     UNLESS k1=0 DO
     {   writeop(k1, n1)
          UNLESS k2=0 DO
          {   wrch(',')
               writeop(k2, n2)
          }
     }
}
 
AND writeop(m, v) BE
  SWITCHON m INTO
  {   CASE m_reg:
       writes(regstring(v))
       ENDCASE
 
       CASE m_glob:  m := tranr(r_di); GOTO po
       CASE m_loc:   m := tranr(r_bp); GOTO po
       CASE m_bx:    m := tranr(r_bx); GOTO po
       CASE m_si:    m := tranr(r_si)
po:    writef("%N(", v)
       writeop(m_reg, m)
       wrch(')')
       ENDCASE
 
       CASE m_imm:
       writef("!#X%X4", v)
       ENDCASE
 
       CASE m_lab:
       writef("L%N", v)
       ENDCASE
 
       DEFAULT:
       cgerror("BAD TYPE IN WRITEOP %N", FALSE, m)
  }
 
AND listat(t, v) BE IF cglisting DO
{   listchan := TRUE
 
     SWITCHON t INTO
     {   CASE s_datalab:
          listlab(v)
          ENDCASE
 
          CASE s_iteml:
          writef("*N%X4          DW L%N", stvp, v)
          ENDCASE
 
       CASE s_itemn:
          writef("*N%X4          DW %X4", stvp, v)
          ENDCASE
 
          DEFAULT:
          cgerror("BAD TYPE IN LISTAT %N", FALSE, t)
     }
}
 
AND listl(s, a, b, c, d, e) BE IF cglisting DO
{   listchan := TRUE
     writef("*N%X4          ", stvp)
     writef(s, a, b, c, d, e)
}
 
AND stringrb(t) = VALOF
  SWITCHON t INTO
  { CASE 0: RESULTIS "AL"
     CASE 1: RESULTIS "CL"
     CASE 2: RESULTIS "DL"
     CASE 3: RESULTIS "BL"
     CASE 4: RESULTIS "AH"
     CASE 5: RESULTIS "CH"
     CASE 6: RESULTIS "DH"
     CASE 7: RESULTIS "BH"
     DEFAULT: RESULTIS "BADREG"
  }
 
AND sf(f) = VALOF
  SWITCHON f INTO
  { CASE f_xor:    RESULTIS "XOR"
     CASE f_or:    RESULTIS "OR"
     CASE f_and:    RESULTIS "AND"
     CASE f_add:    RESULTIS "ADD"
     CASE f_sub:    RESULTIS "SUB"
     CASE f_cmp:    RESULTIS "CMP"
 
     CASE f_movimmtrm:
     CASE f_movrtrm:
     CASE f_movimmtr:    RESULTIS "MOV"
 
     CASE f_lea:     RESULTIS "LEA"
     CASE f_cwd:     RESULTIS "CWD"
 
     CASE f_jmp:     RESULTIS "JMP"
     CASE f_jge:     RESULTIS "JGE"
     CASE f_jg:      RESULTIS "JG"
     CASE f_jle:     RESULTIS "JLE"
     CASE f_jl:      RESULTIS "JL"
     CASE f_je:      RESULTIS "JE"
     CASE f_jne:     RESULTIS "JNE"
 
     CASE f_neg:     RESULTIS "NEG"
     CASE f_not:     RESULTIS "NOT"
 
     CASE f_shl:     RESULTIS "SHL"
     CASE f_shr:     RESULTIS "SHR"
     CASE f_sar:     RESULTIS "SAR"
 
     CASE f_nop:     RESULTIS "NOP"
 
     DEFAULT: RESULTIS "UNKNOWN"
  }
 
 
AND listlab(l) BE IF cglisting DO
{ listchan := TRUE
  writef("*N%X4  L%N EQU $", stvp, l)
}
 
 
AND regstring(r) = VALOF
  SWITCHON r INTO
  { CASE 3: RESULTIS "BX"
     CASE 0: RESULTIS "AX"
     CASE 1: RESULTIS "CX"
     CASE 2: RESULTIS "DX"
     CASE 6: RESULTIS "SI"
     CASE 5: RESULTIS "BP"
     CASE 4: RESULTIS "SP"
     CASE 7: RESULTIS "DI"
     DEFAULT: RESULTIS "BADREG"
  }
 /*3032>*/
