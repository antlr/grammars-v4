//     This program is an ASCII INTCODE assembler and interpreter
// for a 16 bit EBCDIC machine,  hence the need for the ASCII and
// EBCDIC tables near the end.  It has been tested on the IBM 370
// (a 32 bit EBCDIC machine).
 
GET "libhdr"
 
GLOBAL $(
sysprint:200; source:201
etoa:202; atoe:203
$)
 
MANIFEST $(
fshift=13
ibit=#10000; pbit=#4000; gbit=#2000; dbit=#1000
abits=#777
wordsize=16; bytesize=8
lig1=#012001
k2  =#140002
x22 =#160026
$)
 
GLOBAL $(
g:210; p:211; ch:212; cyclecount:213
labv:220; cp:221; a:222; b:223; c:224; d:225; w:226
$)
 
 
 
LET assemble() BE
$( LET v = VEC 700
   LET f = 0
   labv := v
 
clear:
   FOR i = 0 TO 500 DO labv!i := 0
   cp := 0
 
next:
   rch()
sw:SWITCHON ch INTO
 
   $( DEFAULT: IF ch=endstreamch RETURN
               writef("*nbad ch %c at p = %n*n", ch, p)
               GOTO next
 
      CASE '0':CASE '1':CASE '2':CASE '3':CASE '4':
      CASE '5':CASE '6':CASE '7':CASE '8':CASE '9':
               setlab(rdn())
               cp := 0
               GOTO sw
 
      CASE '$':CASE '*s':CASE '*n': GOTO next
 
      CASE 'L': f := 0; ENDCASE
      CASE 'S': f := 1; ENDCASE
      CASE 'A': f := 2; ENDCASE
      CASE 'J': f := 3; ENDCASE
      CASE 'T': f := 4; ENDCASE
      CASE 'F': f := 5; ENDCASE
      CASE 'K': f := 6; ENDCASE
      CASE 'X': f := 7; ENDCASE
 
      CASE 'C': rch(); stc(rdn()); GOTO sw
 
      CASE 'D': rch()
                TEST ch='L'
                THEN $( rch()
                        stw(0)
                        labref(rdn(), p-1)
                     $)
                ELSE stw(rdn())
                GOTO sw
 
      CASE 'G': rch()
                a := rdn() + g
                TEST ch='L'
                THEN rch()
                ELSE writef("*nbad code at p = %n*n", p)
                !a := 0
                labref(rdn(), a)
                GOTO sw
 
      CASE 'Z': FOR i = 0 TO 500 IF labv!i>0 DO writef("L%n unset*n", i)
                GOTO clear
   $)
 
 
   w := f<<fshift
   rch()
   IF ch='I' DO $( w := w+ibit; rch() $)
   IF ch='P' DO $( w := w+pbit; rch() $)
   IF ch='G' DO $( w := w+gbit; rch() $)
 
   TEST ch='L'
   THEN $( rch()
           stw(w+dbit)
           stw(0)
           labref(rdn(), p-1)
        $)
   ELSE $( LET a = rdn()
           TEST (a&abits)=a
           THEN stw(w+a)
           ELSE $( stw(w+dbit); stw(a)  $)
        $)
 
   GOTO sw
$)
 
AND stw(w) BE $( !p := w
                 p, cp := p+1, 0
              $)
 
AND stc(c) BE $( IF cp=0 DO $( stw(0)
                               cp := wordsize
                            $)
                 cp := cp - bytesize
                 !(p-1) := !(p-1) + (c<<cp)
              $)
 
AND rch() BE $( ch := rdch()
                UNLESS ch='/' RETURN
                UNTIL ch='*n' DO ch := rdch()
             $) REPEAT
 
AND rdn() = VALOF
$( LET a, b = 0, FALSE
   IF ch='-' DO $( b := TRUE; rch()  $)
   WHILE '0'<=ch<='9' DO $( a := 10*a + ch - '0'; rch()  $)
   IF b DO a := -a
   RESULTIS a
$)
 
AND setlab(n) BE
$( LET k = labv!n
   IF k<0 DO writef("l%n already set TO %n at p = %n*n",n,-k,p)
   WHILE k>0 DO $( LET n = !k
                   !k := p
                   k := n
                $)
   labv!n := -p
$)
 
 
AND labref(n, a) BE
$( LET k = labv!n
   TEST k<0 THEN k := -k ELSE labv!n := a
   !a := !a + k
$)
 
 
AND interpret() = VALOF
$( cyclecount := cyclecount + 1
   w := !c
   c := c + 1
 
   TEST (w&dbit)=0 THEN d := w&abits
                   ELSE $( d := !c; c := c+1  $)
 
   IF (w & pbit) NE 0 DO d := d + p
   IF (w & gbit) NE 0 DO d := d + g
   IF (w & ibit) NE 0 DO d := !d
 
   SWITCHON w>>fshift INTO
 
   $( error:
      DEFAULT: selectoutput(sysprint)
               writef("*nintcode error at c = %n*n", c-1)
               RESULTIS -1
 
      CASE 0: b := a; a := d; LOOP
 
      CASE 1: !d := a; LOOP
 
      CASE 2: a := a + d; LOOP
 
      CASE 3: c := d; LOOP
 
      CASE 4: IF a DO c := d; LOOP
 
      CASE 5: UNLESS a DO c := d; LOOP
 
      CASE 6: d := p + d
              d!0, d!1 := p, c
              p, c := d, a
              LOOP
 
      CASE 7: SWITCHON d INTO
 
      $( DEFAULT: GOTO error
 
         CASE 1:  a := !a; LOOP
         CASE 2:  a := -a;     LOOP
         CASE 3:  a := NOT a; LOOP
         CASE 4:  c := p!1
                  p := p!0
                  LOOP
         CASE 5:  a := b * a; LOOP
         CASE 6:  a := b / a; LOOP
         CASE 7:  a := b REM a; LOOP
         CASE 8:  a := b + a; LOOP
         CASE 9:  a := b - a; LOOP
         CASE 10: a := b = a; LOOP
         CASE 11: a := b NE a; LOOP
         CASE 12: a := b < a; LOOP
         CASE 13: a := b >= a; LOOP
         CASE 14: a := b > a; LOOP
         CASE 15: a := b <= a; LOOP
         CASE 16: a := b << a; LOOP
         CASE 17: a := b >> a; LOOP
         CASE 18: a := b & a; LOOP
         CASE 19: a := b | a; LOOP
         CASE 20: a := b NEQV a; LOOP
         CASE 21: a := b EQV a;  LOOP
 
         CASE 22: RESULTIS 0   // finish
 
         CASE 23: b, d := c!0, c!1   // SWITCHON
                  UNTIL b=0 DO
                  $( b, c := b-1, c+2
                     IF a=c!0 DO
                     $( d := c!1
                        BREAK
                     $)
                  $)
                  c := d
                  LOOP
         CASE 24: a := b % a; LOOP
         CASE 25: b % a := p!(c!0)
                  c := c+1
                  LOOP
         CASE 26: a := ABS a; LOOP
 
// cases 40 upwards are only called from the following
// hand written intcode library - iclib:
 
//    11 LIP2 X40 X4 G11L11 /selectinput
//    12 LIP2 X41 X4 G12L12 /selectoutput
//    13 X42 X4      G13L13 /rdch
//    14 LIP2 X43 X4 G14L14 /wrch
//    42 LIP2 X44 X4 G42L42 /findinput
//    41 LIP2 X45 X4 G41L41 /findoutput
//    30 LIP2 X46 X4 G30L30 /stop
//    31 X47 X4 G31L31 /level
//    32 LIP3 LIP2 X48 G32L32 /longjump
//    46 X49 X4 G46L46 /endread
//    47 X50 X4 G47L47 /endwrite
//    40 LIP3 LIP2 X51 G40L40 /aptovec
//    85 LIP3 LIP2 X52 X4 G85L85 / getbyte
//    86 LIP3 LIP2 X53 X4 G86L86 / putbyte
//    Z
 
         CASE 40: selectinput(a); LOOP
         CASE 41: selectoutput(a); LOOP
         CASE 42: a := etoa!rdch(); LOOP
         CASE 43: wrch(atoe!a); LOOP
         CASE 44: a := findinput(string370(a)); LOOP
         CASE 45: a := findoutput(string370(a)); LOOP
         CASE 46: RESULTIS a  // stop(a)
         CASE 47: a := p!0; LOOP  // used in level()
         CASE 48: p, c := a, b;         // used in longjump(p,l)
                  LOOP
         CASE 49: endread(); LOOP
         CASE 50: endwrite(); LOOP
         CASE 51: d := p+b+1        // used in aptovec(f, n)
                  d!0, d!1, d!2, d!3 := p!0, p!1, p, b
                  p, c := d, a
                  LOOP
         CASE 52: a := icgetbyte(a, b)  // getbyte(s, i)
                  LOOP
         CASE 53: icputbyte(a, b, p!4)  // putbyte(s, i, ch)
                  LOOP
      $)
   $)
$) REPEAT
 
 
AND string370(s) = VALOF
$( LET t = TABLE 0,0,0,0,0,0,0,0
 
   t%0 := icgetbyte(s, 0)
   FOR i = 1 TO icgetbyte(s,0) DO t%i := atoe!icgetbyte(s,i)
   RESULTIS t
$)
 
AND icgetbyte(s, i) = VALOF
$( LET w = s!(i/2)
   IF (i&1)=0 DO w := w>>8
   RESULTIS w&255
$)
 
AND icputbyte(s, i, ch) BE
$( LET p = @s!(i/2)
   LET w = !p
   TEST (i&1)=0 THEN !p := w&#x00FF | ch<<8
                ELSE   !p := w&#xFF00 | ch
$)
 
LET start(parm) BE
$( LET progvec = VEC 20000
   LET globvec = VEC 500
 
   g, p := globvec, progvec
 
   sysprint := output()
 
   writes("intcode system entered*n")
 
   source := findinput("INTIN")
   IF source=0 DO
   $( writes("Cannot open INTIN*n")
      RETURN
   $)
   selectinput(source)
   assemble()
   source := findinput("SYSIN")
   UNLESS source=0 DO selectinput(source)
 
   writef("*nprogram size = %n*n", p-progvec)
 
   atoe := 1+TABLE -1,
          0,  0,  0,  0,  0,  0,  0,  0,  // ascii to ebcdic
          0,  5, 21,  0, 12,  0,  0,  0,  // '*t' '*n' '*p'
          0,  0,  0,  0,  0,  0,  0,  0,
          0,  0,  0,  0,  0,  0,  0,  0,
 
         64, 90,127,123, 91,108, 80,125, // '*s' ! " # $ % & '
         77, 93, 92, 78,107, 96, 75, 97, //   (  ) * + , - . /
        240,241,242,243,244,245,246,247, //   0  1 2 3 4 5 6 7
        248,249,122, 94, 76,126,110,111, //   8  9 : ; < = > ?
        124,193,194,195,196,197,198,199, //   @  A B C D E F G
        200,201,209,210,211,212,213,214, //   H  I J K L M N O
        215,216,217,226,227,228,229,230, //   P  Q R S T U V W
        231,232,233, 66, 98, 67,101,102, //   X  Y Z [ \ ] ? ?
         64,129,130,131,132,133,134,135, //      a b c d e f g
        136,137,145,146,147,148,149,150, //   h  i j k l m n o
        151,152,153,162,163,164,165,166, //   p  q r s t u v w
        167,168,169, 64, 79, 64, 95,255  //   x  y z   |   ~
 
 
   etoa := 1+TABLE -1,
      0,   0,   0,   0,   0, #11,   0,   0,
      0,   0,   0, #13, #14, #15,   0,   0,
      0,   0,   0,   0,   0, #12,   0,   0,
      0,   0,   0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0, #12,   0,   0,
      0,   0,   0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,   0,   0,
    #40,   0,#133,#135,   0,   0,   0,   0,
      0,   0,   0, #56, #74, #50, #53,#174,
    #46,   0,   0,   0,   0,   0,   0,   0,
      0,   0, #41, #44, #52, #51, #73,#176,
    #55, #57,#134,   0,   0,#136,#137,   0,
      0,   0,   0, #54, #45,#140, #76, #77,
      0,   0,   0,   0,   0,   0,   0,   0,
      0,   0, #72, #43,#100, #47, #75, #42,
      0,#141,#142,#143,#144,#145,#146,#147,
   #150,#151,   0,   0,   0,   0,   0,   0,
      0,#152,#153,#154,#155,#156,#157,#160,
   #161,#162,   0,   0,   0,   0,   0,   0,
      0,   0,#163,#164,#165,#166,#167,#170,
   #171,#172,   0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,   0,   0,
      0,#101,#102,#103,#104,#105,#106,#107,
   #110,#111,   0,   0,   0,   0,   0,   0,
      0,#112,#113,#114,#115,#116,#117,#120,
   #121,#122,   0,   0,   0,   0,   0,   0,
      0,   0,#123,#124,#125,#126,#127,#130,
   #131,#132,   0,   0,   0,   0,   0,   0,
    #60, #61, #62, #63, #64, #65, #66, #67,
    #70, #71,   0,   0,   0,   0,   0,   0
 
 
 
   c := TABLE lig1, k2, x22
 
   cyclecount := 0
   a := interpret()
 
   selectoutput(sysprint)
   writef("*n*nexecution cycles = %n, code = %n*n", cyclecount, a)
//IF a<0 DO mapstore()
$)
 
 
 
