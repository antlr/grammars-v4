/*
BSD License

Copyright (c) 2018, Tom Everett
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:

1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.
3. Neither the name of Tom Everett nor the names of its contributors
   may be used to endorse or promote products derived from this software
   without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

grammar asm8086;

prog
   : line* EOF
   ;

line
   : lbl? (assemblerdirective | instruction)? ('!' instruction)* EOL
   ;

instruction
   : rep? opcode expressionlist?
   ;

lbl
   : label COLON?
   ;

assemblerdirective
   : org
   | end
   | if_
   | endif_
   | equ
   | db
   | dw
   | cseg
   | dd
   | dseg
   | title
   | include_
   | rw
   | rb
   | rs
   | DOT
   ;

rw
   : name? RW expression
   ;

rb
   : name? RB expression
   ;

rs
   : name? RS expression
   ;

cseg
   : CSEG expression?
   ;

dseg
   : DSEG expression?
   ;

dw
   : DW expressionlist
   ;

db
   : DB expressionlist
   ;

dd
   : DD expressionlist
   ;

equ
   : name EQU expression
   ;

if_
   : IF assemblerexpression
   ;

assemblerexpression
   : assemblerterm (assemblerlogical assemblerterm)*
   | RP assemblerexpression LP
   ;

assemblerlogical
   : EQ
   | NE
   ;

assemblerterm
   : name
   | number
   | NOT assemblerterm
   ;

endif_
   : ENDIF
   ;

end
   : END
   ;

org
   : ORG expression
   ;

title
   : TITLE string_
   ;

include_
   : INCLUDE name
   ;

expressionlist
   : expression (COMMA expression)*
   ;

label
   : name
   ;

expression
   : multiplyingExpression (sign multiplyingExpression)*
   ;

multiplyingExpression
   : argument ((STAR | SLASH | MOD | AND) argument)*
   ;

argument
   : number
   | dollar
   | register_
   | name
   | string_
   | RP expression LP
   | (number | name)? LB expression RB_
   | ptr expression
   | NOT expression
   | OFFSET expression
   | LENGTH expression
   | register_ COLON expression
   ;

ptr
   : (BYTE | WORD | DWORD)? PTR
   ;

dollar
   : DOLLAR
   ;

register_
   : AH
   | AL
   | BH
   | BL
   | CH
   | CL
   | DH
   | DL
   | AX
   | BX
   | CX
   | DX
   | CI
   | DI
   | BP
   | SP
   | IP
   | CS
   | DS
   | ES
   | SS
   ;

string_
   : STRING
   ;

name
   : NAME
   ;

number
   : sign? NUMBER
   ;

opcode
   : AAA
   | AAD
   | AAM
   | AAS
   | ADC
   | ADD
   | AND
   | CALL
   | CBW
   | CLC
   | CLD
   | CLI
   | CMC
   | CMP
   | CMPSB
   | CMPSW
   | CWD
   | DAA
   | DAS
   | DEC
   | DIV
   | ESC
   | HLT
   | IDIV
   | IMUL
   | IN
   | INC
   | INT
   | INTO
   | IRET
   | JA
   | JAE
   | JB
   | JBE
   | JC
   | JE
   | JG
   | JGE
   | JL
   | JLE
   | JNA
   | JNAE
   | JNB
   | JNBE
   | JNC
   | JNE
   | JNG
   | JNGE
   | JNL
   | JNLE
   | JNO
   | JNP
   | JNS
   | JNZ
   | JO
   | JP
   | JPE
   | JPO
   | JS
   | JZ
   | JCXZ
   | JMP
   | JMPS
   | JMPF
   | LAHF
   | LDS
   | LEA
   | LES
   | LOCK
   | LODS
   | LODSB
   | LODSW
   | LOOP
   | LOOPE
   | LOOPNE
   | LOOPNZ
   | LOOPZ
   | MOV
   | MOVS
   | MOVSB
   | MOVSW
   | MUL
   | NEG
   | NOP
   | NOT
   | OR
   | OUT
   | POP
   | POPF
   | PUSH
   | PUSHF
   | RCL
   | RCR
   | RET
   | RETN
   | RETF
   | ROL
   | ROR
   | SAHF
   | SAL
   | SAR
   | SALC
   | SBB
   | SCASB
   | SCASW
   | SHL
   | SHR
   | STC
   | STD
   | STI
   | STOSB
   | STOSW
   | SUB
   | TEST
   | WAIT
   | XCHG
   | XLAT
   | XOR
   ;

rep
   : REP
   | REPE
   | REPNE
   | REPNZ
   | REPZ
   ;

sign : PLUS | MINUS ;

BYTE : B Y T E ;
WORD : W O R D ;
DWORD : D W O R D ;
DSEG : D S E G ;
CSEG : C S E G ;
INCLUDE : I N C L U D E ;
TITLE : T I T L E ;
END : E N D ;
ORG : O R G ;
ENDIF : E N D I F ;
IF : I F ;
EQU : E Q U ;
DW : D W ;
DB : D B ;
DD : D D ;
PTR : P T R ;
OFFSET : O F F S E T ;
RW : R W ;
RB : R B ;
RS : R S ;
LENGTH : L E N G T H ;
EQ : E Q ;
NE : N E ;
MOD : M O D ;

COMMENT
   : ';' ~ [\r\n]* -> skip
   ;

AH : A H ;
AL : A L ;
BH : B H ;
BL : B L ;
CH : C H ;
CL : C L ;
DH : D H ;
DL : D L ;
AX : A X ;
BX : B X ;
CX : C X ;
DX : D X ;
CI : C I ;
DI : D I ;
BP : B P ;
SP : S P ;
IP : I P ;
CS : C S ;
DS : D S ;
ES : E S ;
SS : S S ;

AAA : A A A ;
AAD : A A D ;
AAM : A A M ;
AAS : A A S ;
ADC : A D C ;
ADD : A D D ;
AND : A N D ;
CALL : C A L L ;
CBW : C B W ;
CLC : C L C ;
CLD : C L D ;
CLI : C L I ;
CMC : C M C ;
CMP : C M P ;
CMPSB : C M P S B ;
CMPSW : C M P S W ;
CWD : C W D ;
DAA : D A A ;
DAS : D A S ;
DEC : D E C ;
DIV : D I V ;
ESC : E S C ;
HLT : H L T ;
IDIV : I D I V ;
IMUL : I M U L ;
IN : I N ;
INC : I N C ;
INT : I N T ;
INTO : I N T O ;
IRET : I R E T ;
JA : J A ;
JAE : J A E ;
JB : J B ;
JBE : J B E ;
JC : J C ;
JE : J E ;
JG : J G ;
JGE : J G E ;
JL : J L ;
JLE : J L E ;
JNA : J N A ;
JNAE : J N A E ;
JNB : J N B ;
JNBE : J N B E ;
JNC : J N C ;
JNE : J N E ;
JNG : J N G ;
JNGE : J N G E ;
JNL : J N L ;
JNLE : J N L E ;
JNO : J N O ;
JNP : J N P ;
JNS : J N S ;
JNZ : J N Z ;
JO : J O ;
JP : J P ;
JPE : J P E ;
JPO : J P O ;
JS : J S ;
JZ : J Z ;
JCXZ : J C X Z ;
JMP : J M P ;
JMPS : J M P S ;
JMPF : J M P F ;
LAHF : L A H F ;
LDS : L D S ;
LEA : L E A ;
LES : L E S ;
LOCK : L O C K ;
LODS : L O D S ;
LODSB : L O D S B ;
LODSW : L O D S W ;
LOOP : L O O P ;
LOOPE : L O O P E ;
LOOPNE : L O O P N E ;
LOOPNZ : L O O P N Z ;
LOOPZ : L O O P Z ;
MOV : M O V ;
MOVS : M O V S ;
MOVSB : M O V S B ;
MOVSW : M O V S W ;
MUL : M U L ;
NEG : N E G ;
NOP : N O P ;
NOT : N O T ;
OR : O R ;
OUT : O U T ;
POP : P O P ;
POPF : P O P F ;
PUSH : P U S H ;
PUSHF : P U S H F ;
RCL : R C L ;
RCR : R C R ;
RET : R E T ;
RETN : R E T N ;
RETF : R E T F ;
ROL : R O L ;
ROR : R O R ;
SAHF : S A H F ;
SAL : S A L ;
SAR : S A R ;
SALC : S A L C ;
SBB : S B B ;
SCASB : S C A S B ;
SCASW : S C A S W ;
SHL : S H L ;
SHR : S H R ;
STC : S T C ;
STD : S T D ;
STI : S T I ;
STOSB : S T O S B ;
STOSW : S T O S W ;
SUB : S U B ;
TEST : T E S T ;
WAIT : W A I T ;
XCHG : X C H G ;
XLAT : X L A T ;
XOR : X O R ;

REP : R E P ;
REPE : R E P E ;
REPNE : R E P N E ;
REPNZ : R E P N Z ;
REPZ : R E P Z ;


STAR : '*' ;
SLASH : '/' ;
DOLLAR : '$' ;
PLUS : '+' ;
MINUS : '-' ;
NOT_ : '!' ;
COLON : ':' ;
DOT : '.' ;
RP : '(' ;
LP : ')' ;
COMMA : ',' ;
SEMI : ';' ;
LB : '[' ;
RB_ : ']' ;


NAME
   : [.a-zA-Z] [a-zA-Z0-9."_]*
   ;

NUMBER
   : [0-9a-fA-F] + ('H' | 'h')?
   ;

STRING
   : '\u0027' ~'\u0027'* '\u0027'
   ;

EOL
   : [\r\n] +
   ;

WS
   : [ \t] -> skip
   ;

fragment A
   : ('a' | 'A')
   ;

fragment B
   : ('b' | 'B')
   ;

fragment C
   : ('c' | 'C')
   ;

fragment D
   : ('d' | 'D')
   ;

fragment E
   : ('e' | 'E')
   ;

fragment F
   : ('f' | 'F')
   ;

fragment G
   : ('g' | 'G')
   ;

fragment H
   : ('h' | 'H')
   ;

fragment I
   : ('i' | 'I')
   ;

fragment J
   : ('j' | 'J')
   ;

fragment K
   : ('k' | 'K')
   ;

fragment L
   : ('l' | 'L')
   ;

fragment M
   : ('m' | 'M')
   ;

fragment N
   : ('n' | 'N')
   ;

fragment O
   : ('o' | 'O')
   ;

fragment P
   : ('p' | 'P')
   ;


fragment Q
   : ('q' | 'Q')
   ;

fragment R
   : ('r' | 'R')
   ;

fragment S
   : ('s' | 'S')
   ;

fragment T
   : ('t' | 'T')
   ;

fragment U
   : ('u' | 'U')
   ;

fragment V
   : ('v' | 'V')
   ;

fragment W
   : ('w' | 'W')
   ;

fragment X
   : ('x' | 'X')
   ;

fragment Y
   : ('y' | 'Y')
   ;

fragment Z
   : ('z' | 'Z')
   ;
