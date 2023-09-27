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

options { caseInsensitive = true; }

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

BYTE: 'BYTE';
WORD: 'WORD';
DWORD: 'DWORD';
DSEG: 'DSEG';
CSEG: 'CSEG';
INCLUDE: 'INCLUDE';
TITLE: 'TITLE';
END: 'END';
ORG: 'ORG';
ENDIF: 'ENDIF';
IF: 'IF';
EQU: 'EQU';
DW: 'DW';
DB: 'DB';
DD: 'DD';
PTR: 'PTR';
OFFSET: 'OFFSET';
RW: 'RW';
RB: 'RB';
RS: 'RS';
LENGTH: 'LENGTH';
EQ: 'EQ';
NE: 'NE';
MOD: 'MOD';

COMMENT
   : ';' ~ [\r\n]* -> skip
   ;

AH: 'AH';
AL: 'AL';
BH: 'BH';
BL: 'BL';
CH: 'CH';
CL: 'CL';
DH: 'DH';
DL: 'DL';
AX: 'AX';
BX: 'BX';
CX: 'CX';
DX: 'DX';
CI: 'CI';
DI: 'DI';
BP: 'BP';
SP: 'SP';
IP: 'IP';
CS: 'CS';
DS: 'DS';
ES: 'ES';
SS: 'SS';

AAA: 'AAA';
AAD: 'AAD';
AAM: 'AAM';
AAS: 'AAS';
ADC: 'ADC';
ADD: 'ADD';
AND: 'AND';
CALL: 'CALL';
CBW: 'CBW';
CLC: 'CLC';
CLD: 'CLD';
CLI: 'CLI';
CMC: 'CMC';
CMP: 'CMP';
CMPSB: 'CMPSB';
CMPSW: 'CMPSW';
CWD: 'CWD';
DAA: 'DAA';
DAS: 'DAS';
DEC: 'DEC';
DIV: 'DIV';
ESC: 'ESC';
HLT: 'HLT';
IDIV: 'IDIV';
IMUL: 'IMUL';
IN: 'IN';
INC: 'INC';
INT: 'INT';
INTO: 'INTO';
IRET: 'IRET';
JA: 'JA';
JAE: 'JAE';
JB: 'JB';
JBE: 'JBE';
JC: 'JC';
JE: 'JE';
JG: 'JG';
JGE: 'JGE';
JL: 'JL';
JLE: 'JLE';
JNA: 'JNA';
JNAE: 'JNAE';
JNB: 'JNB';
JNBE: 'JNBE';
JNC: 'JNC';
JNE: 'JNE';
JNG: 'JNG';
JNGE: 'JNGE';
JNL: 'JNL';
JNLE: 'JNLE';
JNO: 'JNO';
JNP: 'JNP';
JNS: 'JNS';
JNZ: 'JNZ';
JO: 'JO';
JP: 'JP';
JPE: 'JPE';
JPO: 'JPO';
JS: 'JS';
JZ: 'JZ';
JCXZ: 'JCXZ';
JMP: 'JMP';
JMPS: 'JMPS';
JMPF: 'JMPF';
LAHF: 'LAHF';
LDS: 'LDS';
LEA: 'LEA';
LES: 'LES';
LOCK: 'LOCK';
LODS: 'LODS';
LODSB: 'LODSB';
LODSW: 'LODSW';
LOOP: 'LOOP';
LOOPE: 'LOOPE';
LOOPNE: 'LOOPNE';
LOOPNZ: 'LOOPNZ';
LOOPZ: 'LOOPZ';
MOV: 'MOV';
MOVS: 'MOVS';
MOVSB: 'MOVSB';
MOVSW: 'MOVSW';
MUL: 'MUL';
NEG: 'NEG';
NOP: 'NOP';
NOT: 'NOT';
OR: 'OR';
OUT: 'OUT';
POP: 'POP';
POPF: 'POPF';
PUSH: 'PUSH';
PUSHF: 'PUSHF';
RCL: 'RCL';
RCR: 'RCR';
RET: 'RET';
RETN: 'RETN';
RETF: 'RETF';
ROL: 'ROL';
ROR: 'ROR';
SAHF: 'SAHF';
SAL: 'SAL';
SAR: 'SAR';
SALC: 'SALC';
SBB: 'SBB';
SCASB: 'SCASB';
SCASW: 'SCASW';
SHL: 'SHL';
SHR: 'SHR';
STC: 'STC';
STD: 'STD';
STI: 'STI';
STOSB: 'STOSB';
STOSW: 'STOSW';
SUB: 'SUB';
TEST: 'TEST';
WAIT: 'WAIT';
XCHG: 'XCHG';
XLAT: 'XLAT';
XOR: 'XOR';

REP: 'REP';
REPE: 'REPE';
REPNE: 'REPNE';
REPNZ: 'REPNZ';
REPZ: 'REPZ';


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
   : [.A-Z] [A-Z0-9."_]*
   ;

NUMBER
   : [0-9A-F] + 'H'?
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
