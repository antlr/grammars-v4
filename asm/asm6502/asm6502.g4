/*
BSD License

Copyright (c) 2013, Tom Everett
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

grammar asm6502;

options { caseInsensitive = true; }

prog
   : line* EOF
   ;

line
   : (instruction | assemblerinstruction | lbl)? EOL
   ;

instruction
   : label? opcode argumentlist?
   ;

assemblerinstruction
   : argument? assembleropcode argumentlist?
   ;

assembleropcode
   : ASSEMBLER_INSTRUCTION
   ;

lbl
   : label ':'
   ;

argumentlist
   : argument (',' argumentlist)?
   ;

label
   : name
   ;

argument
   : prefix_? (number | name | string_ | '*') (('+' | '-') number)?
   | '(' argument ')'
   ;

prefix_
   : '#'
   ;

string_
   : STRING
   ;

name
   : NAME
   ;

number
   : NUMBER
   ;

opcode
   : ADC
   | AND
   | ASL
   | BCC
   | BCS
   | BEQ
   | BIT
   | BMI
   | BNE
   | BPL
   | BRA
   | BRK
   | BVC
   | BVS
   | CLC
   | CLD
   | CLI
   | CLV
   | CMP
   | CPX
   | CPY
   | DEC
   | DEX
   | DEY
   | EOR
   | INC
   | INX
   | INY
   | JMP
   | JSR
   | LDA
   | LDY
   | LDX
   | LSR
   | NOP
   | ORA
   | PHA
   | PHX
   | PHY
   | PHP
   | PLA
   | PLP
   | PLY
   | ROL
   | ROR
   | RTI
   | RTS
   | SBC
   | SEC
   | SED
   | SEI
   | STA
   | STX
   | STY
   | STZ
   | TAX
   | TAY
   | TSX
   | TXA
   | TXS
   | TYA
   ;


ASSEMBLER_INSTRUCTION
   : 'ORG' | 'EQU' | 'ASC' | 'DS' | 'DFC' | '='
   ;

/*
* opcodes
*/

ADC: 'ADC';
AND: 'AND';
ASL: 'ASL';
BCC: 'BCC';
BCS: 'BCS';
BEQ: 'BEQ';
BIT: 'BIT';
BMI: 'BMI';
BNE: 'BNE';
BPL: 'BPL';
BRA: 'BRA';
BRK: 'BRK';
BVC: 'BVC';
BVS: 'BVS';
CLC: 'CLC';
CLD: 'CLD';
CLI: 'CLI';
CLV: 'CLV';
CMP: 'CMP';
CPX: 'CPX';
CPY: 'CPY';
DEC: 'DEC';
DEX: 'DEX';
DEY: 'DEY';
EOR: 'EOR';
INC: 'INC';
INX: 'INX';
INY: 'INY';
JMP: 'JMP';
JSR: 'JSR';
LDA: 'LDA';
LDY: 'LDY';
LDX: 'LDX';
LSR: 'LSR';
NOP: 'NOP';
ORA: 'ORA';
PHA: 'PHA';
PHX: 'PHX';
PHY: 'PHY';
PHP: 'PHP';
PLA: 'PLA';
PLP: 'PLP';
PLY: 'PLY';
ROL: 'ROL';
ROR: 'ROR';
RTI: 'RTI';
RTS: 'RTS';
SBC: 'SBC';
SEC: 'SEC';
SED: 'SED';
SEI: 'SEI';
STA: 'STA';
STX: 'STX';
STY: 'STY';
STZ: 'STZ';
TAX: 'TAX';
TAY: 'TAY';
TSX: 'TSX';
TXA: 'TXA';
TXS: 'TXS';
TYA: 'TYA';


NAME
   : [A-Z] [A-Z0-9."]*
   ;


NUMBER
   : '$'? [0-9A-F] +
   ;


COMMENT
   : ';' ~ [\r\n]* -> skip
   ;


STRING
   : '"' ~ ["]* '"'
   ;


EOL
   : [\r\n] +
   ;


WS
   : [ \t] -> skip
   ;
