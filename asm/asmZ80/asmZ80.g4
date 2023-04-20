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
/*
* http://fms.komkon.org/comp/CPUs/z80.txt
*/

grammar asmZ80;

options { caseInsensitive = true; }

prog
   : EOL* ((line EOL)* line EOL*)? EOF
   ;

line
   : lbl? (instruction | directive) comment?
   | lbl comment?
   | comment
   ;

instruction
   : opcode expressionlist?
   ;

opcode
   : OPCODE
   ;

register_
   : REGISTER
   ;

directive
   : argument? assemblerdirective expressionlist
   ;

assemblerdirective
   : ASSEMBLER_DIRECTIVE
   ;

lbl
   : label ':'?
   ;

expressionlist
   : expression (',' expression)*
   ;

label
   : name
   ;

expression
   : multiplyingExpression (('+' | '-') multiplyingExpression)*
   ;

multiplyingExpression
   : argument (('*' | '/') argument)*
   ;

argument
   : number
   | register_
   | dollar
   | name
   | string_
   | '(' expression ')'
   ;

dollar
   : '$'
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

comment
   : COMMENT
   ;

REGISTER
   : 'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'H' | 'L' | 'I' | 'R' | 'IXH' | 'IXL' | 'IYH' | 'IYL' | 'AF' | 'BC' | 'DE' | 'HL' | 'PC' | 'SP' | 'IX' | 'IY'
   ;

ASSEMBLER_DIRECTIVE
    : 'ORG'
    | 'END'
    | 'EQU'
    | 'DEFB'
    | 'DEFW'
    | 'DS'
    | 'IF'
    | 'ENDIF'
    | 'SET'
    ;

OPCODE
    : 'ADC'
    | 'ADD'
    | 'AND'
    | 'BIT'
    | 'CALL'
    | 'CCF'
    | 'CP'
    | 'CPD'
    | 'CPDR'
    | 'CPI'
    | 'CPIR'
    | 'CPL'
    | 'DAA'
    | 'DEC'
    | 'DI'
    | 'DJNZ'
    | 'EI'
    | 'EX'
    | 'EXX'
    | 'IM'
    | 'IN'
    | 'INC'
    | 'IND'
    | 'INDR'
    | 'INI'
    | 'INIR'
    | 'JP'
    | 'JR'
    | 'LD'
    | 'LDD'
    | 'LDDR'
    | 'LDI'
    | 'LDIR'
    | 'NEG'
    | 'NOP'
    | 'OR'
    | 'OTDR'
    | 'OTIR'
    | 'OUT'
    | 'OUTD'
    | 'OUTI'
    | 'POP'
    | 'PUSH'
    | 'RES'
    | 'RET'
    | 'RETI'
    | 'RETN'
    | 'RL'
    | 'RLA'
    | 'RLC'
    | 'RLCA'
    | 'RLD'
    | 'RR'
    | 'RRA'
    | 'RRC'
    | 'RRCA'
    | 'RRD'
    | 'RST'
    | 'SBC'
    | 'SCF'
    | 'SLA'
    | 'SLL'
    | 'SL1'
    | 'SRA'
    | 'SRL'
    | 'SUB'
    | 'XOR'
   ;

NAME
   : [A-Z] [A-Z0-9."]*
   ;

NUMBER
   : '$'? [0-9A-F]+ 'H'?
   ;

COMMENT
   : ';' ~ [\r\n]*
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
