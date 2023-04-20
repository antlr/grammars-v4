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

grammar asmMASM;

options { caseInsensitive=true; }

prog
   : line* EOF
   ;

line
   : (lbl | endlbl)? (assemblerdirective | masmdirectives | instruction)? EOL
   ;

instruction
   : rep? opcode expressionlist?
   ;

lbl
   : label ':'?
   ;

endlbl
   : END name?
   ;

assemblerdirective
   : org
   | if_
   | endif_
   | equ
   | db
   | dw
   | dm
   | ds
   | include
   | includelib
   | invoke
   | option
   | put
   | assign
   | segment
   | endsegment
   | group
   | label_
   | assume
   | extern_
   | public_
   | type_ expressionlist+
   ;

masmdirectives
   : masmdirective+
   ;

masmdirective
   : MASMDIRECTIVE expressionlist?
   ;

assume
   : ASSUME register_ ':' name (',' register_ ':' name)*
   ;

label_
   : name LABEL type_
   ;

type_
   : BYTE
   | SBYTE
   | WORD
   | DWORD
   ;

group
   : name GROUP name (',' name)*
   ;

segment
   : name SEGMENT align?
   ;

endsegment
   : name SEGMENTEND
   ;

align
   : BYTE | WORD | DWORD | PARA | PAGE
   | ALIGN '(' number ')'
   ;

assign
   : name ASSIGN expression
   ;

put
   : PUT expressionlist
   ;

include
   : INCLUDE expressionlist
   ;

includelib
   : INCLUDELIB expressionlist
   ;

invoke
   : INVOKE expressionlist
   ;

option
   : OPTION expressionlist
   ;

ds
   : DS expressionlist
   ;

dw
   : DW expressionlist
   ;

db
   : DB expressionlist
   ;

dm
   : DM expressionlist
   ;

dup
   : number DUP expression
   ;

equ
   : EQU expression
   ;

extern_
   : EXTERN expression
   ;

public_
   : PUBLIC expression
   ;

if_
   : IF expression
   ;

endif_
   : ENDIF
   ;

org
   : ORG expression
   ;

expressionlist
   : expression (',' expression)*
   ;

label
   : name
   | gross
   ;

expression
   : multiplyingExpression (SIGN multiplyingExpression)*
   ;

multiplyingExpression
   : argument (('*' | '/') argument)*
   ;

argument
   : number
   | dollar
   | ques
   | register_
   | (name ':')? name
   | string
   | '(' expression ')'
   | '[' expression ']'
   | NOT expression
   | OFFSET expression
   | gross
   | dup
   ;

/*
 MASM allows opcode names such as "RET" to be label names and also allows assemlber directives such as "PUT" as names
*/
gross
   : opcode
   | grossrawassemblerdirective
   ;

grossrawassemblerdirective
   : PUT
   | IF
   | ENDIF
   | ORG
   | EQU
   ;

dollar
   : DOLLAR
   ;

ques
   : QUES
   ;

register_
   : REGISTER
   ;

string
   : STRING1
   | STRING2
   ;

name
   : NAME
   ;

number
   : SIGN? NUMBER
   ;

opcode
   : OPCODE
   ;

rep
   : REP
   ;

ORG: 'ORG';
END: 'END';
ENDIF: 'ENDIF';
IF: 'IF';
EQU: 'EQU';
DW: 'DW';
DB: 'DB';
DM: 'DM';
DS: 'DS';
INCLUDE: 'INCLUDE';
INCLUDELIB: 'INCLUDELIB';
INVOKE: 'INVOKE';
OPTION: 'OPTION';
PUT: 'PUT';
NOT: 'NOT';
REGISTER
    : 'AH'
    | 'AL'
    | 'BH'
    | 'BL'
    | 'CH'
    | 'CL'
    | 'DH'
    | 'DL'
    | 'AX'
    | 'BX'
    | 'CX'
    | 'DX'
    | 'CI'
    | 'DI'
    | 'BP'
    | 'SP'
    | 'IP'
    | 'CS'
    | 'ES'
    | 'SS'
    ;

OPCODE
    : 'AAA'
    | 'AAD'
    | 'AAM'
    | 'AAS'
    | 'ADC'
    | 'ADD'
    | 'AND'
    | 'CALL'
    | 'CBW'
    | 'CLC'
    | 'CLD'
    | 'CLI'
    | 'CMC'
    | 'CMP'
    | 'CMPSB'
    | 'CMPSW'
    | 'CWD'
    | 'DAA'
    | 'DAS'
    | 'DEC'
    | 'DIV'
    | 'ESC'
    | 'HLT'
    | 'IDIV'
    | 'IMUL'
    | 'IN'
    | 'INC'
    | 'INT'
    | 'INTO'
    | 'IRET'
    | 'JA'
    | 'JAE'
    | 'JB'
    | 'JBE'
    | 'JC'
    | 'JE'
    | 'JG'
    | 'JGE'
    | 'JL'
    | 'JLE'
    | 'JNA'
    | 'JNAE'
    | 'JNB'
    | 'JNBE'
    | 'JNC'
    | 'JNE'
    | 'JNG'
    | 'JNGE'
    | 'JNL'
    | 'JNLE'
    | 'JNO'
    | 'JNP'
    | 'JNS'
    | 'JNZ'
    | 'JO'
    | 'JP'
    | 'JPE'
    | 'JPO'
    | 'JS'
    | 'JZ'
    | 'JCXZ'
    | 'JMP'
    | 'JMPS'
    | 'JMPF'
    | 'LAHF'
    | 'LDS'
    | 'LEA'
    | 'LES'
    | 'LOCK'
    | 'LODS'
    | 'LODSB'
    | 'LODSW'
    | 'LOOP'
    | 'LOOPE'
    | 'LOOPNE'
    | 'LOOPNZ'
    | 'LOOPZ'
    | 'MOV'
    | 'MOVS'
    | 'MOVSB'
    | 'MOVSW'
    | 'MUL'
    | 'NEG'
    | 'NOP'
    | 'OR'
    | 'OUT'
    | 'POP'
    | 'POPF'
    | 'PUSH'
    | 'PUSHF'
    | 'RCL'
    | 'RCR'
    | 'RET'
    | 'RETN'
    | 'RETF'
    | 'ROL'
    | 'ROR'
    | 'SAHF'
    | 'SAL'
    | 'SAR'
    | 'SALC'
    | 'SBB'
    | 'SCASB'
    | 'SCASW'
    | 'SHL'
    | 'SHR'
    | 'STC'
    | 'STD'
    | 'STI'
    | 'STOSB'
    | 'STOSW'
    | 'SUB'
    | 'TEST'
    | 'WAIT'
    | 'XCHG'
    | 'XLAT'
    | 'XOR'
    ;

REP : 'REP'
    | 'REPE'
    | 'REPNE'
    | 'REPNZ'
    | 'REPZ'
    ;

OFFSET: 'OFFSET';
SEGMENT: 'SEGMENT';
SEGMENTEND: 'ENDS';
GROUP: 'GROUP';
BYTE: 'BYTE';
SBYTE: 'SBYTE';
WORD: 'WORD';
DWORD: 'DWORD';
PARA: 'PARA';
PAGE: 'PAGE';
ALIGN: 'ALIGN';
LABEL: 'LABEL';
DUP: 'DUP';
ASSUME: 'ASSUME';
EXTERN: 'EXTERN';
PUBLIC: 'PUBLIC';

ASSIGN: '=';
DOLLAR: '$';
QUES: '?';


SIGN
   : '+' | '-'
   ;


MASMDIRECTIVE
   : '.' [A-Z0-9] +
   ;


NAME
   : [_A-Z] [A-Z0-9._@]*
   ;


NUMBER
   : [0-9A-F] + 'H'?
   ;


STRING1
   : '"' ~'"'* '"'
   ;


STRING2
   : '\u0027' ~'\u0027'* '\u0027'
   ;


COMMENT
   : ';' ~ [\r\n]* -> skip
   ;

EOL
   : [\r\n] +
   ;


WS
   : [ \t] -> skip
   ;
