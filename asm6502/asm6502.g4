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

prog
    : (line? EOL)+
    ;

line
    : comment 
    | instruction
    | assemblerinstruction
    | lbl
    ;
   
instruction
    : label? opcode argumentlist? comment?
    ;

assemblerinstruction
    : argument? assembleropcode argumentlist? comment?
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
    : prefix? (number | name | string | '*') (('+' | '-') number)?
    | '(' argument ')'
    ;

prefix
    : '#'
    ;

string
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
      
opcode
    : OPCODE
    ;

ASSEMBLER_INSTRUCTION
    : 'ORG'
    | 'EQU'
    | 'ASC'
    | 'DS'
    | 'DFC'
    | '='
    ;

OPCODE  
    : 'ADC'	
    | 'AND'	
    | 'ASL'	
    | 'BCC'	
    | 'BCS'	
    | 'BEQ'	
    | 'BIT'	
    | 'BMI'	
    | 'BNE'	
    | 'BPL'
    | 'BRA'
    | 'BRK'	
    | 'BVC'	
    | 'BVS'	
    | 'CLC'	
    | 'CLD'	
    | 'CLI'	
    | 'CLV'	
    | 'CMP'	
    | 'CPX'	
    | 'CPY'	
    | 'DEC'	
    | 'DEX'	
    | 'DEY'	
    | 'EOR'	
    | 'INC'	
    | 'INX'	
    | 'INY'	
    | 'JMP'	
    | 'JSR'	
    | 'LDA'	
    | 'LDY'	
    | 'LDX'	
    | 'LSR'	
    | 'NOP'	
    | 'ORA'	
    | 'PHA'
    | 'PHX'
    | 'PHY'
    | 'PHP'	
    | 'PLA'	
    | 'PLP'
    | 'PLY'
    | 'ROL'	
    | 'ROR'	
    | 'RTI'	
    | 'RTS'	
    | 'SBC'	
    | 'SEC'	
    | 'SED'	
    | 'SEI'	
    | 'STA'	
    | 'STX'	
    | 'STY'
    | 'STZ'
    | 'TAX'	
    | 'TAY'	
    | 'TSX'	
    | 'TXA'	
    | 'TXS'	
    | 'TYA'
    | 'adc'	
    | 'and'	
    | 'asl'	
    | 'bcc'	
    | 'bcs'	
    | 'beq'	
    | 'bit'	
    | 'bmi'	
    | 'bne'	
    | 'bpl'
    | 'bra'
    | 'brk'	
    | 'bvc'	
    | 'bvs'	
    | 'clc'	
    | 'cld'	
    | 'cli'	
    | 'clv'	
    | 'cmp'	
    | 'cpx'	
    | 'cpy'	
    | 'dec'	
    | 'dex'	
    | 'dey'	
    | 'eor'	
    | 'inc'	
    | 'inx'	
    | 'iny'	
    | 'jmp'	
    | 'jsr'	
    | 'lda'	
    | 'ldy'	
    | 'ldx'	
    | 'lsr'	
    | 'nop'	
    | 'ora'	
    | 'pha'
    | 'phx'
    | 'phy'
    | 'php'	
    | 'pla'	
    | 'plp'
    | 'ply'
    | 'rol'	
    | 'ror'	
    | 'rti'	
    | 'rts'	
    | 'sbc'	
    | 'sec'	
    | 'sed'	
    | 'sei'	
    | 'sta'	
    | 'stx'	
    | 'sty'
    | 'stz'
    | 'tax'	
    | 'tay'	
    | 'tsx'	
    | 'txa'	
    | 'txs'	
    | 'tya'
    ;
      
NAME
    : [a-zA-Z] [a-zA-Z0-9."]*
    ;

NUMBER
    : '$'? [0-9a-fA-F]+
    ;

COMMENT
    : ';' ~[\r\n]*
    ;

STRING
    : '"' ~["]* '"'
    ;

EOL
    : '\r'? '\n'
    ;

WS
    : [ \t]->skip
    ;
