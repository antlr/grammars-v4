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
/*
    http://www.ibiblio.org/apollo/
*/
/*
    http://www.ibiblio.org/apollo/hrst/archive/1689.pdf
*/
/*
    http://www.ibiblio.org/apollo/assembly_language_manual.html
*/

grammar agc;

prog
    : (line? EOL)+
    ;

line
    : (assignment |cmd)? comment?
    ;

assignment
    : variable '=' expression
    ;

cmd
    : (label? opcode '*'? expression*) 
    | label
    ;

opcode
    : 'SETLOC'
    | 'TC'          
    | 'CCS'           
    | 'TCF'
    | 'DAS'
    | 'LXCH'
    | 'INCR'
    | 'ADS'
    | 'CA'
    | 'CS'
    | 'INDEX'
    | 'DXCH'
    | 'TS'
    | 'XCH'
    | 'AD'
    | 'MASK'
    | 'READ'
    | 'WRITE'
    | 'RAND'
    | 'WAND'
    | 'ROR'
    | 'WOR'
    | 'RXOR'
    | 'EDRUPT'
    | 'BZF'
    | 'MSU'
    | 'QXCH'
    | 'AUG'
    | 'DIM'
    | 'DCA'
    | 'DCS'
    | 'SU'
    | 'BZMF'
    | 'MP'
    | 'XXALQ'
    | 'XLQ'
    | 'RETURN'
    | 'RELINT'
    | 'INHINT'
    | 'EXTEND'
    | 'NOOP'
    | 'DDOUBL'
    | 'DTCF'
    | 'COM'
    | 'ZL'
    | 'RESUME'
    | 'DTCB'
    | 'OVSK'
    | 'TCAA'
    | 'DOUBLE'
    | 'ZQ'
    | 'DCOM'
    | 'SQUARE'
    | 'PINC'
    | 'PCDU'
    | 'MINC'
    | 'MCDU'
    | 'DINC'
    | 'SHINC'
    | 'INOTRD'
    | 'INOTLD'
    | 'FETCH'
    | 'STORE'
    | 'GOJ'
    | 'TCSAJ'
    | 'CAF'
    | 'OCT'
    | 'CADR'
    | 'BANK'
    | 'DMOVE'
    | 'VMOVE'
    | 'SMOVE'
    | 'DSU'
    | 'RTB'
    | 'ITC'
    | 'NOLOD'
    | 'EXIT'
    | 'BPL'
    | 'SIN'
    | 'COS'
    | 'CAD'
    | 'DEC'
    | 'TEST'
    | 'VXSC'
    | 'ITC'  
    | '2DEC'
    | 'EQUALS'
    | 'DAD'
    | 'VXV'
    | 'VAD'
    | 'DAD'
    | 'BPL'
    | 'DMP'
    | 'BOV'
    | 'VXV'
    | 'VAD'
    | 'UNIT'
    | 'OCTAL'
    | 'ADRES'
    | 'ABVAL'
    ;

label
    : STRING
    ;

comment
    : COMMENT
    ;

variable
    : STRING (COMMA number)?
    | STRING LPAREN variable RPAREN
    ;

expression 
    : multiplyingExpression ((PLUS|MINUS) multiplyingExpression)*
    ;

multiplyingExpression  
    : atom ((TIMES|DIV) atom)*
    ;

atom 
    : number
    | variable
    ;

number
    : FLOAT
    | DECIMAL
    ;

STRING
    : [a-zA-Z0-9] [a-zA-Z0-9_.+\-/*]*
    ; 

DECIMAL
    : [0-9]+ 'DEC'
    ;

FLOAT
    : ('+' | '-')? [0-9]+ ('.' [0-9]*)?
    | ('+' | '-')? '.' [0-9]+
    ;

COMMENT
    : '#' ~[\r\n]*
    ;

PLUS 
    : '+'
    ;

MINUS 
    : '-'
    ;

TIMES 
    : '*'
    ;

DIV 
    : '/'
    ;

COMMA
    : ','
    ;

LPAREN 
    : '('
    ;

RPAREN 
    : ')'
    ;

EOL
    : '\r'? '\n'
    ;

WS
    : [ \t\r\n]->skip
    ;