/*
BSD License

Copyright (c) 2020, Tom Everett
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
grammar metamath;

database
   : outermostscopestmt* EOF
   ;

outermostscopestmt
   : includestmt
   | constantstmt
   | stmt
   ;

includestmt
   : '$[' filename '$]'
   ;

constantstmt
   : '$c' constant+ '$.'
   ;

stmt
   : block
   | variablestmt
   | disjointstmt
   | hypothesisstmt
   | assertstmt
   ;

block
   : '${' stmt* '$}'
   ;

variablestmt
   : '$v' variable+ '$.'
   ;

disjointstmt
   : '$d' variable variable variable* '$.'
   ;

hypothesisstmt
   : floatingstmt
   | essentialstmt
   ;

floatingstmt
   : LABEL '$f' typecode variable '$.'
   ;

essentialstmt
   : LABEL '$e' typecode mathsymbol* '$.'
   ;

assertstmt
   : axiomstmt
   | provablestmt
   ;

axiomstmt
   : LABEL '$a' typecode mathsymbol* '$.'
   ;

provablestmt
   : LABEL '$p' typecode mathsymbol* '$=' proof '$.'
   ;

proof
   : uncompressedproof
   | compressedproof
   ;

uncompressedproof
   : (LABEL | '?')+
   ;

compressedproof
   : '(' LABEL* ')' COMPRESSEDPROOFBLOCK+
   ;

typecode
   : constant
   ;

mathsymbol
   : (printablecharacter | LPAREN | RPAREN)+
   ;

printablecharacter
   : PRINTABLECHARACTER | LABEL
   ;

filename
   : mathsymbol
   ;

constant
   : mathsymbol
   ;

variable
   : mathsymbol
   ;

LPAREN
   : '('
   ;

RPAREN
   : ')'
   ;

LABEL
   : (LETTERORDIGIT | '.' | '-' | '_')+
   ;

PRINTABLECHARACTER
   : [\u0021-\u007e]+
   ;

fragment LETTERORDIGIT
   : [A-Za-z0-9]
   ;

COMPRESSEDPROOFBLOCK
   : ([A-Z] | '?')+
   ;

BLOCK_COMMENT
   : '$(' .*? '$)' -> skip
   ;

WS
   : [ \r\n\t\f]+ -> skip
   ;

