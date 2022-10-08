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
   : (O R G) | (E N D) | (E Q U) | (D E F B) | (D E F W) | (D S) | (I F) | (E N D I F) | (S E T)
   ;

OPCODE
   : (A D C) | (A D D) | (A N D) | (B I T) | (C A L L) | (C C F) | (C P) | (C P D) | (C P D R) | (C P I) | (C P I R) | (C P L) | (D A A) | (D E C) | (D I) | (D J N Z) | (E I) | (E X) | (E X X) | (I M) | (I N) | (I N C) | (I N D) | (I N D R) | (I N I) | (I N I R) | (J P) | (J R) | (L D) | (L D D) | (L D D R) | (L D I) | (L D I R) | (N E G) | (N O P) | (O R) | (O T D R) | (O T I R) | (O U T) | (O U T D) | (O U T I) | (P O P) | (P U S H) | (R E S) | (R E T) | (R E T I) | (R E T N) | (R L) | (R L A) | (R L C) | (R L C A) | (R L D) | (R R) | (R R A) | (R R C) | (R R C A) | (R R D) | (R S T) | (S B C) | (S C F) | (S E T) | (S L A) | (S L L) | (S L '1') | (S R A) | (S R L) | (S U B) | (X O R)
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

NAME
   : [a-zA-Z] [a-zA-Z0-9."]*
   ;

NUMBER
   : '$'? [0-9a-fA-F] + ('H' | 'h')?
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
