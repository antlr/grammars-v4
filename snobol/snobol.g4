/*
[The "BSD licence"]
Copyright (c) 2012 Tom Everett
All rights reserved.
Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:
1. Redistributions of source code must retain the above copyright
notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
notice, this list of conditions and the following disclaimer in the
documentation and/or other materials provided with the distribution.
3. The name of the author may not be used to endorse or promote products
derived from this software without specific prior written permission.
THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

grammar snobol;

prog
   : lin + EOF
   ;

lin
   : line? EOL
   ;

line
   : (label? subject pattern? (EQ expression +)? (COLON transfer)?)
   | (COLON transfer)
   | (COMMENT | END)
   ;

label
   : STRING
   ;

subject
   : (AMP? STRING ('[' STRING (',' STRING)* ']')?)
   ;

pattern
   : STRINGLITERAL1
   | STRINGLITERAL2
   ;

expression
   : multiplyingExpression ((PLUS | MINUS) multiplyingExpression)*
   ;

multiplyingExpression
   : powExpression ((TIMES | DIV) powExpression)*
   ;

powExpression
   : atom (POW expression)?
   ;

atom
   : (STRINGLITERAL1 | STRINGLITERAL2 | INTEGER)
   | subject
   | command
   | '[' expression (',' expression)* ']'
   | LPAREN expression RPAREN
   ;

command
   : ident
   | differ
   | eq
   | ne
   | ge
   | le
   | lt
   | integer
   | lgt
   | atan
   | chop
   | cos
   | exp
   | ln
   | remdr
   | sin
   | tan
   | date
   | dupl
   | reverse
   | replace
   | size
   | trim
   | array_
   | sort
   | table
   | break_
   ;

ident
   : 'ident' LPAREN expression RPAREN
   ;

differ
   : 'differ' LPAREN expression RPAREN
   ;

eq
   : 'eq' LPAREN expression RPAREN
   ;

ne
   : 'ne' LPAREN expression RPAREN
   ;

ge
   : 'ge' LPAREN expression RPAREN
   ;

gt
   : 'gt' LPAREN expression RPAREN
   ;

le
   : 'le' LPAREN expression RPAREN
   ;

lt
   : 'lt' LPAREN expression RPAREN
   ;

integer
   : 'integer' LPAREN expression RPAREN
   ;

lgt
   : 'lgt' LPAREN expression RPAREN
   ;

atan
   : 'atan' LPAREN expression RPAREN
   ;

chop
   : 'chop' LPAREN expression RPAREN
   ;

cos
   : 'cos' LPAREN expression RPAREN
   ;

exp
   : 'exp' LPAREN expression RPAREN
   ;

ln
   : 'ln' LPAREN expression RPAREN
   ;

remdr
   : 'remdr' LPAREN expression RPAREN
   ;

sin
   : 'sin' LPAREN expression RPAREN
   ;

tan
   : 'tan' LPAREN expression RPAREN
   ;

dupl
   : 'dupl' LPAREN expression COMMA expression RPAREN
   ;

reverse
   : 'reverse' LPAREN expression RPAREN
   ;

date
   : 'date' LPAREN RPAREN
   ;

replace
   : 'replace' LPAREN expression COMMA expression COMMA expression RPAREN
   ;

size
   : 'size' LPAREN expression RPAREN
   ;

trim
   : 'trim' LPAREN expression RPAREN
   ;

array_
   : 'array' LPAREN expression COMMA expression RPAREN
   ;

convert
   : 'convert' LPAREN expression COMMA expression RPAREN
   ;

table
   : 'table' LPAREN expression RPAREN
   ;

sort
   : 'sort' LPAREN expression RPAREN
   ;

break_
   : 'break' LPAREN expression RPAREN
   ;

transfer
   : (transferpre? LPAREN (label | END) RPAREN)?
   ;

transferpre
   : ('f' | 'F' | 's' | 'S')
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


AMP
   : '&'
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


POW
   : '^'
   ;


EQ
   : '='
   ;


COLON
   : ':'
   ;


END
   : 'END'
   ;


STRINGLITERAL1
   : '"' ~ ["\r\n]* '"'
   ;


STRINGLITERAL2
   : '\'' ~['\r\n]* '\''
   ;


STRING
   : ('a' .. 'z' | 'A' .. 'Z') ('0' .. '9' | 'a' .. 'z' | 'A' .. 'Z')*
   ;


INTEGER
   : ('+' | '-')? ('0' .. '9') +
   ;


REAL
   : ('+' | '-')? ('0' .. '9') + ('.' ('0' .. '9') +)? (('e' | 'E') REAL)*
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


COMMENT
   : '*' ~ [\r\n]*
   ;


EOL
   : [\r\n] +
   ;


WS
   : (' ' | '\t') + -> skip
   ;
