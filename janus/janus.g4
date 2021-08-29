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
grammar janus;

program
   : (IDENT (LBRACE_K NUM LBRACE_K)?)* (PROCEDURE IDENT statements)*
   ;

statements
   : statement+
   ;

statement
   : ifstmt
   | dostmt
   | callstmt
   | readstmt
   | writestmt
   | lvalstmt
   ;

ifstmt
   : IF expression (THEN statements)? (ELSE statements)? FI expression
   ;

dostmt
   : FROM expression (DO statements)? (LOOP statements)? UNTIL expression
   ;

callstmt
   : CALL IDENT
   | UNCALL IDENT
   ;

readstmt
   : READ IDENT
   ;

writestmt
   : WRITE IDENT
   ;

lvalstmt
   : lvalue modstmt
   | lvalue swapstmt
   ;

modstmt
   : PLUS_EQUAL expression
   | MINUS_EQUAL expression
   | NOT_EQUAL expression
   | LEG expression
   ;

swapstmt
   : COLON lvalue
   ;

expression
   : minexp
   | minexp binop expression
   ;

minexp
   : LBRACE expression LBRACE
   | MINUS expression
   | TILDE expression
   | lvalue
   | constant
   ;

lvalue
   : IDENT
   | IDENT LBRACE_K expression RBRACE_K
   ;

constant
   : NUM
   ;

DO 
   : 'DO'
   ;

IF
   : 'IF'
   ;

UNTIL
   : 'UNTIL'
   ;

LOOP
   : 'LOOP'
   ;

THEN 
   : 'THEN'
   ;

ELSE
   : 'ELSE'
   ;

FI
   : 'FI'
   ;

READ
   : 'READ'
   ;

WRITE
   : 'WRITE'
   ;

CALL
   : 'CALL'
   ;

UNCALL
   : 'UNCALL'
   ;

FROM
   : 'FROM'
   ;

PROCEDURE
   : 'PROCEDURE'
   ;

binop
   : PLUS
   | MINUS
   | NOT
   | LOW
   | GREATE
   | '&'
   | '|'
   | EQUAL
   | '#'
   | LE
   | GE
   | '*'
   | '/'
   | '\\'
   ;

LBRACE
   : '('
   ;

RBRACE
   : ')'
   ;

LBRACE_K
   : '['
   ;

RBRACE_K
   : ']'
   ;

COLON
   : ':'
   ;

MINUS
   : '-'
   ;

PLUS
   : '+'
   ;

NOT
   : '!'
   ;

EQUAL
   : '='
   ;

PLUS_EQUAL
   : '+='
   ;

MINUS_EQUAL
   : '-='
   ;

GREATE
   : '>'
   ;

LOW
   : '<'
   ;


GE 
   : '>='
   ;

LE 
   : '<='
   ;

NOT_EQUAL
   : '!='
   ;

LEG
   : '<=>'
   ;

TILDE
   : '~'
   ;



NUM
   : [0-9]
   ;

IDENT
   : [a-zA-Z] [a-zA-Z0-9]*
   ;

COMMENT
   : ';' ~ [\r\n]* -> skip
   ;

WS
   : [ \t\r\n]+ -> skip
   ;

