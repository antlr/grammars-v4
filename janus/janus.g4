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
   : (IDENT ('[' NUM ']')?)* ('PROCEDURE' IDENT statements)* EOF
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
   : 'IF' expression ('THEN' statements)? ('ELSE' statements)? 'FI' expression
   ;

dostmt
   : 'FROM' expression ('DO' statements)? ('LOOP' statements)? 'UNTIL' expression
   ;

callstmt
   : 'CALL' IDENT
   | 'UNCALL' IDENT
   ;

readstmt
   : 'READ' IDENT
   ;

writestmt
   : 'WRITE' IDENT
   ;

lvalstmt
   : lvalue modstmt
   | lvalue swapstmt
   ;

modstmt
   : '+=' expression
   | '-=' expression
   | '!=' expression
   | '<=>' expression
   ;

swapstmt
   : ':' lvalue
   ;

expression
   : minexp
   | minexp BINOP expression
   ;

minexp
   : '(' expression ')'
   | '-' expression
   | '~' expression
   | lvalue
   | constant
   ;

lvalue
   : IDENT
   | IDENT '[' expression ']'
   ;

constant
   : NUM
   ;

BINOP
   : '+'
   | '-'
   | '!'
   | '<'
   | '>'
   | '&'
   | '|'
   | '='
   | '#'
   | '<='
   | '>='
   | '*'
   | '/'
   | '\\'
   ;

IDENT
   : [a-zA-Z] [a-zA-Z0-9]*
   ;

NUM
   : [0-9]
   ;

COMMENT
   : ';' ~ [\r\n]* -> skip
   ;

WS
   : [ \t\r\n]+ -> skip
   ;

