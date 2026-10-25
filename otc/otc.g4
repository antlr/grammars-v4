/*
BSD License

Copyright (c) 2026, Tom Everett
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
grammar otc;

prog
   : decl* EOF
   ;

decl
   : vardecl
   | fundecl
   ;

vardecl
   : (type NAME ';')
   | (type NAME '[' INT ']' ';')
   ;

fundecl
   : type NAME '(' args ')' '{' body '}'
   ;

args
   : ((arg ',')* arg)?
   ;

arg
   : type NAME
   ;

body
   : vardecl* stmt*
   ;

stmt
   : ifstmt
   | whilestmt
   | dowhilestmt
   | ('return' expr ';')
   | (expr ';')
   | ('{' stmt* '}')
   | ';'
   ;

ifstmt
   : ('if' '(' expr ')' stmt)
   | ('if' '(' expr ')' stmt 'else' stmt)
   ;

whilestmt
   : 'while' '(' expr ')' stmt
   ;

dowhilestmt
   : 'do' stmt 'while' '(' expr ')' ';'
   ;

expr
   : unop expr
   | '(' expr ')'
   | NAME
   | INT
   | CHAR
   | STRING
   | expr '(' exprList ')'
   | expr '[' expr ']'
   | expr binop expr
   ;

exprList
   : ((expr ',')* expr)?
   ;

binop
   : '+'
   | '-'
   | '*'
   | '/'
   | '%'
   | '='
   | '<'
   | '=='
   | '!='
   ;

unop
   : '!'
   | '-'
   | '*'
   ;

type
   : ('int' stars)
   | ('char' stars)
   ;

stars
   : '*'*
   ;

CHAR
   : '\'' (ESC | ~ ['\\\r\n]) '\''
   ;

fragment ESC
   : '\\' .
   ;

STRING
   : '"' (ESC | ~ ["\\\r\n])* '"'
   ;

INT
   : [0-9]+
   ;

NAME
   : [a-zA-Z_] [a-zA-Z_0-9]*
   ;

LINE_COMMENT
   : '//' ~ [\r\n]* -> skip
   ;

BLOCK_COMMENT
   : '/*' .*? '*/' -> skip
   ;

WS
   : [ \t\r\n] -> skip
   ;

