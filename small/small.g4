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

// Grammar for the SMALL programming language, as specified at

grammar small;

prog
   : (statement | command)* EOF
   ;

command
   : EDIT NAME
   | EXIT
   ;

statement
   : structuredStatement
   | expressionStatement
   ;

expressionStatement
   : expression assignment? disposition?
   ;

assignment
   : ARROW variable
   ;

disposition
   : IGNORED
   | OUTPUT
   | RETURNED
   | TESTED
   ;

expression
   : prefix? simpleExpression (operator prefix? simpleExpression)*
   ;

operator
   : PLUS
   | MINUS
   | STAR
   | SLASH
   | PERCENT
   | LE
   | NE
   | GE
   | LT
   | GT
   | EQ
   | AMP
   | PIPE
   | COLON
   |
   ;

prefix
   : PLUS
   | MINUS
   | TILDE
   ;

simpleExpression
   : constant
   | variable
   | input
   ;

input
   : DOLLAR
   ;

variable
   : NAME
   ;

constant
   : STRING
   | FLOAT
   | INT
   ;

structuredStatement
   : iteration
   | selection
   ;

iteration
   : LBRACE alternatives RBRACE
   ;

selection
   : LBRACKET alternatives RBRACKET
   ;

alternatives
   : alternative (ORELSE alternative)*
   ;

alternative
   : (statement | command)*
   ;

EDIT
   : 'edit'
   ;

EXIT
   : 'exit'
   ;

ARROW
   : '->'
   ;

ORELSE
   : '||'
   ;

IGNORED
   : ';'
   ;

OUTPUT
   : '!'
   ;

RETURNED
   : '^'
   ;

TESTED
   : '?'
   ;

LE
   : '<='
   ;

NE
   : '<>'
   ;

GE
   : '>='
   ;

LT
   : '<'
   ;

GT
   : '>'
   ;

EQ
   : '='
   ;

PLUS
   : '+'
   ;

MINUS
   : '-'
   ;

STAR
   : '*'
   ;

SLASH
   : '/'
   ;

PERCENT
   : '%'
   ;

AMP
   : '&'
   ;

PIPE
   : '|'
   ;

COLON
   : ':'
   ;

TILDE
   : '~'
   ;

DOLLAR
   : '$'
   ;

LBRACE
   : '{'
   ;

RBRACE
   : '}'
   ;

LBRACKET
   : '['
   ;

RBRACKET
   : ']'
   ;

NAME
   : [a-zA-Z]+
   ;

FLOAT
   : [0-9]+ '.' [0-9]+
   ;

INT
   : [0-9]+
   ;

STRING
   : '"' ~ ["\r\n]* '"'
   ;

WS
   : [ \t\r\n]+ -> skip
   ;

COMMENT
   : '//' ~ [\r\n]* -> skip
   ;

