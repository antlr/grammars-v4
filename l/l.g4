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
grammar l;

start
   : series
   | statement
   ;

series
   : statement ';' statement
   | series ';' statement
   ;

statement
   : tuple
   | expression
   ;

tuple
   : expression ',' expression
   | tuple ',' expression
   ;

expression
   : apply
   | primary
   ;

apply
   : adjacentApply
   | dotApply
   | spacedApply
   ;

adjacentApply
   : primary primary
   | adjacentApply primary
   ;

dotApply
   : secondary '.' secondary
   | dotApply '.' secondary
   ;

spacedApply
   : tertiary SPACE tertiary
   | spacedApply SPACE tertiary
   ;

primary
   : subExpression
   | record
   | block
   | name
   | literal
   ;

secondary
   : adjacentApply
   | primary
   ;

tertiary
   : dotApply
   | secondary
   ;

subExpression
   : '(' start ')'
   ;

record
   : '{' start '}'
   ;

block
   : '[' start ']'
   ;

name
   : LETTERS
   | SYMBOLS
   ;

literal
   : integer
   | string
   | symbol
   ;

integer
   : DIGITS
   ;

string
   : '`' CHARACTERS '`'
   ;

symbol
   : '\'' name
   ;

comment
   : '"' CHARACTERS '"'
   ;

LETTERS
   : [a-zA-Z]+
   ;

DIGITS
   : [0-9]+
   ;

SYMBOLS
   : [!@#$%^&*()]
   ;

SPACE
   : [ \t\r\n]
   ;

