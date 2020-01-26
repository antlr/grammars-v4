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
grammar tlaplus;

modules
   : module+
   ;

module
   : moduleheader modulebody moduleend
   ;

moduleheader
   : '-'+ 'MODULE' identifier '-'+
   ;

modulebody
   : extends_? algo invariants?
   ;

extends_
   : 'EXTENDS' identifier (',' identifier)*
   ;

algo
   : '(*' '--algorithm' identifier vars? constants? (processes | block) 'end' 'algorithm' '*)'
   ;

processes
   : process+
   ;

process
   : 'process' vardecl vars? constants? block 'end' 'process'
   ;

constants
   : 'define' constant+ 'end' 'define' ';'
   ;

constant
   : identifier '==' 'DOMAN' expression
   ;

vars
   : (variable | variables)
   ;

variable
   : 'variable' vardecl ';'
   ;

variables
   : 'variables' vardecl (',' vardecl)+ ';'
   ;

vardecl
   : expressiondecl
   | setdecl
   | tupledecl
   ;

expressiondecl
   : identifier '=' expression
   ;

setdecl
   : identifier IN atom '..' atom
   ;

set
   : ('{' expression (',' expression)* '}')
   | ('{' '}')
   ;

tupledecl
   : identifier '=' tuple
   ;

tuple
   : '<<' ((atom | tuple) (',' (atom | tuple))*)? ','? '>>'
   ;

block
   : 'begin' statement*
   ;

statement
   : label (assignstmt | assertstmt | ifstmt)+
   ;

label
   : identifier ':'
   ;

assignstmt
   : lhs ':=' expression ';'
   ;

assertstmt
   : 'assert' expression ';'
   ;

ifstmt
   : 'if' expression 'then' statement* 'end' 'if' ';'
   ;

invariants
   : invariant+
   ;

invariant
   : identifier '==' (expression | equation)
   ;

lhs
   : identifier
   ;

equation
   : expression '=' expression
   ;

expression
   : (atom binop expression)
   | (atom compareop expression)
   | (atom)
   | ('(' expression ')')
   ;

binop
   : '+'
   | '-'
   | '*'
   | '/'
   | '/='
   | '*='
   | '+='
   | '-='
   | AND
   | OR
   ;

compareop
   : '<'
   | '>'
   | '>='
   | '<='
   ;

moduleend
   : '===='
   ;

atom
   : number
   | identifier
   | (identifier '[' expression ']')
   | TRUE
   | FALSE
   ;

number
   : DIGIT+
   ;

identifier
   : ID
   ;

IN
   : '\\in'
   ;

EXISTS
   : '\\E'
   ;

ALL
   : '\\A'
   ;

COMBINE
   : '\\o'
   ;

CARTESIANPRODUCT
   : '\\X'
   ;

KEYVAL
   : '|->'
   ;

MULTIASSIGN
   : '||'
   ;

IMPLIES
   : '=>'
   ;

EITHER
   : '<=>'
   ;

CHOOSE
   : 'CHOOSE'
   ;

DOMAIN
   : 'DOMAIN'
   ;

TRUE
   : 'TRUE'
   ;

FALSE
   : 'FALSE'
   ;

AND
   : '/\\'
   ;

OR
   : '\\/'
   ;

COMMENT
   : '\\*' ~ [\r\n]* -> skip
   ;

DIGIT
   : '0' .. '9'
   ;

ID
   : ('a' .. 'z' | 'A' .. 'Z') ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_')*
   ;

WS
   : [ \t\r\n] -> skip
   ;

