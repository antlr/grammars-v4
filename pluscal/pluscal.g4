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
grammar pluscal;

algorithm
   : '--' 'algorithm' ID '{' varDecls? /*definitions?*/ macro* procedure* (compoundStmt | process+) '}'
   ;

//definitions
//  : 'define' '{' defs '}' ';'?
// ;

macro
   : 'macro' ID '(' (ID (',' ID)*)? ')' compoundStmt ';'?
   ;

procedure
   : 'procedure' ID '(' (pVarDecls (',' pVarDecls)*)? ')' pVarDecls? compoundStmt ';'?
   ;

process
   : 'process' '(' ID (EQ | IN) expr ')' varDecls? compoundStmt ';'?
   ;

pVarDecls
   : ('variable' | 'variables') (ID (EQ expr)? (';' | ',')?)+
   ;

varDecls
   : ('variable' | 'variables') (ID ((EQ | IN) expr)? (';' | ','))+
   ;

compoundStmt
   : '{' stmt (';' stmt)* ';'? '}'
   ;

stmt
   : (ID ':')? (unlabeledStmt | compoundStmt)
   ;

unlabeledStmt
   : assignstmt
   | ifstmt
   | whilestmt
   | eitherstmt
   | withstmt
   | awaitstmt
   | printstmt
   | assertstmt
   | 'skip'
   | 'return'
   | gotostmt
   | 'call'? callstmt
   ;

assignstmt
   : lhs ASSIGN expr ('||' lhs ASSIGN expr)*
   ;

lhs
   : ID ('[' expr (',' expr)* ']' | '.' ID)*
   ;

ifstmt
   : 'if' '(' expr ')' stmt ('else' stmt)?
   ;

whilestmt
   : 'while' '(' expr ')' stmt
   ;

eitherstmt
   : 'either' stmt ('or' stmt)+
   ;

withstmt
   : 'with' '(' ID (EQ | IN) expr ((';' | ',') ID (EQ | IN) expr)* (';' | ',')? ')' stmt
   ;

awaitstmt
   : ('await' | 'when') expr
   ;

printstmt
   : 'print' expr
   ;

assertstmt
   : 'assert' expr
   ;

gotostmt
   : 'goto' ID
   ;

callstmt
   : ID '(' (expr (',' expr)*)? ')'
   ;

expr
   : atom ((AND | OR) atom)*
   | list
   | compare
   ;

compare
   : atom '=' atom
   ;

atom
   : bool
   | numb
   | ID
   ;


list
   : '{' atom (',' atom)* '}'
   ;

numb
   : DIGIT+
   ;

bool
   : 'TRUE' | 'FALSE'
   ;

AND
   : '/\\'
   ;

OR
   : '\\/'
   ;

ASSIGN
   : ':='
   ;
EQ
   : '='
   ;

IN
   : '\\in'
   ;

//defs
 //  : 'A sequence of TLA+ definitions'
 //  ;

DIGIT
   : '0' .. '9'
   ;

ID
   : ('a' .. 'z' | 'A' .. 'Z') ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9')*
   ;

COMMENT
   : ';' ~ [\r\n]*
   ;

WS
   : [ \t\r\n] -> skip
   ;

