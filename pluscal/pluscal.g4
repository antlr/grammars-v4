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
   : ('--algorithm' |'--fair algorithm') name '{' varDecls? definitions? macro* procedure* (compoundStmt | process+) '}'
   ;

definitions
  : 'define' '{' ~'}'* '}' ';'?
 ;

macro
   : 'macro' name '(' (pVarDecl (',' pVarDecl)*)? ')' compoundStmt ';'?
   ;

procedure
   : 'procedure' name '(' (pVarDecls (',' pVarDecls)*)? ')' pVarDecls? compoundStmt ';'?
   ;

process
   : 'process' '(' name (EQ | IN) expr ')' varDecls? compoundStmt ';'?
   ;

varDecls
   : ('variable' | 'variables') varDecl+
   ;

varDecl: variable ((EQ | IN) expr)? (';' | ',');

pVarDecls
   : ('variable' | 'variables') (pVarDecl (';' | ','))+
   ;

pVarDecl: variable (EQ expr)? ';';







compoundStmt
   : '{' stmt (';' stmt)* ';'? '}'
   ;

stmt
   : (label ':' ('+' | '-')?)? (unlabeledStmt | compoundStmt)
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
   | callstmt
   ;

assignstmt
   : lhs ASSIGN expr ('||' lhs ASSIGN expr)*
   ;

lhs
   : name ('[' expr (',' expr)* ']' | '.' name)*
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
   : 'with' '(' variable (EQ | IN) expr ((';' | ',') variable (EQ | IN) expr)* (';' | ',')? ')' stmt
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
   : 'goto' label
   ;

callstmt
   : 'call'?  macrocall
   ;

macrocall: name '(' (expr (',' expr)*)? ')';

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
   | name
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

label: ID;
variable: ID;
name: ID;

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

