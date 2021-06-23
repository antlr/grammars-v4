/*
BSD License

Copyright (c) 2021, Tom Everett
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
grammar miranda;

script
   : decl*
   ;

rhs
   : simple_rhs ';'?
   | cases
   ;

simple_rhs
   : exp whdefs?
   ;

cases
   : alt ';'? '=' cases
   | lastcase ';'?
   ;

decl
   : def
   | tdef
   | spec
   | 'libdir'
   ;

alt
   : exp ',' 'if'? exp
   ;

def
   : fnform '=' rhs
   | pat '=' rhs
   ;

lastcase
   : lastalt whdefs?
   ;

lastalt
   : exp ',' 'if'? exp
   | exp ',' 'otherwise'
   ;

tdef
   : tform '==' type ';'?
   | tform '::' constructs ';'?
   | 'abstype' tform_list 'with' sig ';'?
   ;

whdefs
   : 'where' def def*
   ;

spec
   : var_list '::' type ';'?
   | tform_list '::' type ';'?
   ;

sig
   : spec spec*
   ;

exp
   : e1
   | PREFIX1
   | INFIX
   ;

exp_list
   : exp (',' exp)*
   ;

constructs
   : construct '|' constructs
   | construct
   ;

e1
   : simple simple*
   | PREFOX e1
   | e1 INFIX e1
   ;

construct
   : constructor argtype*
   | type '$' constructor type
   | (construct) argtype*
   ;

simple
   : var
   | constructor
   | literal
   | 'readvals'
   | 'show'
   | '(' INFIX1 e1 ')'
   | '(' e1 INFIX ')'
   | '(' exp_list? ')'
   | '[' exp_list? ']'
   | '[' exp+ ']'
   | '[' exp (',' exp)* ']'
   | '[' exp
   | qualifs ']'
   | '[' exp '//' qualifs ']'
   ;

type
   : argtype
   | typename argtype*
   | type '->' type
   | type '$' typename type
   ;

type_list
   : type (',' type)*
   ;

argtype
   : typename
   | TYPEVAR
   | '(' type_list? ')'
   | '[' type_list ']'
   ;

tform
   : typename TYPEVAR '*'
   | TYPEVAR '$' typename TYPEVAR
   ;

tform_list
   : tform (',' tform)*
   ;

qualifs
   : qualifier ';' qualifs
   | qualifier
   ;

fnform
   : var formal '*'
   | pat '$' var pat
   | '(' fnform ')' formal '*'
   ;

qualifier
   : exp
   | generator
   ;

generator
   : pat_list '<-' exp
   | pat '<-' exp (',' exp)*
   ;

var
   : IDENTIFIER
   ;

var_list
   : var (',' var)*
   ;

pat
   : formal
   | '-' numeral
   | constructor formal '*'
   | pat ':' pat
   | pat '+' NAT
   | pat '$' constructor pat
   | '(' pat ')' formal '*'
   ;

pat_list
   : pat (',' pat)*
   ;

constructor
   : IDENTIFIER
   ;

typename
   : IDENTIFIER
   ;

formal
   : var
   | constructor
   | literal1
   | '(' pat_list? ')'
   | '[' pat_list? ']'
   ;

numeral
   : NAT
   | FLOAT
   ;

literal
   : numeral
   | CHARCONST
   | STRINGCONST
   ;

literal1
   : literal '-' FLOAT
   ;

IDENTIFIER_LC
   : [a-z] [a-zA-Z0-9_']* '-' DELIMITER
   ;

DELIMITER
   : '-'
   | PREFIX1
   | INFIX1
   | OTHER
   ;

INFIX1
   : '++'
   | '--'
   | ':'
   | '\/'
   | '&'
   | '>'
   | '>='
   | '='
   | '~='
   | '<='
   | '<'
   | '+'
   | '*'
   | '/'
   | 'div'
   | 'mod'
   | '^'
   | '.'
   | '!'
   | '$' IDENTIFIER_LC
   | '$' IDENTIFIER
   ;

INFIX
   : INFIX1
   | '-'
   ;

PREFIX1
   : '~'
   | '#'
   ;

PREFIX
   : PREFIX1
   | '-'
   ;

fragment OTHER
   : 'abstype'
   | 'if'
   | 'otherwise'
   | 'readvals'
   | 'show'
   | 'type'
   | 'where'
   | 'with'
   | '%export'
   | '%free'
   | '%include'
   | '%insert'
   | '%list'
   | '%nolist'
   | '='
   | '=='
   | '::='
   | '::'
   | '=>'
   | VERTICAL_BAR
   | '//'
   | '->'
   | ';'
   | ','
   | '('
   | ')'
   | '['
   | ']'
   | '{'
   | '}'
   | '<-'
   | '..'
   | '$$'
   | '$-'
   | '$+'
   | '$*'
   ;

VERTICAL_BAR
   : '|'
   ;

FLOAT
   : [0-9]* [.] NAT EPART?
   | NAT EPART
   ;

TYPEVAR
   : '*' '*'*
   ;

fragment EPART
   : 'e'? ('+' | '-')? NAT
   ;

fragment CHARCONST
   : '\'' ~ '\''* '\''
   ;

fragment STRINGCONST
   : '"' ~ '"'* '"'
   ;

NAT
   : [0-9]+
   ;

IDENTIFIER
   : [A-Z] [a-zA-Z0-9_']*
   ;

WS
   : [ \r\n\t]+ -> skip
   ;

