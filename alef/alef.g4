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

// http://doc.cat-v.org/plan_9/2nd_edition/papers/alef/ref

grammar alef;

program
   : decllist? EOF
   ;

decllist
   : decl+
   ;

decl
   : tname vardecllist? ';'
   | tname vardecl '(' arglist? ')' block
   | tname adtfunc '(' arglist? ')' block
   | tname vardecl '(' arglist? ')' ';'
   | typespec ';'
   | TYPEDEF ztname vardecl zargs? ';'
   | TYPEDEF IDENTIFIER ';'
   ;

zargs
   : '(' arglist? ')'
   ;

ztname
   : tname
   | AGGR
   | ADT
   | UNION
   ;

adtfunc
   : typename '.' name
   | indsp typename '.' name
   ;

typespec
   : AGGR ztag? '{' memberlist '}' ztag?
   | UNION ztag? '{' memberlist '}' ztag?
   | ADT ztag? zpolytype? '{' memberlist '}' ztag?
   | ENUM ztag? '{' setlist '}'
   ;

ztag
   : name
   | typename
   ;

zpolytype
   : '[' polytype ']'
   ;

polytype
   : name
   | name ',' polytype
   ;

setlist
   : sname?
   | setlist ',' setlist
   ;

sname
   : name ('=' expr_)?
   ;

name
   : IDENTIFIER
   ;

memberlist
   : decl
   | memberlist decl
   ;

vardecllist
   : ivardecl (',' ivardecl)*
   ;

ivardecl
   : vardecl zinit?
   ;

zinit
   : '=' zelist
   ;

zelist
   : zexpr?
   | '[' expr_ ']' expr_
   | '.' stag expr_
   | '{' zelist '}'
   | '[' expr_ ']' '{' zelist '}'
   | zelist ',' zelist
   ;

vardecl
   : IDENTIFIER arrayspec?
   | indsp IDENTIFIER arrayspec?
   | '(' indsp IDENTIFIER arrayspec? ')' '(' arglist? ')'
   | indsp '(' indsp IDENTIFIER arrayspec? ')' '(' arglist? ')'
   ;

arrayspec
   : ('[' zexpr? ']')+
   ;

indsp
   : '*'+
   ;

arglist
   : arglistp* ',' arg
   ;

arglistp
   : arg
   | '*' xtname
   | '.' xtname
   ;

arg
   : xtname
   | xtname indsp arrayspec?
   | xtname '(' indsp ')' '(' arglist? ')'
   | xtname indsp '(' indsp ')' '(' arglist? ')'
   | TUPLE tuplearg
   | xtname vardecl
   | '.' '.' '.'
   ;

tuplearg
   : tname
   | tname '(' indsp ')' '(' arglist? ')'
   | tname vardecl
   ;

autolist
   : autodecl+
   ;

autodecl
   : xtname vardecllist? ';'
   | TUPLE tname vardecllist? ';'
   ;

block
   : '{' autolist? slist? '}'
   | '!' '{' autolist? slist? '}'
   ;

slist
   : stmnt+
   ;

tbody
   : '{' ctlist? '}'
   | '!' '{' clist? '}'
   ;

ctlist
   : tcase+
   ;

tcase
   : CASE typecast ':' slist?
   | DEFAULT ':' slist?
   ;

cbody
   : '{' clist? '}'
   | '!' '{' clist? '}'
   ;

clist
   : case_+
   ;

case_
   : CASE expr_ ':' slist?
   | DEFAULT ':' slist?
   ;

rbody
   : stmnt
   | IDENTIFIER block
   ;

zlab
   : IDENTIFIER
   ;

stmnt
   : nlstmnt
   | IDENTIFIER ':' stmnt
   ;

info
   : ',' STRING_CONST
   ;

nlstmnt
   : zexpr? ';'
   | block
   | CHECK expr_ info? ';'
   | ALLOC elist ';'
   | UNALLOC elist ';'
   | RESCUE rbody
   | RAISE zlab? ';'
   | GOTO IDENTIFIER ';'
   | PROC elist ';'
   | TASK elist ';'
   | BECOME expr_ ';'
   | ALT cbody
   | RETURN zexpr? ';'
   | FOR '(' zexpr? ';' zexpr? ';' zexpr? ')' stmnt
   | WHILE '(' expr_ ')' stmnt
   | DO stmnt WHILE '(' expr_ ')'
   | IF '(' expr_ ')' stmnt
   | IF '(' expr_ ')' stmnt ELSE stmnt
   | PAR block
   | SWITCH expr_ cbody
   | TYPEOF expr_ tbody
   | CONTINUE zconst? ';'
   | BREAK zconst? ';'
   ;

zconst
   : CONSTANT
   ;

zexpr
   : expr_
   ;

expr_
   : castexpr
   | expr_ '*' expr_
   | expr_ '/' expr_
   | expr_ '%' expr_
   | expr_ '+' expr_
   | expr_ '-' expr_
   | expr_ '>>' expr_
   | expr_ '<<' expr_
   | expr_ '<' expr_
   | expr_ '>' expr_
   | expr_ '<=' expr_
   | expr_ '>=' expr_
   | expr_ '==' expr_
   | expr_ '!=' expr_
   | expr_ '&' expr_
   | expr_ '^' expr_
   | expr_ '|' expr_
   | expr_ '&&' expr_
   | expr_ '||' expr_
   | expr_ '=' expr_
   | expr_ ':=' expr_
   | expr_ '<-' '=' expr_
   | expr_ '+=' expr_
   | expr_ '-=' expr_
   | expr_ '*=' expr_
   | expr_ '/=' expr_
   | expr_ '%=' expr_
   | expr_ '>>=' expr_
   | expr_ '<<=' expr_
   | expr_ '&=' expr_
   | expr_ '|=' expr_
   | expr_ '^=' expr_
   | expr_ '::' expr_
   ;

castexpr
   : monexpr
   | '(' typecast ')' castexpr
   | '(' ALLOC typecast ')' castexpr
   ;

typecast
   : xtname
   | xtname indsp
   | xtname '(' indsp ')' '(' arglist? ')'
   | TUPLE tname
   ;

monexpr
   : term_
   | '*' castexpr
   | '&' castexpr
   | '+' castexpr
   | '-' castexpr
   | '--' castexpr
   | ZEROX castexpr
   | '++' castexpr
   | '!' castexpr
   | '~' castexpr
   | SIZEOF monexpr
   | '<-' castexpr
   | '?' castexpr
   ;

ztelist
   : telist
   ;

telist
   : tcomp (',' tcomp)*
   ;

tcomp
   : expr_
   | '{' ztelist? '}'
   ;

term_
   : '(' telist ')'
   | SIZEOF '(' typecast ')'
   | term_ '(' zarlist? ')'
   | term_ '[' expr_ ']'
   | term_ '.' stag
   | '.' typename '.' stag
   | term_ '->' stag
   | term_ '--'
   | term_ '++'
   | term_ '?'
   | name
   | '.' '.' '.'
   | ARITHMETIC_CONST
   | NIL
   | CONSTANT
   | enum_member
   | STRING_CONST
   | '$' STRING_CONST
   ;

stag
   : IDENTIFIER
   | typename
   ;

zarlist
   : elist
   ;

elist
   : expr_
   | elist ',' expr_
   ;

tlist
   : typecast
   | typecast ',' tlist
   ;

tname
   : sclass? xtname
   | sclass? TUPLE '(' tlist ')'
   | sclass? '(' tlist ')'
   ;

variant
   : typecast
   | typecast ',' variant
   ;

xtname
   : INT
   | UINT
   | SINT
   | USINT
   | BYTE
   | FLOAT
   | VOID
   | typename
   | typename '[' variant ']'
   | CHAN '(' variant ')' bufdim?
   ;

bufdim
   : '[' expr_ ']'
   ;

sclass
   : EXTERN
   | INTERN
   | PRIVATE
   ;

typename
   : IDENTIFIER
   ;

enum_member
   : IDENTIFIER
   ;

ADT
   : 'adt'
   ;

AGGR
   : 'aggr'
   ;

ALLOC
   : 'alloc'
   ;

ALT
   : 'alt'
   ;

BECOME
   : 'become'
   ;

BREAK
   : 'break'
   ;

BYTE
   : 'byte'
   ;

CASE
   : 'case'
   ;

CHAN
   : 'chan'
   ;

CHECK
   : 'check'
   ;

CONTINUE
   : 'continue'
   ;

DEFAULT
   : 'default'
   ;

DO
   : 'do'
   ;

ELSE
   : 'else'
   ;

ENUM
   : 'enum'
   ;

EXTERN
   : 'extern'
   ;

FLOAT
   : 'float'
   ;

FOR
   : 'for'
   ;

GOTO
   : 'goto'
   ;

IF
   : 'if'
   ;

INT
   : 'int'
   ;

INTERN
   : 'intern'
   ;

LINT
   : 'lint'
   ;

NIL
   : 'nil'
   ;

PAR
   : 'par'
   ;

PROC
   : 'proc'
   ;

RAISE
   : 'raise'
   ;

RESCUE
   : 'rescue'
   ;

RETURN
   : 'return'
   ;

SINT
   : 'sint'
   ;

SIZEOF
   : 'sizeof'
   ;

SWITCH
   : 'switch'
   ;

TASK
   : 'task'
   ;

TUPLE
   : 'tuple'
   ;

TYPEDEF
   : 'typedef'
   ;

TYPEOF
   : 'typeof'
   ;

UINT
   : 'uint'
   ;

ULINT
   : 'ulint'
   ;

UNALLOC
   : 'unalloc'
   ;

UNION
   : 'union'
   ;

USINT
   : 'usint'
   ;

VOID
   : 'void'
   ;

WHILE
   : 'while'
   ;

ZEROX
   : 'zerox'
   ;

PRIVATE
   : 'private'
   ;

IDENTIFIER
   : [a-zA-Z_] [a-zA-Z_0-9]*
   ;

STRING_CONST
   : '"' ~ '"'* '"'
   ;

CONSTANT
   : '\'' ~ '\''* '\''
   ;

ARITHMETIC_CONST
   : DIGIT+ ('.' DIGIT*)? ('e' DIGIT+)?
   ;

fragment DIGIT
   : [0-9]
   ;

WS
   : [ \r\n\t]+ -> skip
   ;

