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
'AS IS' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
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
grammar fantom;

file_
   : compilationUnit EOF
   ;

compilationUnit
   : using* typeDef*
   ;

using
   : usingPod
   | usingType
   | usingAs
   ;

usingPod
   : 'using' podSpec EOS
   ;

usingType
   : 'using' podSpec '::' ID EOS
   ;

usingAs
   : 'using' podSpec '::' ID 'as' ID EOS
   ;

podSpec
   : ID
   | str
   | ffiPodSpec
   ;

ffiPodSpec
   : '[' ID ']' ID ('.' ID)*
   ;

typeDef
   : classDef
   | mixinDef
   | facetDef
   | enumDef
   ;

classDef
   : typeHeader 'class' ID inheritance? '{' slotDefs '}'
   ;

mixinDef
   : typeHeader 'mixin' ID inheritance? '{' slotDefs '}'
   ;

facetDef
   : typeHeader 'facet' 'class' ID inheritance? '{' slotDefs '}'
   ;

enumDef
   : typeHeader 'enum' 'class' ID inheritance? '{' enumValDefs slotDefs '}'
   ;

typeHeader
   : facets typeFlags
   ;

typeFlags
   : typeFlag*
   ;

typeFlag
   : protection
   | 'abstract'
   | 'final'
   | 'const'
   | 'native'
   ;

protection
   : 'public'
   | 'protected'
   | 'private'
   | 'internal'
   ;

inheritance
   : ':' typeList
   ;

enumValDefs
   : enumValDef (',' enumValDef)* EOS
   ;

enumValDef
   : facets ID ('(' args ')')?
   ;

slotDefs
   : slotDef*
   ;

slotDef
   : fieldDef
   | methodDef
   | ctorDef
   | staticInit
   ;

fieldDef
   : facets fieldFlags type ID (':=' expr)? ('{' fieldGetter? fieldSetter? '}')? EOS
   ;

fieldFlags
   : fieldFlag*
   ;

fieldFlag
   : protection
   | 'abstract'
   | 'const'
   | 'final'
   | 'native'
   | 'override'
   | 'readonly'
   | 'static'
   | 'virtual'
   ;

fieldGetter
   : 'get' (EOS | block)
   ;

fieldSetter
   : protection? 'set' (EOS | block)
   ;

methodDef
   : facets methodFlags type ID '(' params ')' methodBody
   ;

methodFlags
   : methodFlag*
   ;

methodFlag
   : protection
   | 'abstract'
   | 'native'
   | 'once'
   | 'override'
   | 'static'
   | 'virtual'
   | 'final'
   ;

params
   : (param (',' param)*)?
   ;

param
   : type ID (':=' expr)?
   ;

methodBody
   : EOS
   | ('{' stmts '}')
   ;

ctorDef
   : facets ctorFlags 'new' ID '(' params ')' ctorChain? methodBody
   ;

ctorFlags
   : protection?
   ;

ctorChain
   : ':' (ctorChainThis | ctorChainSuper)
   ;

ctorChainThis
   : 'this' '.' ID '(' args ')'
   ;

ctorChainSuper
   : 'super' ('.' ID)? '(' args ')'
   ;

staticInit
   : 'static' '{' stmts '}'
   ;

facets
   : facet*
   ;

facet
   : '@' simpleType facetVals?
   ;

facetVals
   : '{' facetVal (EOS facetVal)* '}'
   ;

facetVal
   : ID '=' expr
   ;

block
   : stmt
   | ('{' stmts '}')
   ;

stmts
   : stmt*
   ;

stmt
   : break_
   | continue_
   | for_
   | if_
   | return_
   | switch_
   | throw_
   | while_
   | try_
   | exprStmt
   | localDef
   | itAdd
   ;

break_
   : 'break' EOS
   ;

continue_
   : 'continue' EOS
   ;

for_
   : 'for' '(' forInit? ';' expr? ';' expr? ')' block
   ;

forInit
   : expr
   | forLocalDef
   ;

forLocalDef
   : type? ID (':=' expr)?
   ;

if_
   : 'if' '(' expr ')' block ('else' block)?
   ;

return_
   : 'return' expr? EOS
   ;

throw_
   : 'throw' expr EOS
   ;

while_
   : 'while' '(' expr ')' block
   ;

exprStmt
   : expr EOS
   ;

localDef
   : type? ID (':=' expr)? EOS
   ;

itAdd
   : expr (',' expr)* ','? EOS
   ;

try_
   : 'try' block catch_* finally_?
   ;

catch_
   : 'catch' catchDef? block
   ;

catchDef
   : '(' type ID ')'
   ;

finally_
   : 'finally' block
   ;

switch_
   : 'switch' '(' expr ')' '{' case_* default_? '}'
   ;

case_
   : 'case' expr ':' stmts
   ;

default_
   : 'default' ':' stmts
   ;

expr
   : assignExpr
   ;

assignExpr
   : ifExpr (assignOp assignExpr)?
   ;

assignOp
   : '='
   | '*='
   | '/='
   | '%='
   | '+='
   | '-='
   ;

ifExpr
   : condOrExpr (ternarySuffix | elvisSuffix)?
   ;

ternarySuffix
   : '?' ifExprBody ':' ifExprBody
   ;

elvisSuffix
   : '?:' ifExprBody
   ;

ifExprBody
   : condOrExpr
   | ifExprThrow
   ;

ifExprThrow
   : 'throw' expr
   ;

condOrExpr
   : condAndExpr ('||' condAndExpr)*
   ;

condAndExpr
   : equalityExpr ('&&' equalityExpr)*
   ;

equalityExpr
   : relationalExpr (('==' | '!=' | '===' | '!==') relationalExpr)?
   ;

relationalExpr
   : rangeExpr (typeCheckSuffix | compareSuffix)?
   ;

typeCheckSuffix
   : ('is' | 'as' | 'isnot') type
   ;

compareSuffix
   : ('<' | '<=' | '>' | '>=' | '<=>') rangeExpr
   ;

rangeExpr
   : addExpr (('..' | '..<') addExpr)*
   ;

addExpr
   : multExpr (('+' | '-') multExpr)*
   ;

multExpr
   : parenExpr (('*' | '/' | '%') parenExpr)*
   ;

parenExpr
   : castExpr
   | unaryExpr
   | groupedExpr
   ;

castExpr
   : '(' type ')' parenExpr
   ;

groupedExpr
   : '(' expr ')' termChain*
   ;

unaryExpr
   : prefixExpr
   | postfixExpr
   | termExpr
   ;

prefixExpr
   : ('!' | '+' | '-' | '++' | '--') parenExpr
   ;

postfixExpr
   : termExpr ('++' | '--')
   ;

termExpr
   : termBase termChain*
   ;

termBase
   : literal
   | idExpr
   | typeBase
   ;

typeBase
   : typeLiteral
   | slotLiteral
   | namedSuper
   | staticCall
   | dsl
   | simple
   | ctorBlock
   | closure
   ;

typeLiteral
   : type '#'
   ;

slotLiteral
   : type? '#' ID
   ;

namedSuper
   : type '.' 'super'
   ;

staticCall
   : type '.' slotExpr
   ;

dsl
   : type DSL_BODY
   ;

simple
   : type '(' expr ')'
   ;

ctorBlock
   : type itBlock
   ;

termChain
   : dotCall
   | dynCall
   | safeDotCall
   | safeDynCall
   | indexExpr
   | callOp
   | itBlock
   ;

itBlock
   : '{' stmts '}'
   ;

dotCall
   : '.' slotExpr
   ;

dynCall
   : '->' slotExpr
   ;

safeDotCall
   : '?.' slotExpr
   ;

safeDynCall
   : '?->' slotExpr
   ;

idExpr
   : local
   | slotExpr
   ;

slotExpr
   : field
   | call
   ;

local
   : ID
   ;

field
   : '&' ID
   ;

call
   : ID ('(' args ')')? closure?
   ;

args
   : (expr (',' expr)*)?
   ;

indexExpr
   : '[' expr ']'
   ;

callOp
   : '(' args ')' closure?
   ;

literal
   : 'null'
   | 'this'
   | 'super'
   | 'it'
   | bool_
   | int_
   | float
   | decimal
   | str
   | duration
   | uri
   | list
   | map
   ;

bool_
   : 'true'
   | 'false'
   ;

int_
   : INT
   | CHAR
   ;

float
   : FLOAT
   ;

decimal
   : DECIMAL
   ;

str
   : STR
   ;

duration
   : DURATION
   ;

uri
   : URI
   ;

list
   : type? '[' listItems ']'
   ;

listItems
   : ','
   | (expr (',' expr)*) ','?
   ;

map
   : mapType? '[' mapItems ','? ']'
   ;

mapItems
   : ':'
   | (mapPair (',' mapPair)*)
   ;

mapPair
   : expr ':' expr
   ;

closure
   : closureSig '{' stmts '}'
   ;

closureSig
   : '|' closureParams ('->' type)? '|'
   ;

closureParams
   : (closureParam (',' closureParam)*)?
   ;

closureParam
   : formal
   ;

type
   : simpleType # simpleTypeRef
   | mapType # mapTypeRef
   | funcType # funcTypeRef
   | type '[]' # listTypeRef
   | type '?' # nullableTypeRef
   ;

typeList
   : type (',' type)*
   ;

simpleType
   : ID ('::' ID)?
   ;

mapType
   : '[' type ':' type ']'
   ;

funcType
   : '|' formals ('->' type)? '|'
   ;

formals
   : (formal (',' formal)*)?
   ;

formal
   : formalFull
   | formalInferred
   | formalTypeOnly
   ;

formalFull
   : type ID
   ;

formalInferred
   : ID
   ;

formalTypeOnly
   : type
   ;

ID
   : IDSTART (IDCHAR)*
   ;

fragment IDSTART
   : [A-Za-z_]
   ;

fragment IDCHAR
   : [A-Za-z_0-9]
   ;

fragment DIGIT
   : [0-9]
   ;

fragment HEXDIGIT
   : [0-9a-fA-F]
   ;

fragment DIGITS
   : DIGIT ('_'? DIGIT)*
   ;

fragment EXPONENT
   : [eE] [+-]? DIGITS
   ;

fragment ESC
   : '\\' (["'\\$`bfnrt] | 'u' HEXDIGIT HEXDIGIT HEXDIGIT HEXDIGIT | 'u{' HEXDIGIT+ '}')
   ;

INT
   : '0x' HEXDIGIT ('_'? HEXDIGIT)*
   | '0b' [01] ('_'? [01])*
   | DIGITS
   ;

CHAR
   : '\'' (ESC | ~ ['\\\r\n]) '\''
   ;

FLOAT
   : DIGITS ('.' DIGITS)? EXPONENT? [fF]
   ;

DECIMAL
   : DIGITS ('.' DIGITS)? EXPONENT? [dD]
   ;

DURATION
   : DIGITS ('.' DIGITS)? ('ns' | 'ms' | 'sec' | 'min' | 'hr' | 'day')
   ;

STR
   : '"""' .*? '"""'
   | '"' (ESC | ~ ["\\\r\n])* '"'
   ;

URI
   : '`' (ESC | ~ [`\\\r\n])* '`'
   ;

DSL_BODY
   : '<|' .*? '|>'
   ;

EOS
   : ';'
   ;

DOC_COMMENT
   : '**' ~ [\r\n]* -> skip
   ;

LINE_COMMENT
   : '//' ~ [\r\n]* -> skip
   ;

BLOCK_COMMENT
   : '/*' (BLOCK_COMMENT | .)*? '*/' -> skip
   ;

WS
   : [ \r\n\t]+ -> skip
   ;

