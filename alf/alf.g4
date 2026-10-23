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
grammar alf;

program
   : module+ goal*
   ;

goal
   : '?-' literalList '.'
   ;

module
   : 'module' modulename ('(' parameter (',' parameter)* ')')? '.' (moduleInterface | moduleDeclaration)* ('rules.' moduleBody)? 'end' modulename '.'
   ;

moduleInterface
   : ('export' symbolList '.')
   | ('use' useDeclaration (';' useDeclaration)* '.')
   ;

moduleDeclaration
   : ('datatype' datatypeDeclaration (';' datatypeDeclaration)* '.')
   | ('func' functionDeclaration (';' functionDeclaration)* '.')
   | ('pred' predicateDeclaration (';' predicateDeclaration)* '.')
   ;

useDeclaration
   : modulename ('(' name (',' name)* ')' ('=' modulename)?)? ('with' renamings)?
   ;

renamings
   : atom 'for' atom (',' atom 'for' atom)*
   ;

datatypeDeclaration
   : IDENT
   | (IDENT '=' '{' constructorDeclaration (';' constructorDeclaration)* '}')
   ;

constructorDeclaration
   : funcatom ('(' sortList ')')? (opType precedence_)?
   ;

functionDeclaration
   : funcatom ':' (sortList '->')? sort (opType precedence_)? ('partial')?
   ;

predicateDeclaration
   : predatom ':' sortList? predopType?
   ;

opType
   : 'prefix'
   | 'postfix'
   | 'infixnot'
   | 'infixleft'
   | 'infixright'
   ;

predopType
   : 'prefix'
   | 'postfix'
   | 'infix'
   ;

moduleBody
   : (functionRule | predicateRule)+
   ;

functionRule
   : functionterm ('=' term)? (':-' literalList)? ('onlyrewrite' | 'onlynarrow')? '.'
   ;

predicateRule
   : literal (':-' literalList)? '.'
   ;

literalList
   : equationliteral (',' equationliteral)*
   ;

equationliteral
   : literal
   | equation
   ;

literal
   : (predname '(' termList ')')
   | (term? predatom term?)
   ;

equation
   : term '=' term
   ;

termList
   : term (',' term)*
   ;

term
   : NUMBER
   | VARIABLE
   | DOUBLE_QUOTE_STRING
   | funcname '(' termList ')'
   | funcname
   | '(' term ')'
   | funcatom term
   | term funcatom term
   | term funcatom
   ;

functionterm
   : term
   ;

parameter
   : IDENT
   | ('(' 'func' functionDeclaration ')')
   | ('(' 'pred' predicateDeclaration ')')
   ;

name
   : (modulename '.')? atom
   ;

modulename
   : IDENT
   | QUOTED_MODULE_NAME
   ;

QUOTED_MODULE_NAME
   : ('\'' CHARACTER* '/' IDENT '\'')
   ;

predname
   : name
   ;

predatom
   : atom
   ;

funcname
   : name
   ;

funcatom
   : atom
   ;

sortList
   : sort (',' sort)*
   ;

sort
   : (modulename '.')? IDENT
   ;

symbolList
   : symbol (',' symbol)*
   ;

symbol
   : (funcatom | predatom) ('/' NUMBER)?
   ;

precedence_
   : NUMBER
   ;

atom
   : IDENT
   | SPECIALCHARACTERS
   | SINGLE_QUOTE_STRING
   | NUMBER
   ;

VARIABLE
   : (UPPERCASE | '_') (UPPERCASE | LOWERCASE | DIGIT | '_')*
   ;

IDENT
   : LOWERCASE (UPPERCASE | LOWERCASE | DIGIT | '_')*
   ;

DOUBLE_QUOTE_STRING
   : '"' CHARACTER* '"'
   ;

SINGLE_QUOTE_STRING
   : '\'' CHARACTER* '\''
   ;

NUMBER
   : DIGIT+
   ;

fragment CHARACTER
   : LOWERCASE
   | UPPERCASE
   | DIGIT
   ;

SPECIALCHARACTERS
   : SPECIALCHARACTER+
   ;

fragment SPECIALCHARACTER
   : '+'
   | '-'
   | '*'
   | '/'
   | '<'
   | '='
   | '>'
   | '\''
   | '\\'
   | ':'
   | '.'
   | '?'
   | '@'
   | '#'
   | '$'
   | '&'
   | '^'
   | '~'
   ;

fragment UPPERCASE
   : [A-Z]
   ;

fragment LOWERCASE
   : [a-z]
   ;

fragment DIGIT
   : [0-9]
   ;

WS
   : [ \r\n\t]+ -> skip
   ;

