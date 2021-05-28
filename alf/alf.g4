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
   : ('datatype' datatypeDeclaration (';' datatypeDeclaration) '.'*)
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
   : ident
   | (ident '=' 'f' constructorDeclaration (';' constructorDeclaration)* 'g')
   ;

constructorDeclaration
   : funcatom ('(' sortList ')')? (opType precedence)?
   ;

functionDeclaration
   : funcatom ':' (sortList '->')? sort (opType precedence)? ('partial')?
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
   : (functionRule | predicateRule)
   ;

functionRule
   : functionterm '=' term (':-' literalList)? ('onlyrewrite' | 'onlynarrow')? '.'
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
   : Number
   | variable
   | String
   | functionterm
   | '(' term ')'
   ;

functionterm
   : (funcname '(' termList ')')
 //  | (term? funcatom term?)
   | ('(' functionterm ')')
   ;

variable
   : Uppercase (Letter | Digit | '_')*
   | ('_' Letter | Digit | '_')*
   ;

parameter
   : ident
   | ('(' 'func' functionDeclaration ')')
   | ('(' 'pred' predicateDeclaration ')')
   ;

name
   : (modulename '.')? atom
   ;

modulename
   : ident
   | ('\'' pathname '/' ident '\'')
   ;

pathname
   : Character*
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
   : (modulename '.')? ident
   ;

symbolList
   : symbol (',' symbol)*
   ;

symbol
   : funcatom ('/' Number)?
   | predatom ('/' Number)?
   ;

precedence
   : Number
   ;

String
   : '"' Character* '"'
   ;

atom
   : ident
   | SpecialCharacters
   | '\'' Character* '\''
   ;

ident
   : Lowercase (Letter | Digit | '_')*
   ;

Character
   : [a-zA-Z0-9]
   ;

SpecialCharacters
   : SpecialCharacter+
   ;

SpecialCharacter
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

Number
   : Digit+
   ;

Digit
   : '0'
   | '1'
   | '2'
   | '3'
   | '4'
   | '5'
   | '6'
   | '7'
   | '8'
   | '9'
   ;

Letter
   : Uppercase
   | Lowercase
   ;

Uppercase
   : [A-Z]
   ;

Lowercase
   : [a-z]
   ;

WS
   : [ \r\n\t]+ -> skip
   ;

