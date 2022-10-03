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
grammar orwell;

program
   : decl+ EOF
   ;

decl
   : syndecl
   | condecl
   | typedecl
   | opdecl
   | def_
   ;

syndecl
   : tylhs '==' type_
   ;

condecl
   : tylhs ':==' construct ('|' construct)*
   ;

typedecl
   : name (',' name)* '::' type_
   ;

name
   : var_ '(' (var_ prefix | infix) ')'
   ;

tylhs
   : (tyvar infix tyvar)
   | (prefix tyvar)
   | tylhs1
   ;

tylhs1
   : tylhsprimary tyvar*
   ;

tylhsprimary
   : tyname '(' (tylhs | tylhssection) ')'
   ;

tylhssection
   : prefix
   | infix
   | (infix tyvar)
   | (tyvar infix)
   ;

type_
   : tyterm1 (infix type_)?
   ;

tyterm1
   : prefix tyterm1
   | tyterm2
   ;

tyterm2
   : typrimary
   | typrimaryname typrimary*
   ;

typrimaryname
   : tyname
   | '(' (type_ | tysection) ')'
   ;

typrimary
   : typrimaryname
   | tyvar tytuple
   | tylist
   ;

tysection
   : prefix infix
   | (infix tyterm1)
   | (tyterm1 infix)
   ;

tylist
   : '[' type_ ']'
   ;

tytuple
   : '(' type_ (',' type_)* ')'
   ;

construct
   : (con typrimary*)
   | (typrimary infix typrimary)
   | (prefix typrimary)
   ;

opdecl
   : opkind OP+
   ;

opkind
   : (assoc DIGIT)
   | '%prefix'
   | '%prefixcon'
   ;

assoc
   : '%left'
   | '%right'
   | '%non'
   | '%leftcon'
   | '%rightcon'
   | '%noncon'
   ;

def_
   : pat '=' rhs ('%else'? pat '=' rhs)*
   ;

rhs
   : (term | conditional) wherepart?
   ;

conditional
   : ifpart ('=' ifpart)* ('=' otherpart)?
   ;

ifpart
   : term ',' 'if' term
   ;

otherpart
   : term ',' 'otherwise'
   ;

wherepart
   : 'where' def_+
   ;

pat
   : pat1 (infix pat)?
   ;

pat1
   : prefix pat1
   | pat2
   ;

pat2
   : patprimary
   | (patprimaryname patprimary*)
   ;

patprimaryname
   : var_
   | ('(' (pat | patsection) ')')
   ;

patprimary
   : patprimaryname
   | literal
   | pattuple
   | patlist
   ;

patsection
   : prefix
   | infix
   | (infix pat1)
   | (pat1 infix)
   ;

pattuple
   : '(' pat ',' pat (',' pat)* ')'
   ;

patlist
   : '[' (pat (',' pat)*)? ']'
   ;

term
   : term1 (infix term)?
   ;

term1
   : (prefix term1)
   | term2
   ;

term2
   : primary
   | (primaryname primary*)
   ;

primaryname
   : var_
   | '(' (term | section) ')'
   ;

primary
   : primaryname
   | fliteral
   | tuple_
   | list_
   ;

section
   : prefix
   | infix
   | (infix term1)
   | (term1 infix)
   ;

list_
   : listform
   | upto
   | comp
   ;

tuple_
   : '(' term ',' term (',' term)* ')'
   ;

listform
   : '[' (term (',' term)*)? ']'
   ;

upto
   : '[' term (',' term)? '..' term? ']'
   ;

comp
   : '[' term '|' (qualifier (';' qualifier)*)? ']'
   ;

qualifier
   : term
   | pat '<-' term
   ;

fliteral
   : FLOAT
   | literal
   ;

literal
   : INTEGER
   | CHAR
   | STRING
   ;

infix
   : OP
   ;

prefix
   : OP
   ;

tyname
   : ID
   ;

tyvar
   : ID
   ;

con
   : ID
   ;

var_
   : ID
   ;

INTEGER
   : DIGIT+
   ;

FLOAT
   : INTEGER '.' INTEGER ('e' '—'? INTEGER)?
   ;

STRING
   : '"' ~ '"'* '"'
   ;

ESCCHAR
   : '\\' (CHAR | DIGIT (DIGIT DIGIT?)?)
   ;

PRAGMA
   : '%' ID
   ;

OP
   : SYMBOL+
   | ('$' ID)
   ;

ID
   : LETTER (LETTER | SYMBOL | DIGIT | '\'' | '_')*
   ;

fragment SYMBOL
   : '+'
   | '-'
   | '*'
   | '='
   | '<'
   | '>'
   | '~'
   | '&'
   | '\\'
   | '/'
   | '^'
   | ':'
   | '#'
   | '!'
   | '.'
   | ';'
   | '|'
   | '?'
   | '@'
   | '`'
   | '$'
   | '%'
   | '{'
   | '}'
   ;

fragment LETTER
   : [a-zA-Z]
   ;

DIGIT
   : '0' .. '9'
   ;

CHAR
   : [0-9a-zA-Z]
   ;

WS
   : [ \r\n\t]+ -> skip
   ;

