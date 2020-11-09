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

/*
* https://people.mpi-sws.org/~rossberg/sml.html
*/
grammar standardml;

longid
   : id ('.' id)*
   ;

lab
   : id
   | IDENTIFIER
   ;

exp
   : con
   | 'op'? longid
   | (exp exp)
   | (exp id exp)
   | '(' exp ')'
   | '(' exp (',' exp)* ')'
   | '{' exprow? '}'
   | '#' lab
   | '[' exp (',' exp)* ']'
   | '(' exp (';' exp)* ')'
   | 'let' dec 'in' exp (';' exp)* 'end'
   | exp ':' typ*
   | 'raise' exp
   | exp 'handle' match
   | exp 'andalso' exp
   | exp 'orelse' exp
   | 'if' exp 'then' exp 'else' exp
   | 'while' exp 'do' exp
   | 'case' exp 'of' match
   | 'fn' match
   ;

exprow
   : lab '=' exp (',' exprow)?
   ;

match
   : pat '=>' exp ('|' match)?
   ;

pat
   : con
   | '_'
   | 'op'? id
   | 'op'? longid pat?
   | pat id pat
   | '(' pat ')'
   | '(' pat (',' pat)* ')'
   | '{' patrow? '}'
   | '[' pat (',' pat)* ']'
   | pat ':' typ*
   | 'op'? id (':' typ*)? 'as' pat
   ;

patrow
   : '...'
   | lab '=' pat (',' patrow)*
   | id (':' typ*)? ('as' pat)? (',' patrow)?
   ;

typ
   : var
   //   | typ? longid
   | '(' typ* ')'
   | typ '->' typ
   | '{' typrow? '}'
   ;

typrow
   : lab ':' typ* (',' typrow)?
   ;

dec
   : 'val' var? valbind
   | 'fun' var? funbind
   | 'type' typbind
   | 'datatype' datbind ('withtype' typbind)?
   | 'datatype' id '=' 'datatype' longid
   | 'abstype' datbind ('withtype' typbind)? 'with' dec 'end'
   | 'exception' exnbind
   | 'structure' strbind
   | dec ';'? dec
   | 'local' dec 'in' dec 'end'
   | 'open' longid+
   | 'nonfix' id+
   | 'infix' DIGIT? id+
   | 'infixr' DIGIT? id+
   ;

valbind
   : pat '=' exp ('and' valbind)?
   | 'rec' valbind
   ;

funbind
   : funmatch ('and' funbind)?
   ;

funmatch
   : 'op'? id pat+ (':' typ)? '=' exp ('|' funmatch)?
   | pat id pat (':' typ)? '=' exp ('|' funmatch)?
   | '(' pat id pat ')' pat+ (':' typ)? '=' exp ('|' funmatch)?
   ;

typbind
   : var? id '=' typ* ('and' typbind)?
   ;

datbind
   : var? id '=' conbind ('and' datbind)?
   ;

conbind
   : id ('of' typ) ('|' conbind)
   ;

exnbind
   : id ('of' typ) ('and' exnbind)
   | id '=' longid ('and' exnbind)
   ;

str
   : longid
   | 'struct' dec 'end'
   | str ':' sig
   | str ':>' sig
   | id '(' str ')'
   | id '(' dec ')'
   | 'let' dec 'in' str 'end'
   ;

strbind
   : id (':' '>'? sig)? '=' str ('and' strbind)?
   ;

sig
   : id
   | 'sig' spec 'end'
   | sig 'where' 'type' typrefin
   ;

typrefin
   : var? longid '=' typ* ('and' 'type' typrefin)?
   ;

spec
   : 'val' valdesc
   | 'type' typdesc
   | 'eqtype' typdesc
   | 'type' typbind
   | 'datatype' datdesc
   | 'datatype' id '=' 'datatype' longid
   | 'exception' exndesc
   | 'structure' strdesc spec ';'? spec
   | 'include' sig exp
   | 'include' id+
   | spec 'sharing' 'type' longid ('=' longid)*
   | spec 'sharing' longid ('=' longid)*
   ;

valdesc
   : id ':' typ* ('and' valdesc)?
   ;

typdesc
   : var? id ('and' typdesc)?
   ;

datdesc
   : var? id '=' condesc ('and' datdesc)?
   ;

condesc
   : id ('of' typ)? ('|' condesc)?
   ;

exndesc
   : id ('of' typ)? ('and' exndesc)?
   ;

strdesc
   : id ':' sig ('and' strdesc)?
   ;

progs
   : prog (';'? prog)*
   ;

prog
   : (dec | 'functor' fctbind | 'signature' sigbind) EOF?
   ;

fctbind
   : id '(' id ':' sig ')' (':' '>'? sig)? '=' str ('and' fctbind)?
   | id '(' spec ')' (':' '>'? sig)? '=' str ('and' fctbind)?
   ;

sigbind
   : id '=' sig ('and' sigbind)?
   ;

con
   : INT
   | WORD
   | FLOAT
   | CHAR
   | STRING
   ;

id
   : IDENTIFIER
   ;

var
   : IDENTIFIER
   ;

IDENTIFIER
   : (LETTER (LETTER | DIGIT | '_')*)
   | (('!' | '%' | '&' | '$' | '#' | '+' | '-' | '/' | ':' | '<' | '=' | '>' | '?' | '@' | '\\' | '~' | '`' | '^' | '|' | '*')+)
   ;

INT
   : ('~'? NUM)
   | ('~'? '0x' HEX)
   ;

WORD
   : ('0w' NUM)
   | ('0w' HEX)
   ;

FLOAT
   : ('~'? NUM ' .' NUM)
   | ('~'? NUM ('.' NUM)? 'e' '~'? NUM)
   ;

CHAR
   : '#' ASCII
   ;

STRING
   : '"' ASCII* '"'
   ;

DIGIT
   : [0-9]
   ;

//VAR
//   : (LETTER | DIGIT | '_')+
//   ;

fragment NUM
   : DIGIT+
   ;

fragment HEX
   : (DIGIT | LETTER)+
   ;

fragment ASCII
   : '\u0021' .. '\u007E'
   ;

fragment LETTER
   : [a-zA-Z]
   ;

COMMENT
   : '(*' .*? '*)' -> skip
   ;

WS
   : [ \r\n\t]+ -> skip
   ;

