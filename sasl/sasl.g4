/*
BSD License

Copyright (c) 2022, Tom Everett
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
grammar sasl;

program
   : expr?
   | 'def' defs?
   ;

expr
   : expr 'where' defs
   | condexp
   ;

condexp
   : opexp '→' condexp ';' condexp
   | listexp
   ;

listexp
   : opexp (',' opexp)*
   | opexp ','
   | opexp
   ;

opexp
   : prefix opexp
   | opexp infix opexp
   | comb
   ;

comb
   : comb simple
   | simple
   ;

simple
   : name
   | constant
   | '(' expr ')'
   | zfexpr
   ;

defs
   : clause ';' defs
   | clause
   ;

clause
   : namelist '=' expr
   | name rhs
   ;

rhs
   : formal rhs
   | formal '=' expr
   ;

namelist
   : struct (',' struct)*
   | struct ','
   | struct
   ;

struct
   : formal ':' struct
   | '(' formal ')'
   ;

formal
   : name
   | constant
   | (namelist)
   ;

constant
   : numeral
   | CHARCONST
   | BOOLCONST
   | '(' ')'
   | STRING
   ;

numeral
   : REAL scalefactor
   | '—' REAL scalefactor
   ;

scalefactor
   : 'e' DIGIT+
   | 'e' '-' DIGIT+
   ;

zfexpr
   : (expr ';' qualifiers)?
   | (generator ';' qualifiers)?
   ;

qualifiers
   : qualifier
   | qualifiers ';' qualifier
   ;

qualifier
   : generator
   | filter
   ;

generator
   : name '←' expr
   | name '←' generator
   ;

filter
   : expr
   ;

prefix
   : '~'
   | '+'
   | '-'
   | '#'
   ;

infix
   : ':'
   | '++'
   | '--'
   | '..'
   | '|'
   | '&'
   | '>>'
   | '>'
   | '>='
   | '='
   | '~='
   | '<='
   | '<'
   | '<<'
   | '+'
   | '-'
   | '*'
   | '/'
   | 'div'
   | 'rem'
   | '**'
   ;

postfix
   : '...'
   ;

name
   : 'abs'
   | 'all'
   | 'and'
   | 'append'
   | 'arctan'
   | 'code'
   | 'cons'
   | 'converse'
   | 'char'
   | 'cjustify'
   | 'concat'
   | 'cost'
   | 'count'
   | 'decode'
   | 'digit'
   | 'digitval'
   | 'drop'
   | 'e'
   | 'entier'
   | 'eq'
   | 'exp'
   | 'filter'
   | 'foldl'
   | 'foldr'
   | 'for'
   | 'from'
   | 'function'
   | 'hd'
   | 'I'
   | 'interleave'
   | 'intersection'
   | 'iterate'
   | 'K'
   | 'lay'
   | 'layn'
   | 'length'
   | 'letter'
   | 'list'
   | 'listdiff'
   | 'ljust'
   | 'log'
   | 'logical'
   | 'map'
   | 'member'
   | 'mkset'
   | 'not'
   | 'number'
   | 'or'
   | 'pi'
   | 'plus'
   | 'printwidth'
   | 'product'
   | 'reverse'
   | 'rjustify'
   | 'show'
   | 'sin'
   | 'some'
   | 'spaces'
   | 'sqrt'
   | 'sum'
   | 'take'
   | 'times'
   | 'tl'
   | 'union'
   | 'until'
   | 'while'
   | 'zip'
   ;

REAL
   : DIGIT*
   | DIGIT* '.' DIGIT*
   ;

BOOLCONST
   : 'true'
   | 'false'
   ;

CHARCONST
   : '%' [a-zA-Z0-9]
   | ' '
   | '\r'
   | '\n'
   | '\t'
   ;

STRING
   : '‘' .*? '’'
   ;

DIGIT
   : [0-9]
   ;

WS
   : [ \r\n\t]+ -> skip
   ;

