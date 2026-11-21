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

// Grammar for SASL ("St Andrews Static Language"), D A Turner, 1976.


//


// Based on the SASL Language Manual (D A Turner, revised November 1983),


// section IX "Summary of Syntax etc", together with the worked examples


// found throughout the manual (https://web.archive.org/web/20150402133901/


// http://www.eis.mdx.ac.uk/staffpages/dat/saslman.pdf) and


// https://en.wikipedia.org/wiki/SASL_(programming_language) .


//


// The manual typesets the "where", "arrow" (->) and "generator" (<-)


// symbols using special mathematical characters (->, <-) that were not


// available on an ordinary ASCII keyboard. Appendix III ("Running SASL


// under UNIX") gives the real, typed-in forms of the language: "->" for


// the conditional arrow, "<-" for the ZF-expression generator arrow, a


// single quote (') to open a string and a double quote (") to close it.


// Those ASCII forms -- not the typeset math symbols -- are what this


// grammar accepts.

grammar sasl;
// Every worked example in the manual ends its expr/def with a literal

// '?', which triggers evaluation ("Don't forget the question mark or

// nothing will happen" -- Appendix III). That '?' is therefore part of

// <program>, even though it is not among the 10 lexical delimiters

// (where = ( ) , -> ; <- { }) used inside expressions and definitions.

program
   : expr '?' EOF
   | 'def' defs '?' EOF
   ;

expr
   : expr 'where' defs
   | condexp
   ;

condexp
   : opexp '->' condexp ';' condexp
   | listexp
   ;

listexp
   : opexp (',' opexp)* ','?
   ;
   // Operators, in order of increasing binding power (SASL manual, section
   
   // IX): ':' '++' '--'  <  '..'  <  '...'  <  '|'  <  '&'  <  '~'  <
   
   // relational operators  <  infix '+' '-'  <  prefix '+' '-'  <
   
   // '*' '/' div rem  <  '**'  <  '#'. ANTLR4 gives earlier alternatives in
   
   // a left-recursive rule the higher (tighter) precedence, so the
   
   // alternatives below are listed from tightest to loosest, the reverse of
   
   // the manual's table.
   
opexp
   : '#' opexp
   | < assoc = right > opexp '**' opexp
   | opexp ('*' | '/' | 'div' | 'rem') opexp
   | ('+' | '-') opexp
   | opexp ('+' | '-') opexp
   | opexp ('>>' | '>' | '>=' | '=' | '~=' | '<=' | '<' | '<<') opexp
   | '~' opexp
   | opexp '&' opexp
   | opexp '|' opexp
   | opexp '...'
   | opexp '..' opexp
   | < assoc = right > opexp (':' | '++' | '--') opexp
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
   : struct (',' struct)* ','?
   ;
   // The manual's formal BNF only allows a struct to be a ':'-chain or a
   
   // single parenthesized formal, but every namelist example in the manual
   
   // (e.g. "x, y, z = 1, 2, 3") uses bare, unparenthesized names, so a bare
   
   // <formal> is accepted as a struct as well.
   
struct
   : formal ':' struct
   | '(' formal ')'
   | formal
   ;

formal
   : name
   | constant
   | '(' namelist ')'
   ;

constant
   : numeral
   | charconst
   | boolconst
   | '(' ')'
   | STRING
   ;

numeral
   : REAL scalefactor?
   ;

scalefactor
   : 'e' '-'? DIGIT+
   ;

boolconst
   : 'true'
   | 'false'
   ;

charconst
   : CHARCONST
   | 'sp'
   | 'nl'
   | 'np'
   | 'tab'
   ;

zfexpr
   : '{' expr ';' qualifiers '}'
   | '{' generator ';' qualifiers '}'
   ;

qualifiers
   : qualifier (';' qualifier)*
   ;

qualifier
   : generator
   | filter
   ;
   // i <- j <- k <- exp is shorthand for i <- exp; j <- exp; k <- exp
   
generator
   : name '<-' expr
   | name '<-' generator
   ;

filter
   : expr
   ;

name
   : NAME
   ;
   // <real> ::= <digit>+ | <digit>+ '.' <digit>+ per the manual's note that
   
   // "*" means "one or more", widened to also accept a leading '.' (as used
   
   // e.g. by ".001" in the printwidth definition in the SASL prelude).
   
REAL
   : DIGIT+ ('.' DIGIT+)?
   | '.' DIGIT+
   ;

CHARCONST
   : '%' .
   ;

STRING
   : '\'' .*? '"'
   ;

DIGIT
   : [0-9]
   ;

NAME
   : [a-zA-Z] [a-zA-Z0-9_]*
   ;

COMMENT
   : '||' ~ [\r\n]* -> skip
   ;

WS
   : [ \r\n\t]+ -> skip
   ;

