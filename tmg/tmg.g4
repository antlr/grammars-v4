/*
 BSD License
 
 Copyright (c) 2021, Tom Everett All rights reserved.
 
 Redistribution and use in source and binary forms, with or without modification, are permitted
 provided that the following conditions are met:
 
 1. Redistributions of source code must retain the above copyright notice, this list of conditions
 and the following disclaimer. 2. Redistributions in binary form must reproduce the above copyright
 notice, this list of conditions and the following disclaimer in the documentation and/or other
 materials provided with the distribution. 3. Neither the name of Tom Everett nor the names of its
 contributors may be used to endorse or promote products derived from this software without specific
 prior written permission.
 
 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
 IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
 FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
 CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
 IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
 OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
grammar tmg;

program
   : statement*
   ;

statement
   : label+ tail ';'
   ;

label
   : NAME ':'
   ;

tail
   : proc? prule (label prule)*
   | trule
   | CHARCL
   | NUMBER (';' NUMBER)*
   ;

proc
   : 'proc' '(' names? (';' names?)? ')'
   ;

prule
   : (disj ('|' disj)?)?
   ;

disj
   : pelem ('.' NAME?)?
   ;

pelem
   : pprime (('\\' | '/') pname)?
   | '(' prule ')'
   ;

pprime
   : pname '.'? ('(' parg ('.' parg)* ')')?
   | expr '?'?
   | LITERAL
   | '=' NAME
   | '=' trule
   ;

pname
   : NAME
   | '$' NUMBER
   ;

parg
   : pname
   | NUMBER
   | '(' prule ')'
   | LITERAL
   | CHARCL
   ;

expr
   : lv ASSIGN expr
   | rv
   ;

rv
   : (rv '?' rv ':')? primary (INFIX primary)?
   ;

primary
   : lv INCDEC?
   | INCDEC lv
   | lv
   | '(' expr ')'
   | UNARY primary
   | NUMBER
   ;

lv
   : pname
   | '*' primary
   | '(' lv ')'
   | lv expr*
   ;

trule
   : ('(' (NUMBER | names) ')')? tbody
   ;

tbody
   : '{' telem? '}'
   ;

telem
   : NAME
   | LITERAL
   | '$' NUMBER
   | bundleref ('(' targ ('.' targ)* ')')?
   ;

bundleref
   : (NAME | NUMBER) ('.' targ)*
   ;

targ
   : NAME
   | tbody
   ;

names
   : NAME (',' NAME)*
   ;

ASSIGN
   : '=' INFIX?
   ;

INCDEC
   : '++'
   | '--'
   ;

LITERAL
   : STAR
   | CHAR+
   ;

UNARY
   : '-'
   | '!'
   | '_'
   ;

INFIX
   : '+'
   | '-'
   | STAR
   | '/'
   | '%'
   | '&'
   | '^'
   | '<<'
   | '>>'
   | '=='
   | '!='
   | '<'
   | '>'
   | '<='
   | '>='
   ;

fragment STAR
   : '*'
   ;

CHARCL
   : '!'? ('<<' CHAR* '>>')+
   ;

NAME
   : [a-zA-Z] [a-zA-Z0-9]*
   ;

NUMBER
   : [0-7]+
   ;

fragment CHAR
   : [a-zA-Z0-9]
   ;

COMMENT
   : '/*' .*? '*/' -> skip
   ;

WS
   : [ \r\n\t]+ -> skip
   ;

