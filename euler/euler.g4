/*
BSD License
Copyright (c) 2018, Tom Everett
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

// https://en.wikipedia.org/wiki/Euler_(programming_language)


// https://web.archive.org/web/20060901063907/http://www.iit.edu/~tc/euler.pdf


//     Thomas W. Christopher, "EULER: An Experiment in Language Definition",


//     section 4.1 "Parser" gives the LL(1) grammar this file is based on.

grammar euler;

program
   : block
   ;

block
   : 'begin' (vardecl ';' | labdecl ';')* stat (';' stat)* 'end'
   ;

vardecl
   : 'new' id
   ;

fordecl
   : 'formal' id
   ;

labdecl
   : 'label' id
   ;

stat
   : expr1
   | id ':' stat
   | var exprtail
   ;

expr
   : var exprtail
   | expr1
   ;

exprtail
   : '<-' expr
   | listN? factortail termtail sumtail choicetail relationtail conjtail disjtail catenatail
   ;

expr1
   : block
   | ifclause truepart expr
   | 'goto' primary
   | 'out' expr
   | primary1 factortail termtail sumtail choicetail relationtail conjtail disjtail catenatail
   | ('+' term | '-' term) sumtail choicetail relationtail conjtail disjtail catenatail
   | '~' relation conjtail disjtail catenatail
   ;

ifclause
   : 'if' expr 'then'
   ;

truepart
   : expr 'else'
   ;

catenatail
   : ('&' primary)*
   ;

disj
   : conj disjtail
   ;

disjtail
   : ('or' disj)?
   ;

conj
   : negation conjtail
   ;

conjtail
   : ('and' conj)?
   ;

negation
   : '~' relation
   | relation
   ;

relation
   : choice relationtail
   ;

relationtail
   : ('=' choice | '~=' choice | '<' choice | '<=' choice | '>' choice | '>=' choice)?
   ;

choice
   : sum choicetail
   ;

choicetail
   : ('min' sum | 'max' sum)*
   ;

sum
   : ('+' term | '-' term | term) sumtail
   ;

sumtail
   : ('+' term | '-' term)*
   ;

term
   : factor termtail
   ;

termtail
   : ('*' factor | '/' factor | 'div' factor | 'mod' factor)*
   ;

factor
   : primary factortail
   ;

factortail
   : ('**' primary)*
   ;

primary
   : var listN?
   | primary1
   ;

primary1
   : logval
   | number
   | symbol
   | reference
   | listN
   | 'tail' primary
   | procdef
   | 'undef'
   | '[' expr ']'
   | 'in'
   | 'isb' var
   | 'isn' var
   | 'isr' var
   | 'isl' var
   | 'isli' var
   | 'isy' var
   | 'isp' var
   | 'isu' var
   | 'abs' primary
   | 'length' var
   | 'integer' primary
   | 'real' primary
   | 'logical' primary
   | 'list' primary
   ;

procdef
   : prochead expr '\''
   ;

prochead
   : '\'' (fordecl ';')*
   ;

listN
   : '(' (')' | expr listTl)
   ;

listTl
   : ')'
   | ',' (expr listTl | ')')
   ;

reference
   : '@' var
   ;

var
   : id ('[' expr ']' | '.')*
   ;

logval
   : 'true'
   | 'false'
   ;

number
   : INTEGER
   | REAL
   ;

symbol
   : SYMBOL
   ;

id
   : IDENTIFIER
   ;

COMMENT
   : '(*' (COMMENT | .)*? '*)' -> skip
   ;

REAL
   : DIGIT+ '.' DIGIT+ ('E' '-'? DIGIT+)?
   ;

INTEGER
   : DIGIT+
   ;

SYMBOL
   : '"' ('""' | ~ '"')* '"'
   ;

IDENTIFIER
   : [a-zA-Z] [a-zA-Z0-9]*
   ;

fragment DIGIT
   : [0-9]
   ;

WS
   : (' ' | '\r' | '\t' | '\u000C' | '\n') -> channel (HIDDEN)
   ;

