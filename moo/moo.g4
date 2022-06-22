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
/*
* http://www2.iath.virginia.edu/courses/moo/ProgrammersManual.texinfo_4.html
*/
/*
* https://www.hayseed.net/MOO/manuals/ProgrammersManual.html
*/

grammar moo;

prog
   : declaration + EOF
   ;

declaration
   : programdecl
   | verbdecl
   | propertydecl
   | rmpropertydecl
   | setpropertydecl
   | displaypropertydecl
   | kidsdecl
   | parentdecl
   | describedecl
   | contentsdecl
   | noteditdecl
   | createdecl
   | editdecl
   | addaliasdecl
   ;

programdecl
   : '@program' programname ':' name statement + '.'
   ;

programname
   : name
   | stringliteral
   ;

verbdecl
   : '@verb' (verbname ':' name) name + permissions?
   ;

verbname
   : name
   | stringliteral
   ;

propertydecl
   : ('@property' | '@prop') property_ '='? expression? permissions?
   ;

rmpropertydecl
   : ('@rmproperty' | '@rmprop') name
   ;

setpropertydecl
   : '@set' property_ 'to' expression
   ;

displaypropertydecl
   : ('@display' | '@disp') property_
   ;

kidsdecl
   : '@kids' name
   ;

parentdecl
   : '@parent' name
   ;

describedecl
   : '@describe' property_ 'as' expression
   ;

contentsdecl
   : '@contents' name
   ;

noteditdecl
   : '@notedit' property_
   ;

createdecl
   : '@create' sysname 'called' expressionlist
   ;

editdecl
   : '@edit' property_
   ;

addaliasdecl
   : '@addalias' name (',' name)* 'to' expression
   ;

statement
   : ifblock
   | whileblock
   | doblock
   | forblock
   | assignblock
   | tryblock
   | command SEMICOLON
   ;

ifblock
   : 'if' condition statement + ('elseif' condition statement +)? ('else' statement +)? 'endif' ';'?
   ;

whileblock
   : 'while' condition statement +
   ;

doblock
   : 'do' statement + 'while' condition
   ;

forblock
   : 'for' name 'in' expression statement + 'endfor'
   ;

tryblock
   : 'try' statement + 'except' property_ statement + 'endtry'
   ;

assignblock
   : property_ ASSIGN expression SEMICOLON
   ;

condition
   : LPAREN expression (relop expression)* RPAREN
   ;

relop
   : EQ
   | NEQ
   | GT
   | GTE
   | LT
   | LTE
   | AND
   | OR
   ;

expressionlist
   : expression (COMMA expression)*
   ;

expression
   : term ((PLUS | MINUS) term)*
   ;

term
   : factor ((TIMES | DIV | MOD) factor)*
   ;

factor
   : signedAtom (POW signedAtom)*
   ;

signedAtom
   : PLUS signedAtom
   | MINUS signedAtom
   | atom
   ;

atom
   : stringliteral
   | functioninvocation
   | verbinvocation
   | property_
   | integer
   | real
   | list_
   | objref
   | '(' expression ')'
   | ('!' expression)
   ;

objref
   : OBJREF
   ;

functioninvocation
   : name '(' expressionlist ')'
   ;

command
   : verbinvocation
   | returncommand
   ;

returncommand
   : 'return' expression?
   ;

verbinvocation
   : property_ ':' verb
   ;

verb
   : name ('(' expressionlist? ')')?
   ;

property_
   : propertyname (('.' name) | '[' expression ']')*
   ;

propertyname
   : name
   | stringliteral
   ;

list_
   : '{' expressionlist? '}'
   ;

stringliteral
   : STRINGLITERAL
   ;

integer
   : INTEGER
   ;

real
   : REAL
   ;

name
   : username
   | sysname
   ;

sysname
   : DOLLAR STRING?
   ;

username
   : STRING
   ;

permissions
   : PERMISSIONS
   ;


LPAREN
   : '('
   ;


RPAREN
   : ')'
   ;


PLUS
   : '+'
   ;


MINUS
   : '-'
   ;


TIMES
   : '*'
   ;


MOD
   : '%'
   ;


DIV
   : '/'
   ;


GT
   : '>'
   ;


LT
   : '<'
   ;


GTE
   : '>='
   ;


LTE
   : '<='
   ;


EQ
   : '=='
   ;


AND
   : '&&'
   ;


OR
   : '||'
   ;


NEQ
   : '!='
   ;


POW
   : '^'
   ;


COMMA
   : ','
   ;


ASSIGN
   : '='
   ;


SEMICOLON
   : ';'
   ;


DOLLAR
   : '$'
   ;


OBJREF
   : '#' [0-9] +
   ;


PERMISSIONS
   : [rcxd] +
   ;


STRING
   : [a-zA-Z] [a-zA-Z0-9!_*] +
   ;


STRINGLITERAL
   : '"' ~ ["]* '"'
   ;


INTEGER
   : [0-9] +
   ;


REAL
   : [0-9] + '.' [0-9] +
   ;


COMMENT
   : ';' ~ [\r\n]* -> skip
   ;


WS
   : [ \r\n\t] -> skip
   ;
