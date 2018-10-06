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

grammar moo;

prog
   : programdecl statement + '.'
   ;

programdecl
   : '@program' programname ':' verbname
   ;

statement
   : ifblock
   | whileblock
   | doblock
   | assignblock
   | tryblock
   | '{' statement* '}'
   | command SEMICOLON
   ;

ifblock
   : 'if' condition statement + ('else' statement +)? 'endif'
   ;

whileblock
   : 'while' condition statement +
   ;

doblock
   : 'do' statement + 'while' condition
   ;

tryblock
   : 'try' statement + 'except' prop statement + 'endtry'
   ;

assignblock
   : prop ASSIGN expression SEMICOLON
   ;

condition
   : LPAREN expression relop expression RPAREN
   ;

relop
   : EQ
   | GT
   | GTE
   | LT
   | LTE
   ;

expressionlist
   : expression (COMMA expression)*
   ;

expression
   : term ((PLUS | MINUS) term)*
   ;

term
   : factor ((TIMES | DIV) factor)*
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
   | prop
   | number
   ;

command
   : prop ':' verb
   ;

verb
   : verbname ('(' expressionlist ')')?
   ;

prop
   : objname ('.' propertyname)*
   ;

stringliteral
   : STRINGLITERAL
   ;

objname
   : STRING
   ;

propertyname
   : STRING
   ;

number
   : NUMBER
   ;

programname
   : STRING
   ;

verbname
   : STRING
   ;


STRING
   : [a-zA-Z] [a-zA-Z0-9!] +
   ;


STRINGLITERAL
   : '"' ~ ["\r\n]* '"'
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


NUMBER
   : [0-9] +
   ;


COMMENT
   : ';' ~ [\r\n]* -> skip
   ;


WS
   : [ \r\n\t] -> skip
   ;
