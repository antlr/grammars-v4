    /*
BSD License

Copyright (c) 2013, Tom Everett
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

grammar mumps;

program
    : routine+
    ;

routine
    : routinename routinebody
    ;

routinename
    : PERCENT? identifier LPAREN paramlist? RPAREN (SPACE+ comment)? CR
    ;

paramlist
    : param (COMMA param)*
    ;

param
    : identifier
    ;

routinebody
    : line+
    ;

line
    : SPACE+ (label SPACE)? command* (SPACE+ comment)? CR
    ;

label
    : identifier
    ;

command
    : commandname (SPACE arglist)?
    ;

commandname
    : identifier
    ;

arglist
    : arg (COMMA arg)*
    ;

arg
    : identifier
    | (NUMBER
    | BANG
    | STRING_LITERAL)
    | variable
    ;

comment
    : SEMICOLON ~(CR)*
    ;

identifier
    : IDENTIFIER
    ;

variable
    : DOLLAR identifier
    ;

SEMICOLON
    : ';'
    ;

COMMA
    : ','
    ;

DOLLAR
    : '$'
    ;

PERCENT
    : '%'
    ;

CARAT
    : '^'
    ;

BANG
    : '!'
    ;


LPAREN
    : '('
    ;

RPAREN
    : ')'
    ;

ADD
    : '+'
    ;

SUBTRACT
    : '-'
    ;

MULTIPLY
    : '*'
    ;

DIVIDE
    : '/'
    ;

IDENTIFIER
    : ('a'..'z' | 'A'..'Z')  ('a'..'z'|'A'..'Z'|'0'..'9')*
    ;

STRING_LITERAL
    : '\"' ('\"\"' | ~('\"'))* '\"'
    ;

NUMBER
    : ('0'..'9')+ ('.' ('0'..'9')+)?
    ;

SPACE
    : ' '
    ;

CR
    : '\n'
    ;

WS
    : [\t]->skip
    ;
