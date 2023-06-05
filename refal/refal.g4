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
grammar refal;

file_ : program EOF;

program
   : f_definition (';'? program)?
   | external_decl ';' program
   | program external_decl ';'
   ;

f_definition
   : '$ENTRY'? f_name '{' block_ '}'
   ;

external_decl
   : ('$EXTERNAL' | '$EXTERN' | '$EXTRN') f_name_list
   ;

f_name_list
   : f_name (',' f_name_list ';')?
   ;

f_name
   : identifier
   ;

block_
   : sentence (';' block_?)?
   ;

sentence
   : left_side conditions ('=' right_side | ',' block_ending)
   ;

left_side
   : pattern
   ;

conditions
   : (',' arg_ ':' pattern conditions)?
   ;

pattern
   : expression_
   ;

arg_
   : expression_
   ;

right_side
   : expression_
   ;

expression_
   : (term_ expression_)?
   ;

term_
   : symbol
   | variable
   | '<' expression_ '>'
   ;

block_ending
   : arg_ ':' '{' block_ '}'
   ;

symbol
   : identifier
   | DIGITS
   | STRING
   | STRING2
   | CHAR
   ;

identifier
   : IDENTIFER
   | STRING
   ;

variable
   : svar
   | tvar
   | evar
   ;

svar
   : 's' '.' index
   ;

tvar
   : 't' '.' index
   ;

evar
   : 'e' '.' index
   ;

index
   : identifier
   | DIGITS
   ;

DIGITS
   : [0-9]+
   ;

IDENTIFER
   : [a-zA-Z] [a-zA-Z0-9_-]*
   ;

STRING
   : '"' ~ '"'* '"'
   ;

STRING2
   : '\'' ~ '\''* '\''
   ;

CHAR
   : '\\\''
   | '\\"'
   | '\\\\'
   | '\\n'
   | '\\r'
   | '\\t'
   | '\\x' [0-9] [0-9]
   ;

LINE_COMMENT
   : '*' ~ [\r\n]* -> skip
   ;

BLOCK_COMMENT
   : '/*' .*? '*/' -> skip
   ;

WS
   : [ \r\n\t]+ -> skip
   ;

