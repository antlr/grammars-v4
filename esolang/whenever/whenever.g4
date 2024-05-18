/*
BSD License

Copyright (c) 2024 Tom Everett
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

// $antlr-format alignTrailingComments true, columnLimit 150, minEmptyLines 1, maxEmptyLinesToKeep 1, reflowComments false, useTab false


// $antlr-format allowShortRulesOnASingleLine false, allowShortBlocksOnASingleLine true, alignSemicolons hanging, alignColons hanging

grammar whenever;

program_
   : line* EOL* EOF
   ;

line
   : linenumber ugh* statement_list ';' EOL
   ;

ugh
   : again
   | defer
   | forget
   ;

defer
   : 'defer' '(' expression ')'
   ;

again
   : 'again' '(' expression ')'
   ;

forget
   : 'forget' '(' expression ')'
   ;

linenumber
   : term
   ;

addremove
   : termlist '#' termlist
   ;

termlist
   : term (',' term)*
   ;

statement_list
   : statement (',' statement)*
   ;

statement
   : addremove? (print_statement | read_statement | decl_statement)?
   ;

decl_statement
   : term (',' term)*
   ;

print_statement
   : 'print' '(' term ')'
   ;

read_statement
   : 'read' '(' ')'
   ;

expression
   : term (COMPARE term)*
   ;

term
   : NOT? mult_term
   ;

mult_term
   : add_term (MULT_OP add_term)*
   ;

add_term
   : value (ADD_OP value)*
   ;

value
   : NUMBER
   | func
   | QUOTED_STRING
   ;

func
   : func_n
   | func_u
   ;

func_n
   : 'N' '(' term ')'
   ;

func_u
   : 'U' '(' term ')'
   ;

COMPARE
   : '<'
   | '>'
   | '>='
   | '<='
   | '=='
   | '||'
   | '&&'
   ;

NOT
   : '!'
   ;

MULT_OP
   : '*'
   | '/'
   ;

ADD_OP
   : '+'
   | '-'
   ;

QUOTED_STRING
   : '"' ~ '"'* '"'
   ;

NUMBER
   : '-'? [0-9]+
   ;

EOL
   : [\r\n]
   ;

WHITESPACE
   : [ \t]+ -> skip
   ;

