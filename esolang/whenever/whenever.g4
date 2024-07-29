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
   : line_* EOL* EOF
   ;

line_
   : linenumber_ when_* statement_list_ ';' EOL
   ;

when_
   : again_
   | defer_
   | forget_
   ;

defer_
   : 'defer' '(' expression_ ')'
   ;

again_
   : 'again' '(' expression_ ')'
   ;

forget_
   : 'forget' '(' expression_ ')'
   ;

linenumber_
   : term_
   ;

addremove_
   : termlist_ '#' termlist_
   ;

termlist_
   : term_ (',' term_)*
   ;

statement_list_
   : statement_ (',' statement_)*
   ;

statement_
   : addremove_? (print_statement_ | read_statement_ | decl_statement_)?
   ;

decl_statement_
   : term_ (',' term_)*
   ;

print_statement_
   : 'print' '(' term_ ')'
   ;

read_statement_
   : 'read' '(' ')'
   ;

expression_
   : term_ (COMPARE term_)*
   ;

term_
   : NOT? mult_term_
   ;

mult_term_
   : add_term_ (MULT_OP add_term_)*
   ;

add_term_
   : value_ (ADD_OP value_)*
   ;

value_
   : NUMBER
   | func_
   | QUOTED_STRING
   ;

func_
   : func_n_
   | func_u_
   ;

func_n_
   : 'N' '(' term_ ')'
   ;

func_u_
   : 'U' '(' term_ ')'
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

