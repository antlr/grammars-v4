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

grammar prolog;


// Prolog text and data formed from terms (6.2)

program: (directive | clause) * EOF ;

directive
    :   ':-'
    (   'dynamic' term
    |   'multifile' term
    |   'discontiguous' term
    |   'op' term term term
    |   'char_conversion' term term
    |   'initialization' term
    |   'include' term
    |   'ensure_loaded' term
    |   'set_prolog_flag' term term
    ) //TODO: in directive also other compound syntaxes possible (e.g. op(1150, fx, record). )
    '.'
    ;

clause: term '.' ;


// Abstract Syntax (6.3): terms formed from tokens

termlist
    : term ( ',' term )* //XXX: resolve different meanings of , and implement their precedence
    ;

term 
    : VARIABLE          # variable
    | '(' term ')'      # braced_term
    | '-'? integer      # integer_term
    | '-'? FLOAT        # float
    // structure / compound term
    | atom '(' termlist ')'     # compound_term
    | term operator term        # binary_operator
    | operator term             # unary_operator
    | '[' termlist ( '|' term )? ']' # list_term //TODO: priority <= 999 //TODO: find out what [|] list syntax means
    | '{' termlist '}'          # curly_bracketed_term
    | atom              # atom_term
    ;

operator //TODO: modifying operator table
    : ':-' | '-->'
    | '?-'
    | ';'
    | '->'
    | ','
    | '\\+'
    | '=' | '\\='
    | '==' | '\\==' | '@<' | '@=<' | '@>' | '@>='
    | '=..'
    | 'is' | '=:=' | '=\\=' | '<' | '=<' | '>' | '>='
    | '+' | '-' | '/\\' | '\\/'
    | '*' | '/' | '//' | 'rem' | 'mod' | '<<' | '>>'
    | '**'
    | '^'
    | '\\'
    ;

atom 
    : '[' ']'           # empty_list
    | '{' '}'           # empty_braces
    | SMALLATOM         # name
    | GRAPHIC_TOKEN     # graphic
    | QUOTED_TOKEN      # quoted
    | '\'' STRING '\''  # quoted_string //FIXME escapes in strings
    | '"' STRING '"'    # dq_string
    | '`' STRING '`'    # backq_string
    | ';'               # semicolon
    | '!'               # cut
    ;


integer
    : DECIMAL
    | BINARY
    | OCTAL
    | HEX
    ;


// Lexer (6.4 & 6.5): Tokens formed from Characters

SMALLATOM
    : LCLETTER CHARACTER*
    ;

VARIABLE
    : UCLETTER CHARACTER*
    | '_' CHARACTER+
    ;

DECIMAL: DIGIT+ ;
BINARY: [01]+ ;
OCTAL: [0-7]+ ;
HEX: [0-9a-fA-F]+ ;

FLOAT
    : DECIMAL '.' [0-9]+ ( [eE] [+-] DECIMAL )?
    ;

fragment CHARACTER
    : LCLETTER
    | UCLETTER
    | DIGIT
    | SPECIAL
    ;

GRAPHIC_TOKEN: (GRAPHIC | '\\')+ ;
fragment GRAPHIC: [#$&*+-./:<=>?@^~] ; // 6.5.1 graphic char

QUOTED_TOKEN: NON_QUOTE_CHAR | '\'\'' | '"' | '`' ;
fragment NON_QUOTE_CHAR // 6.4.2.1
    : GRAPHIC
    | LCLETTER | UCLETTER // == alphanumeric char 6.5.2
    // the others partly duplicate stuff, like allowing single space
    ;

fragment SPECIAL
    : '+' | '-' | '*' | '/' | '\\' | '^' | '~' | ':' | '.' | '?' | '#' | '$' | '&'
    ;

STRING
    : CHARACTER+
    ;

fragment LCLETTER
    : [a-z_];

fragment UCLETTER
    : [A-Z];

fragment DIGIT
    : [0-9];

WS
   : [ \t\r\n]+ -> skip
   ;

COMMENT: '%' ~[\n\r]* ( [\n\r] | EOF) -> channel(HIDDEN) ;
MULTILINE_COMMENT: '/*' ( MULTILINE_COMMENT | . )*? ('*/' | EOF) -> channel(HIDDEN);
