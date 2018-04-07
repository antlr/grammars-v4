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

p_text: (directive | clause) * EOF ;

directive: ':-' term '.' ; // also 3.58

clause: term '.' ; // also 3.33


// Abstract Syntax (6.3): terms formed from tokens

termlist
    : term ( ',' term )*
    ;

term 
    : VARIABLE          # variable
    | '(' term ')'      # braced_term
    | '-'? integer      # integer_term //TODO: negative case should be covered by unary_operator
    | '-'? FLOAT        # float
    // structure / compound term
    | atom '(' termlist ')'     # compound_term
    |<assoc=right> term operator term        # binary_operator
    | operator term             # unary_operator
    | '[' termlist ( '|' term )? ']' # list_term
    | '{' termlist '}'          # curly_bracketed_term

    | atom              # atom_term
    ;

//TODO: operator priority, associativity, arity. Filter valid priority ranges for e.g. [list] syntax
//TODO: modifying operator table

operator
    : ':-' | '-->'
    | '?-'
    | 'dynamic' | 'multifile' | 'discontiguous' | 'public' //TODO: move operators used in directives to "built-in" definition of dialect
    | ';'
    | '->'
    | ','
    | '\\+'
    | '=' | '\\='
    | '==' | '\\==' | '@<' | '@=<' | '@>' | '@>='
    | '=..'
    | 'is' | '=:=' | '=\\=' | '<' | '=<' | '>' | '>='
    | ':' // modules: 5.2.1
    | '+' | '-' | '/\\' | '\\/'
    | '*' | '/' | '//' | 'rem' | 'mod' | '<<' | '>>' //TODO: '/' cannot be used as atom because token here not in GRAPHIC. only works because , is operator too. example: swipl/filesex.pl:177
    | '**'
    | '^'
    | '\\'
    ;

atom // 6.4.2 and 6.1.2
    : '[' ']'           # empty_list //NOTE [] is not atom anymore in swipl 7 and later
    | '{' '}'           # empty_braces
    | LETTER_DIGIT      # name
    | GRAPHIC_TOKEN     # graphic
    | QUOTED            # quoted_string
    | DOUBLE_QUOTED_LIST# dq_string
    | BACK_QUOTED_STRING# backq_string
    | ';'               # semicolon
    | '!'               # cut
    ;


integer // 6.4.4
    : DECIMAL
    | CHARACTER_CODE_CONSTANT
    | BINARY
    | OCTAL
    | HEX
    ;


// Lexer (6.4 & 6.5): Tokens formed from Characters

LETTER_DIGIT // 6.4.2
    : SMALL_LETTER ALPHANUMERIC*
    ;

VARIABLE // 6.4.3
    : CAPITAL_LETTER ALPHANUMERIC*
    | '_' ALPHANUMERIC+
    | '_'
    ;

// 6.4.4
DECIMAL: DIGIT+ ;
BINARY: '0b' [01]+ ;
OCTAL: '0o' [0-7]+ ;
HEX: '0x' HEX_DIGIT+ ;

CHARACTER_CODE_CONSTANT: '0' '\'' SINGLE_QUOTED_CHARACTER ;

FLOAT: DECIMAL '.' [0-9]+ ( [eE] [+-] DECIMAL )? ;


GRAPHIC_TOKEN: (GRAPHIC | '\\')+ ; // 6.4.2
fragment GRAPHIC: [#$&*+./:<=>?@^~] | '-' ; // 6.5.1 graphic char

// 6.4.2.1
fragment SINGLE_QUOTED_CHARACTER: NON_QUOTE_CHAR | '\'\'' | '"' | '`' ;
fragment DOUBLE_QUOTED_CHARACTER: NON_QUOTE_CHAR | '\'' | '""' | '`' ;
fragment BACK_QUOTED_CHARACTER: NON_QUOTE_CHAR | '\'' | '"' | '``' ;
fragment NON_QUOTE_CHAR
    : GRAPHIC
    | ALPHANUMERIC
    | SOLO
    | ' ' // space char
    | META_ESCAPE
    | CONTROL_ESCAPE
    | OCTAL_ESCAPE
    | HEX_ESCAPE
    ;
fragment META_ESCAPE: '\\' [\\'"`] ; // meta char
fragment CONTROL_ESCAPE: '\\' [abrftnv] ;
fragment OCTAL_ESCAPE: '\\' [0-7]+ '\\' ;
fragment HEX_ESCAPE: '\\x' HEX_DIGIT+ '\\' ;

QUOTED:          '\'' (CONTINUATION_ESCAPE | SINGLE_QUOTED_CHARACTER )*? '\'' ; // 6.4.2
DOUBLE_QUOTED_LIST: '"' (CONTINUATION_ESCAPE | DOUBLE_QUOTED_CHARACTER )*? '"'; // 6.4.6
BACK_QUOTED_STRING: '`' (CONTINUATION_ESCAPE | BACK_QUOTED_CHARACTER )*? '`'; // 6.4.7
fragment CONTINUATION_ESCAPE: '\\\n' ;

// 6.5.2
fragment ALPHANUMERIC: ALPHA | DIGIT ;
fragment ALPHA: '_' | SMALL_LETTER | CAPITAL_LETTER ;
fragment SMALL_LETTER: [a-z_];

fragment CAPITAL_LETTER: [A-Z];

fragment DIGIT: [0-9] ;
fragment HEX_DIGIT: [0-9a-fA-F] ;

// 6.5.3
fragment SOLO: [!(),;[{}|%] | ']' ;


WS
   : [ \t\r\n]+ -> skip
   ;

COMMENT: '%' ~[\n\r]* ( [\n\r] | EOF) -> channel(HIDDEN) ;
MULTILINE_COMMENT: '/*' ( MULTILINE_COMMENT | . )*? ('*/' | EOF) -> channel(HIDDEN);
