/*
 [The "BSD licence"]
 Copyright (c) 2017 Sasa Coh, Michał Błotniak
 Copyright (c) 2019 Ivan Kochurkin, kvanttt@gmail.com, Positive Technologies
 Copyright (c) 2019 Dmitry Rassadin, flipparassa@gmail.com, Positive Technologies
 Copyright (c) 2021 Martin Mirchev, mirchevmartin2203@gmail.com
 All rights reserved.

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions
 are met:
 1. Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.
 2. Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.
 3. The name of the author may not be used to endorse or promote products
    derived from this software without specific prior written permission.

 THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

/*
 * A Go grammar for ANTLR 4 derived from the Go Language Specification
 * https://golang.org/ref/spec
 */

// $antlr-format alignTrailingComments true, columnLimit 150, maxEmptyLinesToKeep 1, reflowComments false, useTab false
// $antlr-format allowShortRulesOnASingleLine true, allowShortBlocksOnASingleLine true, minEmptyLines 0, alignSemicolons ownLine
// $antlr-format alignColons trailing, singleLineOverrulesHangingColon true, alignLexerCommands true, alignLabels true, alignTrailers true

lexer grammar GoLexer;

// Keywords

BREAK       : 'break' -> mode(NLSEMI);
DEFAULT     : 'default';
FUNC        : 'func';
INTERFACE   : 'interface';
SELECT      : 'select';
CASE        : 'case';
DEFER       : 'defer';
GO          : 'go';
MAP         : 'map';
STRUCT      : 'struct';
CHAN        : 'chan';
ELSE        : 'else';
GOTO        : 'goto';
PACKAGE     : 'package';
SWITCH      : 'switch';
CONST       : 'const';
FALLTHROUGH : 'fallthrough' -> mode(NLSEMI);
IF          : 'if';
RANGE       : 'range';
TYPE        : 'type';
CONTINUE    : 'continue' -> mode(NLSEMI);
FOR         : 'for';
IMPORT      : 'import';
RETURN      : 'return' -> mode(NLSEMI);
VAR         : 'var';

NIL_LIT: 'nil' -> mode(NLSEMI);

IDENTIFIER: LETTER (LETTER | UNICODE_DIGIT)* -> mode(NLSEMI);

// Punctuation

L_PAREN        : '(';
R_PAREN        : ')' -> mode(NLSEMI);
L_CURLY        : '{';
R_CURLY        : '}' -> mode(NLSEMI);
L_BRACKET      : '[';
R_BRACKET      : ']' -> mode(NLSEMI);
ASSIGN         : '=';
COMMA          : ',';
SEMI           : ';';
COLON          : ':';
DOT            : '.';
PLUS_PLUS      : '++' -> mode(NLSEMI);
MINUS_MINUS    : '--' -> mode(NLSEMI);
DECLARE_ASSIGN : ':=';
ELLIPSIS       : '...';

// Logical

LOGICAL_OR  : '||';
LOGICAL_AND : '&&';

// Relation operators

EQUALS            : '==';
NOT_EQUALS        : '!=';
LESS              : '<';
LESS_OR_EQUALS    : '<=';
GREATER           : '>';
GREATER_OR_EQUALS : '>=';

// Arithmetic operators

OR         : '|';
DIV        : '/';
MOD        : '%';
LSHIFT     : '<<';
RSHIFT     : '>>';
BIT_CLEAR  : '&^';
UNDERLYING : '~';

// Unary operators

EXCLAMATION: '!';

// Mixed operators

PLUS      : '+';
MINUS     : '-';
CARET     : '^';
STAR      : '*';
AMPERSAND : '&';
RECEIVE   : '<-';

// Number literals

DECIMAL_LIT : ('0' | [1-9] ('_'? [0-9])*)   -> mode(NLSEMI);
BINARY_LIT  : '0' [bB] ('_'? BIN_DIGIT)+    -> mode(NLSEMI);
OCTAL_LIT   : '0' [oO]? ('_'? OCTAL_DIGIT)+ -> mode(NLSEMI);
HEX_LIT     : '0' [xX] ('_'? HEX_DIGIT)+    -> mode(NLSEMI);

FLOAT_LIT: (DECIMAL_FLOAT_LIT | HEX_FLOAT_LIT) -> mode(NLSEMI);

DECIMAL_FLOAT_LIT: DECIMALS ('.' DECIMALS? EXPONENT? | EXPONENT) | '.' DECIMALS EXPONENT?;

HEX_FLOAT_LIT: '0' [xX] HEX_MANTISSA HEX_EXPONENT;

fragment HEX_MANTISSA:
    ('_'? HEX_DIGIT)+ ('.' ( '_'? HEX_DIGIT)*)?
    | '.' HEX_DIGIT ('_'? HEX_DIGIT)*
;

fragment HEX_EXPONENT: [pP] [+-]? DECIMALS;

IMAGINARY_LIT: (DECIMAL_LIT | BINARY_LIT | OCTAL_LIT | HEX_LIT | FLOAT_LIT) 'i' -> mode(NLSEMI);

// Rune literals

fragment RUNE: '\'' (UNICODE_VALUE | BYTE_VALUE) '\''; //: '\'' (~[\n\\] | ESCAPED_VALUE) '\'';

RUNE_LIT: RUNE -> mode(NLSEMI);

BYTE_VALUE: OCTAL_BYTE_VALUE | HEX_BYTE_VALUE;

OCTAL_BYTE_VALUE: '\\' OCTAL_DIGIT OCTAL_DIGIT OCTAL_DIGIT;

HEX_BYTE_VALUE: '\\' 'x' HEX_DIGIT HEX_DIGIT;

LITTLE_U_VALUE: '\\' 'u' HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT;

BIG_U_VALUE:
    '\\' 'U' HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT
;

// String literals

RAW_STRING_LIT         : '`' ~'`'* '`'                     -> mode(NLSEMI);
INTERPRETED_STRING_LIT : '"' (~["\\] | ESCAPED_VALUE)* '"' -> mode(NLSEMI);

// Hidden tokens

WS           : [ \t]+        -> channel(HIDDEN);
COMMENT      : '/*' .*? '*/' -> channel(HIDDEN);
TERMINATOR   : [\r\n]+       -> channel(HIDDEN);
LINE_COMMENT : '//' ~[\r\n]* -> channel(HIDDEN);

fragment UNICODE_VALUE: ~[\r\n'] | LITTLE_U_VALUE | BIG_U_VALUE | ESCAPED_VALUE;

// Fragments

fragment ESCAPED_VALUE:
    '\\' (
        'u' HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT
        | 'U' HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT
        | [abfnrtv\\'"]
        | OCTAL_DIGIT OCTAL_DIGIT OCTAL_DIGIT
        | 'x' HEX_DIGIT HEX_DIGIT
    )
;

fragment DECIMALS: [0-9] ('_'? [0-9])*;

fragment OCTAL_DIGIT: [0-7];

fragment HEX_DIGIT: [0-9a-fA-F];

fragment BIN_DIGIT: [01];

fragment EXPONENT: [eE] [+-]? DECIMALS;

fragment LETTER: UNICODE_LETTER | '_';

//[\p{Nd}] matches a digit zero through nine in any script except ideographic scripts
fragment UNICODE_DIGIT: [\p{Nd}];
//[\p{L}] matches any kind of letter from any language
fragment UNICODE_LETTER: [\p{L}];

mode NLSEMI;

// Treat whitespace as normal
WS_NLSEMI: [ \t]+ -> channel(HIDDEN);
// Ignore any comments that only span one line
COMMENT_NLSEMI      : '/*' ~[\r\n]*? '*/' -> channel(HIDDEN);
LINE_COMMENT_NLSEMI : '//' ~[\r\n]*       -> channel(HIDDEN);
// Emit an EOS token for any newlines, semicolon, multiline comments or the EOF and 
//return to normal lexing
EOS: ([\r\n]+ | ';' | '/*' .*? '*/' | EOF) -> mode(DEFAULT_MODE);
// Did not find an EOS, so go back to normal lexing
OTHER: -> mode(DEFAULT_MODE), channel(HIDDEN);