// ******************************************************************************
// Copyright 2021 TypeFox GmbH
// This program and the accompanying materials are made available under the
// terms of the MIT License, which is available in the project root.
// *****************************************************************************


// $antlr-format alignColons trailing, alignLabels true, alignLexerCommands true, alignSemicolons ownLine, alignTrailers true
// $antlr-format alignTrailingComments true, allowShortBlocksOnASingleLine true, allowShortRulesOnASingleLine true, columnLimit 150
// $antlr-format maxEmptyLinesToKeep 1, minEmptyLines 0, reflowComments false, singleLineOverrulesHangingColon true, useTab false

lexer grammar LangiumLexer;

options {
    superClass = LangiumLexerBase;
}

// Insert here @header for C++ lexer.

EXCLAMATION_MARK     : '!';
AMPERSAND            : '&';
LEFT_PARENTHESIS     : '(';
RIGHT_PARENTHESIS    : ')';
ASTERISK             : '*';
COMMA                : ',';
FULL_STOP            : '.';
DOT_DOT              : '..';
COLON                : ':';
SEMICOLON            : ';';
QUESTION_MARK        : '?';
QM_BANG              : '?!';
QM_LT_BANG           : '?<!';
QM_LE                : '?<=';
QM_EQ                : '?=';
COMMERCIAL_AT        : '@';
LEFT_SQUARE_BRACKET  : '[';
RIGHT_SQUARE_BRACKET : ']';
LEFT_CURLY_BRACKET   : '{';
VERTICAL_LINE        : '|';
RIGHT_CURLY_BRACKET  : '}';
PLUS_SIGN            : '+';
PLUS_EQUAL           : '+=';
LESS_THAN_SIGN       : '<';
EQUALS_SIGN          : '=';
EQ_GT                : '=>';
GREATER_THAN_SIGN    : '>';
MINUS_GT             : '->';
KW_BIGINT            : 'bigint';
KW_BOOLEAN           : 'boolean';
KW_CURRENT           : 'current';
KW_DATE              : 'Date';
KW_ENTRY             : 'entry';
KW_EXTENDS           : 'extends';
KW_FALSE             : 'false';
KW_FRAGMENT          : 'fragment';
KW_GRAMMAR           : 'grammar';
KW_HIDDEN            : 'hidden';
KW_IMPORT            : 'import';
KW_INFER             : 'infer';
KW_INFERS            : 'infers';
KW_INTERFACE         : 'interface';
KW_NUMBER            : 'number';
KW_RETURNS           : 'returns';
KW_STRING            : 'string';
KW_TERMINAL          : 'terminal';
KW_TRUE              : 'true';
KW_TYPE              : 'type';
KW_WITH              : 'with';

ID: [_a-zA-Z][_a-zA-Z0-9]*;
// terminal STRING: /"(\\.|[^"\\])*"|'(\\.|[^'\\])*'/;
STRING: '"' ('\\' . | ~["\\])* '"' | '\'' ('\\' . | ~['\\])* '\'';
// terminal NUMBER returns number: /NaN|-?((\d*\.\d+|\d+)([Ee][+-]?\d+)?|Infinity)/;
NUMBER: 'NaN' | '-'? (( [\p{Nd}]* '.'+ | [\p{Nd}]+) ([Ee][+-]? [\p{Nd}]+)? | 'Infinity');
// terminal RegexLiteral returns string: /\/(?![*+?])(?:[^\r\n\[/\\]|\\.|\[(?:[^\r\n\]\\]|\\.)*\])+\/[a-z]*/;
RegexLiteral       : '/' { this.NoSlash() }? RE_regex '/';
fragment RE_regex  : RE_term ( '|' RE_term)*;
fragment RE_term   : RE_factor RE_factor*;
fragment RE_factor : RE_primary RE_quantifier?;
fragment RE_primary:
    RE_literal
    | RE_character_class
    | '.'
    | '(' RE_regex ')'
    | '(?=' RE_regex ')'
    | '(?!' RE_regex ')'
    | '(?:' RE_regex ')'
    | '(?<=' RE_regex ')'
    | '(?<!' RE_regex ')'
;
fragment RE_literal: RE_nonmeta_character | '\\' .; //RE_meta_character ;
fragment RE_nonmeta_character:
    ~(
        '.'
        | '^'
        | '$'
        | '*'
        | '+'
        | '?'
        | '('
        | ')'
        | '['
        | ']'
        | '{'
        | '}'
        | '|'
        | '\\'
        | '\n'
        | '\r'
    )
;
fragment RE_meta_character:
    '.'
    | '^'
    | '$'
    | '*'
    | '+'
    | '?'
    | '('
    | ')'
    | '['
    | ']'
    | '{'
    | '}'
    | '|'
    | '\\'
    | '/'
    | [dD]
    | [wW]
    | [sS]
    | 'n'
    | 'r'
    | 't'
    | '\''
    | '`'
;
fragment RE_character_class : '[' '^'? RE_character_range RE_character_range* ']';
fragment RE_character_range : RE_character ( '-' RE_character)?;
fragment RE_character       : ~[\n\r];
fragment RE_quantifier:
    '*'
    | '+'
    | '?'
    | '??'
    | '*?'
    | '+?'
    | '{' RE_number ( ',' RE_number?)? '}' '?'?
;
fragment RE_number: [0-9]+;

WS         : [ \n\r\t]+    -> channel(HIDDEN);
SL_COMMENT : '//' ~[\n\r]* -> channel(HIDDEN);
ML_COMMENT : '/*' .*? '*/' -> channel(HIDDEN);